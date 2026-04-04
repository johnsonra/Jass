# app.R – Cross Jass interactive Shiny app
# Launched via Jass::play_jass_app()

library(shiny)
library(shinyjs)
library(Jass)

# Serve card PNGs at global scope (must be before ui/server definitions)
local({
  pkg_data <- system.file("data", package = "Jass")
  if (nzchar(pkg_data)) shiny::addResourcePath("cards", pkg_data)
})

# ── Null-coalescing helper ───────────────────────────────────────────────────
`%||%` <- function(x, y) if (!is.null(x) && length(x) > 0 && !is.na(x[1]) && nzchar(x[1])) x else y

# ── Card image helpers ──────────────────────────────────────────────────────

# Map card abbreviation (L/F/S/C prefix) to PNG filename (B/F/S/A prefix)
card_png <- function(abbr) {
  prefix <- c(L = "B", F = "F", S = "S", C = "A")
  paste0(prefix[substr(abbr, 1, 1)], substr(abbr, 2, 2), ".png")
}

suit_icon <- function(suit) {
  c(Bells = "\U0001F514", Flowers = "\U0001F33B",
    Shields = "\U0001F6E1\uFE0F", Acorns = "\U0001F330")[suit]
}

# Human-readable card label: "Under 🌰", "9 🔔", etc.
card_label <- function(abbr) {
  suit_name <- suitTranslation(substr(abbr, 1, 1))
  face_name <- faceTranslation(substr(abbr, 2, 2))
  paste0(face_name, "\u00A0", suit_icon(suit_name))
}

# A clickable or static card slot (image + text label)
card_div <- function(abbr, clickable = TRUE, disabled = FALSE) {
  img <- tags$img(src = card_png(abbr), class = "card-img", alt = abbr)
  lbl <- tags$div(class = "card-label", card_label(abbr))
  cls <- paste("card-slot", if (disabled) "card-disabled" else if (clickable) "card-playable" else "")
  if (clickable && !disabled) {
    tags$div(class = cls, img, lbl,
             onclick = sprintf("Shiny.setInputValue('card_click','%s',{priority:'event'})", abbr))
  } else {
    tags$div(class = cls, img, lbl)
  }
}

card_back_div <- function() tags$div(class = "card-back", "\U0001F0A0")
card_empty_div <- function() tags$div(class = "card-empty")

# ── Scoring helpers ─────────────────────────────────────────────────────────

compute_round_trick_pts <- function(g) {
  pts <- vapply(seq_along(g@round@won), function(i) {
    df <- cards(g@round@won[[i]])
    if (nrow(df) == 0L) return(0L)
    as.integer(sum(card_value(df$face, df$trump)))
  }, integer(1))
  names(pts) <- names(g@score)
  # +5 last-trick bonus
  winner_team <- g@teams[g@round@next_player]
  pts[winner_team] <- pts[winner_team] + 5L
  pts
}

is_round_over <- function(g) length(g@round@history) == 9L

# ── UI ───────────────────────────────────────────────────────────────────────

SUITS <- c("Bells", "Flowers", "Shields", "Acorns")

ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$link(rel = "stylesheet", href = "jass.css")),

  # Setup panel ---------------------------------------------------------------
  conditionalPanel("output.phase === 'setup'",
    div(class = "setup-panel",
      h2("♟ Cross Jass"),
      p(style = "text-align:center; color:#ccc;",
        "Configure players, then deal to begin."),

      # Player rows
      lapply(1:4, function(p) {
        seat <- c("South (You)", "East", "North", "West")[p]
        div(class = "player-row",
          fluidRow(
            column(3, textInput(paste0("pname_", p),
                                label = paste0("Player ", p, " (", seat, ")"),
                                value = paste0("Player ", p))),
            column(3, selectInput(paste0("ptype_", p), "Type",
                                  choices = c("Human", "AI"), selected = if (p == 1) "Human" else "AI")),
            column(3, conditionalPanel(
              sprintf("input.ptype_%d === 'AI'", p),
              selectInput(paste0("pstrat_", p), "Card strategy",
                          choices = c("Random" = "random", "Gemini" = "gemini"),
                          selected = "random")
            )),
            column(3, conditionalPanel(
              sprintf("input.ptype_%d === 'AI'", p),
              selectInput(paste0("ptrump_", p), "Trump strategy",
                          choices = c("Max points" = "Max points",
                                      "Max cards"  = "Max cards",
                                      "Random"     = "Random"),
                          selected = "Max points")
            ))
          ),
          conditionalPanel(
            sprintf("input.ptype_%d === 'AI' && input.pstrat_%d === 'gemini'", p, p),
            textInput(paste0("pmodel_", p), "Gemini model",
                      value = "gemini-2.5-flash-lite")
          )
        )
      }),

      # Gemini API key (shown when any player uses Gemini)
      conditionalPanel(
        paste(sprintf("input.pstrat_%d === 'gemini' && input.ptype_%d === 'AI'", 1:4, 1:4),
              collapse = " || "),
        passwordInput("api_key", "Gemini API key",
                      value = Sys.getenv("GEMINI_API_KEY"),
                      placeholder = "Leave blank to use GEMINI_API_KEY env var")
      ),

      actionButton("btn_deal", "Deal!", class = "btn btn-deal")
    )
  ),

  # Trump selection panel -----------------------------------------------------
  conditionalPanel("output.phase === 'trump'",
    div(class = "trump-panel",
      uiOutput("trump_header"),
      lapply(SUITS, function(s) {
        actionButton(paste0("trump_", s), paste(suit_icon(s), s),
                     class = "btn btn-default suit-btn")
      }),
      hr(style = "border-color: rgba(255,255,255,0.2); margin: 20px 0;"),
      h4(style = "color:#ddd; margin-bottom:8px;", "Your hand:"),
      uiOutput("trump_hand_ui")
    )
  ),

  # Meld panel ----------------------------------------------------------------
  conditionalPanel("output.phase === 'meld'",
    div(class = "meld-panel",
      h3("Meld Declarations"),
      uiOutput("meld_table_ui"),
      br(),
      actionButton("btn_start_play", "Start Playing \u25B6", class = "btn btn-start")
    )
  ),

  # Main play panel -----------------------------------------------------------
  conditionalPanel("output.phase === 'play'",
    uiOutput("score_bar_ui"),
    div(class = "game-table",
      # North (Player 3)
      div(class = "gt-north",
        uiOutput("label_north"),
        uiOutput("hand_north")
      ),
      # West (Player 4)
      div(class = "gt-west",
        uiOutput("label_west"),
        uiOutput("hand_west")
      ),
      # Trick area
      div(class = "gt-trick",
        div(class = "trick-grid",
          div(class = "tn", uiOutput("trick_north")),
          div(class = "tw", uiOutput("trick_west")),
          div(class = "tc", uiOutput("trick_lead")),
          div(class = "te", uiOutput("trick_east")),
          div(class = "ts", uiOutput("trick_south"))
        )
      ),
      # East (Player 2)
      div(class = "gt-east",
        uiOutput("label_east"),
        uiOutput("hand_east")
      ),
      # South (Player 1)
      div(class = "gt-south",
        uiOutput("label_south"),
        uiOutput("hand_south")
      )
    ),
    div(class = "status-bar", textOutput("status_text")),
    uiOutput("next_trick_btn_ui"),
    uiOutput("last_trick_strip")
  ),

  # Hot-seat pass screen ------------------------------------------------------
  conditionalPanel("output.phase === 'pass'",
    div(class = "pass-overlay",
      h2("\U0001F3B4 Device Handoff"),
      uiOutput("pass_message"),
      uiOutput("btn_reveal")
    )
  ),

  # Round end panel -----------------------------------------------------------
  conditionalPanel("output.phase === 'round_end'",
    div(class = "round-end-panel",
      h2("Round Complete!"),
      uiOutput("round_score_ui"),
      br(),
      actionButton("btn_next_round", "Deal Next Round", class = "btn btn-next")
    )
  ),

  # Game end panel ------------------------------------------------------------
  conditionalPanel("output.phase === 'game_end'",
    div(class = "game-end-panel",
      h1("\U0001F3C6 Game Over"),
      uiOutput("game_end_ui"),
      br(),
      actionButton("btn_new_game", "New Game", class = "btn btn-new-game")
    )
  )
)

# ── Server ───────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # ── Reactive state ────────────────────────────────────────────────────────
  game        <- reactiveVal(NULL)
  phase       <- reactiveVal("setup")
  status_msg  <- reactiveVal("")
  meld_data   <- reactiveVal(NULL)
  prev_human  <- reactiveVal(NULL)   # last human who played (player number)
  pass_target <- reactiveVal(NULL)   # next human who needs the device
  round_end_scores <- reactiveVal(NULL)  # list(trick, meld, total, game_before)

  # expose phase to client
  output$phase <- renderText(phase())
  outputOptions(output, "phase", suspendWhenHidden = FALSE)

  # ── Config helpers ────────────────────────────────────────────────────────
  get_cfg <- function() {
    list(
      names   = vapply(1:4, function(p) input[[paste0("pname_", p)]] %||% paste0("Player ", p), character(1)),
      human   = vapply(1:4, function(p) isTRUE(input[[paste0("ptype_", p)]] == "Human"), logical(1)),
      strat   = vapply(1:4, function(p) input[[paste0("pstrat_", p)]] %||% "random", character(1)),
      model   = vapply(1:4, function(p) input[[paste0("pmodel_", p)]] %||% "gemini-2.5-flash-lite", character(1)),
      trump_s = vapply(1:4, function(p) input[[paste0("ptrump_", p)]] %||% "Max points", character(1)),
      api_key = if (is.null(input$api_key)) "" else input$api_key
    )
  }

  # ── AI rules factory ──────────────────────────────────────────────────────
  make_rules <- function(player_idx) {
    cfg <- get_cfg()
    if (!cfg$human[player_idx] && cfg$strat[player_idx] == "gemini") {
      model   <- cfg$model[player_idx]
      api_key <- cfg$api_key
      function(trk, h, state = NULL, ...) {
        if (nzchar(api_key)) {
          old <- Sys.getenv("GEMINI_API_KEY")
          Sys.setenv(GEMINI_API_KEY = api_key)
          on.exit({
            if (nzchar(old)) Sys.setenv(GEMINI_API_KEY = old)
            else Sys.unsetenv("GEMINI_API_KEY")
          })
        }
        tryCatch(
          pick_card_gemini(trk, h, state, model = model),
          error = function(e) pick_random_valid_card(trk, h)
        )
      }
    } else {
      pick_random_valid_card
    }
  }

  # ── Game flow helpers ──────────────────────────────────────────────────────

  handle_round_end <- function(g) {
    trick_pts    <- compute_round_trick_pts(g)
    md           <- meld_data()
    meld_pts     <- if (!is.null(md)) md$team_scores else c(`Team 1` = 0L, `Team 2` = 0L)
    round_pts    <- trick_pts + as.integer(meld_pts)
    game_before  <- g@score

    for (i in seq_along(g@score))
      g@score[i] <- as.integer(g@score[i] + round_pts[i])

    game(g)
    round_end_scores(list(trick = trick_pts, meld = meld_pts,
                          total = round_pts, game_before = game_before,
                          game_after = g@score))
    meld_data(NULL)

    if (any(g@score >= 1000L)) phase("game_end") else phase("round_end")
  }

  check_hot_seat <- function() {
    g      <- game()
    next_p <- g@round@next_player
    prev_p <- prev_human()
    cfg    <- get_cfg()

    if (!is.null(prev_p) && next_p != prev_p && cfg$human[next_p]) {
      pass_target(next_p)
      phase("pass")
    } else if (cfg$human[next_p]) {
      status_msg(paste0(cfg$names[next_p], "'s turn \u2014 choose a card to play"))
    }
  }

  # Fire AI turns one at a time, with a short delay between each for readability.
  # Stops when a trick is complete (4 cards) — user must click Next Trick.
  advance_ai <- function() {
    g <- isolate(game())
    if (is.null(g)) return()

    # Trick just completed: stop and wait for user to click Next Trick
    if (nrow(cards(g@round@trick)) >= 4L) {
      status_msg("Trick complete \u2014 press \u2018Next Trick\u2019 to continue")
      return()
    }

    next_p <- g@round@next_player
    cfg    <- isolate(get_cfg())

    if (cfg$human[next_p]) {
      check_hot_seat()
      return()
    }

    # It's an AI turn
    rules_fn <- make_rules(next_p)
    status_msg(paste0(cfg$names[next_p], " is thinking\u2026"))

    shinyjs::delay(700, {
      g2 <- isolate(game())
      if (is.null(g2)) return()
      g2 <- tryCatch(
        play(g2, rules = rules_fn, auto = FALSE, advance_trick = FALSE, state = g2),
        error = function(e) play(g2, rules = pick_random_valid_card, auto = FALSE, advance_trick = FALSE)
      )
      game(g2)
      status_msg("")
      advance_ai()
    })
  }

  # After deal: handle trump selection (with hot-seat pass if needed)
  start_trump_phase <- function() {
    g      <- game()
    p      <- g@round@next_player
    cfg    <- get_cfg()
    prev_p <- prev_human()

    if (cfg$human[p]) {
      # Show pass screen if a different human was last at the device
      if (!is.null(prev_p) && p != prev_p && sum(cfg$human) > 1L) {
        pass_target(p)
        phase("pass")  # btn_reveal will route to "trump" since trump is not yet set
      } else {
        phase("trump")
      }
    } else {
      # AI picks trump
      ts   <- cfg$trump_s[p]
      suit <- tryCatch(pick_random_trump(g, p, ts), error = function(e) sample(SUITS, 1))
      trump(g) <- suit
      meld_data(meld(g))
      game(g)
      phase("meld")
    }
  }

  # ── Deal button ────────────────────────────────────────────────────────────
  observeEvent(input$btn_deal, {
    cfg <- get_cfg()
    g   <- new("Game")

    # configure player names and human/AI flags
    human_idxs <- which(cfg$human)
    if (length(human_idxs) > 0) g <- set_human(g, human_idxs)

    g <- deal(g)
    game(g)
    prev_human(NULL)
    pass_target(NULL)
    meld_data(NULL)
    round_end_scores(NULL)
    status_msg("")
    start_trump_phase()
  })

  # ── Trump selection buttons ────────────────────────────────────────────────
  lapply(SUITS, function(s) {
    observeEvent(input[[paste0("trump_", s)]], {
      req(phase() == "trump")
      g <- game()
      trump(g) <- s
      game(g)
      meld_data(meld(g))
      phase("meld")
    })
  })

  # ── Meld dismiss ──────────────────────────────────────────────────────────
  observeEvent(input$btn_start_play, {
    req(phase() == "meld")
    phase("play")
    advance_ai()
  })

  # ── Human card click ──────────────────────────────────────────────────────
  observeEvent(input$card_click, {
    req(phase() == "play")
    g <- game()
    if (is.null(g)) return()
    next_p <- g@round@next_player
    cfg    <- get_cfg()
    req(cfg$human[next_p])

    card_code <- input$card_click
    valid <- tryCatch(
      validate_card_choice(card_code, g@round@trick, g@round@hands[[next_p]]) == card_code,
      error = function(e) FALSE
    )
    req(valid)

    prev_human(next_p)
    g <- play(g, to_play = card_code, auto = FALSE, advance_trick = FALSE, state = g)
    game(g)
    status_msg("")
    advance_ai()
  })

  # ── Next Trick button ─────────────────────────────────────────────────────
  observeEvent(input$btn_next_trick, {
    req(phase() == "play")
    g <- game()
    req(!is.null(g), nrow(cards(g@round@trick)) >= 4L)

    g <- next_trick(g)
    game(g)

    # After the 9th trick next_trick() pushes history to length 9 → round over
    if (length(g@round@history) == 9L) {
      handle_round_end(g)
    } else {
      status_msg("")
      advance_ai()
    }
  })

  # ── Pass screen reveal ────────────────────────────────────────────────────
  observeEvent(input$btn_reveal, {
    req(phase() == "pass")
    tgt <- pass_target()
    prev_human(tgt)
    pass_target(NULL)
    g <- game()
    if (!nzchar(g@round@trump %||% "")) {
      phase("trump")  # trump not yet set: go to trump selection
    } else {
      phase("play")
      cfg    <- get_cfg()
      next_p <- g@round@next_player
      status_msg(paste0(cfg$names[next_p], "'s turn \u2014 choose a card to play"))
    }
  })

  # ── Next round ────────────────────────────────────────────────────────────
  observeEvent(input$btn_next_round, {
    req(phase() == "round_end")
    g <- game()
    # Deal new round (advancing start player handled inside deal() for Game)
    g@round <- deal(new("Round"))
    advance_play <- as.integer(c(2, 3, 4, 1))
    g@start <- advance_play[g@start]
    g@round@next_player <- g@start
    game(g)
    prev_human(NULL)
    pass_target(NULL)
    meld_data(NULL)
    round_end_scores(NULL)
    status_msg("")
    start_trump_phase()
  })

  # ── New game ──────────────────────────────────────────────────────────────
  observeEvent(input$btn_new_game, {
    game(NULL)
    phase("setup")
    status_msg("")
    meld_data(NULL)
    prev_human(NULL)
    pass_target(NULL)
    round_end_scores(NULL)
  })

  # ── Rendering helpers ──────────────────────────────────────────────────────

  # Player position constants: seating = South/East/North/West for players 1/2/3/4
  SEAT <- c(1L, 2L, 3L, 4L)  # player numbers for S, E, N, W
  POSITIONS <- c("south", "east", "north", "west")

  player_pos <- function(player_num) POSITIONS[player_num]

  render_hand_ui <- function(player_num, active) {
    g <- game()
    if (is.null(g)) return(NULL)
    cfg <- get_cfg()

    hand_df <- cards(g, player_num)
    if (nrow(hand_df) == 0) return(NULL)
    abbrs <- card_abbr(hand_df)

    if (!active) {
      # Show card backs for non-active players
      do.call(tagList, lapply(seq_along(abbrs), function(i) card_back_div()))
    } else {
      # Determine valid cards (mirrors validate_card_choice logic)
      valid_abbrs <- tryCatch({
        trk <- g@round@trick
        if (trk@lead_suit == "") {
          abbrs  # leading – all cards valid
        } else {
          lead_df  <- hand_df[hand_df$suit == trk@lead_suit,  , drop = FALSE]
          trump_df <- hand_df[hand_df$trump,                  , drop = FALSE]
          if (nrow(lead_df) == 0L) {
            abbrs  # no lead-suit cards: ALL cards valid
          } else {
            card_abbr(unique(rbind(lead_df, trump_df)))
          }
        }
      }, error = function(e) abbrs)

      do.call(tagList, lapply(abbrs, function(a) {
        disabled <- !(a %in% valid_abbrs)
        card_div(a, clickable = TRUE, disabled = disabled)
      }))
    }
  }

  render_trick_slot <- function(player_num) {
    g <- game()
    if (is.null(g)) return(card_empty_div())
    trick_df <- cards(g@round@trick)
    if (nrow(trick_df) == 0) return(card_empty_div())
    row <- trick_df[trick_df$player == player_num, ]
    if (nrow(row) == 0) return(card_empty_div())
    abbr <- card_abbr(row)[1]
    tags$div(class = "card-slot",
             tags$img(src = card_png(abbr), class = "card-img"),
             tags$div(class = "card-label", card_label(abbr)))
  }

  is_active_player <- function(player_num) {
    g   <- game()
    cfg <- get_cfg()
    if (is.null(g)) return(FALSE)
    g@round@next_player == player_num && cfg$human[player_num] && phase() == "play"
  }

  make_label <- function(player_num) {
    g   <- game()
    cfg <- get_cfg()
    if (is.null(g) || is.null(cfg)) return(NULL)
    active <- is_active_player(player_num)
    team   <- g@teams[player_num]
    team_col <- if (team == "Team 1") "color:#ffd700" else "color:#87ceeb"
    name_txt <- cfg$names[player_num]
    n_cards  <- sum(g@round@hands[[player_num]]@cards$inhand)
    cls <- paste("player-label", if (active) "active-player" else "")
    div(class = cls,
        span(style = team_col, name_txt),
        span(style = "color:#aaa; font-size:11px;", paste0(" [", team, "] \u2022 ", n_cards, " cards")),
        if (active) span(style = "color:#ffd700;", " \u25C4 YOUR TURN") else NULL
    )
  }

  # Score bar
  output$score_bar_ui <- renderUI({
    g <- game()
    if (is.null(g)) return(NULL)
    # running totals = game score + current round trick points
    round_pts <- vapply(seq_along(g@round@won), function(i) {
      df <- cards(g@round@won[[i]])
      if (nrow(df) == 0L) return(0L)
      as.integer(sum(card_value(df$face, df$trump)))
    }, integer(1))
    names(round_pts) <- names(g@score)
    live <- g@score + round_pts
    trump_str <- if (nzchar(g@round@trump %||% "")) {
      paste(suit_icon(g@round@trump), g@round@trump)
    } else "—"
    div(class = "score-bar",
      span(class = "team1", paste("Team 1:", live["Team 1"], "pts")),
      span(class = "trump-badge", trump_str),
      span(class = "team2", paste("Team 2:", live["Team 2"], "pts"))
    )
  })

  # Next Trick button — only shown when all 4 cards are on the table
  output$next_trick_btn_ui <- renderUI({
    req(phase() == "play")
    g <- game()
    if (is.null(g) || nrow(cards(g@round@trick)) < 4L) return(NULL)
    div(style = "text-align:center; margin:10px 0;",
        actionButton("btn_next_trick", "Next Trick \u25B6", class = "btn btn-next btn-lg"))
  })
  outputOptions(output, "next_trick_btn_ui", suspendWhenHidden = FALSE)

  # Player labels
  output$label_south <- renderUI(make_label(1L))
  output$label_east  <- renderUI(make_label(2L))
  output$label_north <- renderUI(make_label(3L))
  output$label_west  <- renderUI(make_label(4L))

  # Hands
  output$hand_south <- renderUI(render_hand_ui(1L, is_active_player(1L)))
  output$hand_east  <- renderUI(render_hand_ui(2L, is_active_player(2L)))
  output$hand_north <- renderUI(render_hand_ui(3L, is_active_player(3L)))
  output$hand_west  <- renderUI(render_hand_ui(4L, is_active_player(4L)))

  # Trick slots
  output$trick_south <- renderUI(render_trick_slot(1L))
  output$trick_east  <- renderUI(render_trick_slot(2L))
  output$trick_north <- renderUI(render_trick_slot(3L))
  output$trick_west  <- renderUI(render_trick_slot(4L))

  output$trick_lead <- renderUI({
    g <- game()
    if (is.null(g)) return(NULL)
    ls <- g@round@trick@lead_suit
    if (nzchar(ls)) {
      tags$small(paste(suit_icon(ls), ls))
    } else {
      tags$small(style = "color:#666;", "lead")
    }
  })

  output$status_text <- renderText(status_msg())

  output$last_trick_strip <- renderUI({
    g <- game()
    if (is.null(g) || length(g@round@history) == 0) return(NULL)
    last <- g@round@history[[length(g@round@history)]]
    df   <- cards(last)
    if (nrow(df) == 0) return(NULL)
    winner <- g@round@next_player
    cfg    <- get_cfg()
    lines  <- paste(vapply(seq_len(nrow(df)), function(i) {
      sprintf("P%d: %s%s", df$player[i],
              faceTranslation(df$face[i]), " of ", df$suit[i])
    }, character(1)), collapse = "  |  ")
    tags$details(
      class = "last-trick-strip",
      tags$summary(paste("Last trick \u2014 won by", cfg$names[winner])),
      do.call(tagList, lapply(card_abbr(df), function(a) {
        tags$div(class = "card-slot",
                 tags$img(src = card_png(a), class = "card-img", style = "width:52px;"),
                 tags$div(class = "card-label", style = "font-size:10px;", card_label(a)))
      }))
    )
  })

  # Trump header
  output$trump_header <- renderUI({
    g <- game()
    if (is.null(g)) return(NULL)
    cfg <- get_cfg()
    p   <- g@round@next_player
    div(h3(paste(cfg$names[p], "\u2014 choose trump suit:")))
  })

  # Hand shown during trump selection (read-only)
  output$trump_hand_ui <- renderUI({
    g <- game()
    if (is.null(g) || phase() != "trump") return(NULL)
    p      <- g@round@next_player
    hand_df <- cards(g, p)
    if (nrow(hand_df) == 0) return(NULL)
    do.call(tagList, lapply(card_abbr(hand_df), function(a) card_div(a, clickable = FALSE)))
  })

  # Meld table
  output$meld_table_ui <- renderUI({
    md  <- meld_data()
    g   <- game()
    cfg <- get_cfg()
    if (is.null(md) || is.null(g)) return(NULL)

    win_p  <- md$winning_player
    win_t  <- if (!is.na(win_p)) g@teams[win_p] else NA_character_

    rows <- lapply(1:4, function(p) {
      melds  <- md$player_melds[[p]]
      n_pts  <- sum(vapply(melds, `[[`, numeric(1), "points"))
      meld_desc <- if (length(melds) == 0) {
        "—"
      } else {
        paste(vapply(melds, function(m) paste0(m$type, " (", m$points, " pts)"), character(1)),
              collapse = "; ")
      }
      team   <- g@teams[p]
      is_win <- !is.na(win_t) && team == win_t
      tags$tr(class = if (is_win) "meld-winner" else "",
        tags$td(cfg$names[p]),
        tags$td(team),
        tags$td(meld_desc),
        tags$td(n_pts)
      )
    })

    team_row <- function(team) {
      pts <- md$team_scores[team]
      is_win <- !is.na(win_t) && team == win_t
      tags$tr(class = if (is_win) "meld-winner" else "",
        tags$td(colspan = "3", tags$strong(paste(team, "total:"))),
        tags$td(tags$strong(pts))
      )
    }

    tags$table(class = "meld-table",
      tags$thead(tags$tr(
        tags$th("Player"), tags$th("Team"), tags$th("Melds"), tags$th("Pts")
      )),
      tags$tbody(
        do.call(tagList, rows),
        tags$tr(tags$td(colspan = "4", tags$hr(style = "border-color:rgba(255,255,255,0.2)"))),
        team_row("Team 1"),
        team_row("Team 2"),
        if (!is.na(win_t))
          tags$tr(tags$td(colspan = "4",
            tags$em(style = "color:#ffd700;",
                    paste("\u2605", win_t, "scores their meld points"))))
        else
          tags$tr(tags$td(colspan = "4", tags$em(style = "color:#ccc;", "No melds")))
      )
    )
  })

  # Pass screen
  output$pass_message <- renderUI({
    tgt <- pass_target()
    cfg <- get_cfg()
    if (is.null(tgt)) return(NULL)
    tagList(
      tags$p(paste("Please pass the device to", cfg$names[tgt])),
      tags$p(class = "text-muted", style = "color:#aaa;",
             "Their hand is hidden until they tap below.")
    )
  })

  output$btn_reveal <- renderUI({
    tgt <- pass_target()
    cfg <- get_cfg()
    if (is.null(tgt)) return(NULL)
    actionButton("btn_reveal",
                 paste0("I'm ", cfg$names[tgt], " \u2014 Show My Hand"),
                 class = "btn btn-lg btn-start")
  })

  # Round end scores
  output$round_score_ui <- renderUI({
    rs  <- round_end_scores()
    if (is.null(rs)) return(NULL)
    rows <- lapply(c("Team 1", "Team 2"), function(tm) {
      tags$tr(
        tags$td(tm),
        tags$td(rs$trick[tm]),
        tags$td(rs$meld[tm]),
        tags$td(tags$strong(rs$total[tm])),
        tags$td(rs$game_after[tm])
      )
    })
    tags$table(class = "score-table",
      tags$thead(tags$tr(
        tags$th("Team"), tags$th("Tricks + bonus"), tags$th("Meld"),
        tags$th("Round total"), tags$th("Game total")
      )),
      tags$tbody(do.call(tagList, rows))
    )
  })

  # Game end
  output$game_end_ui <- renderUI({
    g <- game()
    if (is.null(g)) return(NULL)
    winner <- names(which.max(g@score))
    tagList(
      div(class = "winner-badge", paste("\U0001F3C6", winner, "wins!")),
      br(),
      tags$table(class = "score-table",
        tags$thead(tags$tr(tags$th("Team"), tags$th("Final Score"))),
        tags$tbody(
          tags$tr(tags$td("Team 1"), tags$td(g@score["Team 1"])),
          tags$tr(tags$td("Team 2"), tags$td(g@score["Team 2"]))
        )
      )
    )
  })
}

shinyApp(ui, server)
