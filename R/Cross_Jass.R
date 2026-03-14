# Cross_Jass.R

#' Validate a card choice
#'
#' @description Determines the set of legal cards for the current trick and
#'   hand, then returns \code{choice} if it is legal. If \code{choice} is
#'   \code{NULL} or is not a legal play, a random legal card is returned
#'   instead. This encapsulates the core Jass play-validity rules:
#'   follow the lead suit when possible, and trump may always be played.
#' @name validate_card_choice
#' @rdname Cross-Jass
#'
#' @param choice A two-character card abbreviation to validate, or \code{NULL}
#'   to simply draw a random legal card.
#' @param trk A Trick object
#' @param h A Hand object
#' 
#' @return A character value of a valid card to play
#' @importFrom dplyr filter bind_rows
#' @importFrom rlang .data
#' @export
validate_card_choice <- function(choice = NULL, trk, h)
{
  hand_df <- cards(h)

  if (trk@lead_suit == '')
  {
    valid_abbrs <- card_abbr(hand_df)
  } else {
    # must follow lead suit if possible
    valid <- filter(hand_df, .data$suit == trk@lead_suit)
    if (nrow(valid) == 0)
      valid <- hand_df

    # trump may always be played
    valid       <- bind_rows(filter(hand_df, .data$trump), valid) |> unique()
    valid_abbrs <- card_abbr(valid)
  }

  if (!is.null(choice) && choice %in% valid_abbrs)
    return(choice)

  if(!is.null(choice))
    warning("Invalid choice, returning a random, valid card.")
  
  sample(valid_abbrs, 1)
}


#' Pick a random, valid card to play
#'
#' @description This is simplest function to pick a card to play. It picks a random following the basic Jass rules.
#' @name pick_random_valid_card
#' @rdname Cross-Jass
#'
#' @param trk A Trick object
#' @param h A Hand object
#' @param ... Other arguments that are ignored by this function
#' @return A character value of the card to play
#' @export
pick_random_valid_card<- function(trk, h, ...)
{
  validate_card_choice(trk = trk, h = h)
}

#' Have Gemini pick a card to play
#'
#' @description This function will have Gemini pick a card to play based on the current game status, 
#' including which cards have been played and by whom.
#' @name pick_card_gemini
#' @rdname Cross-Jass
#'
#' @param trk A Trick object
#' @param h A Hand object
#' @param state A Game or Round object providing round history and full game context. If NULL, history is omitted from the query.
#' @param ... Other arguments that are ignored by this function
#' 
#' @details  This function assumes Gemini knows the basic Jass rules and will pick an appropriate card.
#' If Gemini returns an invalid or unrecognised card, the function falls back to \code{pick_random_valid_card}.
#' 
#' @return A character value of the card to play
#' @importFrom ai4teaching genAI_query
#' @importFrom OPsecrets get_secret
#' @export
pick_card_gemini <- function(trk, h, state = NULL, ...)
{
  # derive trump suit name from the hand's card flags (Trick has no trump slot)
  trump_suit <- unique(h@cards$suit[h@cards$trump])

  # ---- build query preamble ----
  query <- paste0(
    "We are playing a game of Cross Jass (Schieber Jass), a Swiss trick-taking card game.\n",
    "Card faces: 6=6, 7=7, 8=8, 9=9, B=Banner(10), U=Under(Jack), O=Ober(Queen), K=King, A=Ace\n",
    "Suits: B=Bells, F=Flowers, S=Shields, A=Acorns\n",
    "Trump suit: ", trump_suit, "\n",
    "Trump rules: trump cards beat all non-trump cards; ",
    "within trump, Under (U) and 9 rank highest.\n",
    "Play rules: you must follow the lead suit if you have it; you may always play trump.\n\n"
  )

  # ---- round history ----
  if (!is.null(state))
  {
    history_data <- round_history(state, verbose = FALSE)
    if (!is.data.frame(history_data) && length(history_data) > 0)
    {
      query <- paste0(query, "Cards played in previous tricks this round:\n")
      for (i in seq_along(history_data))
      {
        tbl   <- history_data[[i]]
        lines <- with(tbl, paste0("  Player ", player, " played the ",
                                  faceTranslation(face), " of ", suit))
        query <- paste0(query, "Trick ", i, ":\n", paste(lines, collapse = "\n"), "\n")
      }
      query <- paste0(query, "\n")
    }
  }

  # ---- current trick ----
  on_table <- cards(trk)
  if (nrow(on_table) > 0)
  {
    lines <- with(on_table, paste0("  Player ", player, " played the ",
                                   faceTranslation(face), " of ", suit))
    query <- paste0(query,
                    "Cards currently on the table (lead suit: ", trk@lead_suit, "):\n",
                    paste(lines, collapse = "\n"), "\n\n")
  } else {
    query <- paste0(query, "No cards have been played yet in this trick. You lead.\n\n")
  }

  # ---- hand ----
  hand_df    <- cards(h)
  hand_abbrs <- card_abbr(hand_df)
  hand_lines <- with(hand_df,
                     paste0("  ", hand_abbrs, " (", faceTranslation(face), " of ", suit,
                            ifelse(trump, " [trump]", ""), ")"))
  query <- paste0(query,
                  "Your remaining cards:\n",
                  paste(hand_lines, collapse = "\n"), "\n\n",
                  "Pick one card to play. Respond with ONLY the two-character card code ",
                  "(first letter of suit + face value, e.g. B9 for the 9 of Bells, ",
                  "F8 for the 8 of Flowers). Do not include any other text.")

  # ---- query Gemini ----
  result <- genAI_query(query,
                        model   = 'gemini-2.5-flash-lite',
                        api_key = get_secret('GEMINI_API_KEY', 'Private', 'Gemini', 'api_key'))

  chosen <- trimws(result$response)

  # ---- validate and return ----
  validate_card_choice(chosen, trk, h)
}

#' Pick a random suit for trump
#'
#' @description This is just about the simplest function to pick trump suit.
#' @name pick_random_trump
#' @rdname Cross-Jass
#'
#' @param g A game object
#' @param p The player number who is picking trump
#' @param strategy A string identifying the strategy to use (see details).
#' @param ... All arguments are ignored by this function
#' @details Current strategies for picking a suit are:
#' Random - Pick a suit completely at random
#' Max points - Pick the suit worth the most points if it were trump
#' Max cards - Pick the suit that has the most cards in the hand
#' @return A character value of the trump suit
#' @importFrom purrr map
#' @importFrom stats runif
#' @export
pick_random_trump <- function(g, p, strategy = 'Max points', ...)
{
  # take care of no visible binding notes
  if(FALSE)
    suit <- NULL
  
  # otherwise 'Random'
  scores <- runif(4)

  if(strategy == 'Max points')
  {
    scores <- sapply(c('Bells', 'Flowers', 'Shields', 'Acorns'),
                      function(.x) with(filter(cards(g, p), suit == .x), sum(card_value(face, TRUE))))
  }

  if(strategy == 'Max cards')
  {
    scores <- sapply(c('Bells', 'Flowers', 'Shields', 'Acorns'),
                     function(.x) with(filter(cards(g, p), suit == .x), length(face)))
  }

  return(names(scores)[which.max(scores)])
}
