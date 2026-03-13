# meld_methods.R

# Point value for a sequence of given length
.sequence_points <- function(len)
{
  if(len == 3L) return(20L)
  if(len == 4L) return(50L)
  return(100L) # 5 or more
}

# Find all individual melds in a hand's card data.frame.
# hand_cards: data.frame returned by cards(Hand)
# trump_suit: character, e.g. 'Bells' ('' if not yet set)
# Returns a list of meld lists, each with: type, length, points, trump
.hand_melds <- function(hand_cards, trump_suit)
{
  melds <- list()

  # --- Sequences: 3+ consecutive cards of the same suit (regular card order) ---
  for(s in c('Bells', 'Flowers', 'Shields', 'Acorns'))
  {
    suit_cards <- hand_cards[hand_cards$suit == s, ]
    if(nrow(suit_cards) < 3L) next

    ord <- sort(card_order(suit_cards$face, rep(FALSE, nrow(suit_cards))))

    run_len <- 1L
    for(i in seq(2L, length(ord)))
    {
      if(ord[i] - ord[i - 1L] == 1L)
      {
        run_len <- run_len + 1L
      } else {
        if(run_len >= 3L)
          melds <- c(melds, list(list(type   = 'sequence',
                                      suit   = s,
                                      length = run_len,
                                      points = .sequence_points(run_len),
                                      trump  = s == trump_suit)))
        run_len <- 1L
      }
    }
    # capture the final run
    if(run_len >= 3L)
      melds <- c(melds, list(list(type   = 'sequence',
                                   suit   = s,
                                   length = run_len,
                                   points = .sequence_points(run_len),
                                   trump  = s == trump_suit)))
  }

  # --- Four of a kind (Jacks=200, Nines=150; Aces/Kings/Obers/Banners=100) ---
  four_pts <- c(U = 200L, `9` = 150L, A = 100L, K = 100L, O = 100L, B = 100L)
  for(f in names(four_pts))
  {
    if(sum(hand_cards$face == f) == 4L)
      melds <- c(melds, list(list(type   = 'four_of_a_kind',
                                   face   = f,
                                   length = 4L,
                                   points = four_pts[[f]],
                                   trump  = any(hand_cards$face == f &
                                                  hand_cards$trump))))
  }

  # --- Marriage: King + Ober in the trump suit ---
  if(nchar(trump_suit) > 0L)
  {
    trump_faces <- hand_cards$face[hand_cards$suit == trump_suit]
    if('K' %in% trump_faces && 'O' %in% trump_faces)
      melds <- c(melds, list(list(type   = 'marriage',
                                   suit   = trump_suit,
                                   length = 2L,
                                   points = 20L,
                                   trump  = TRUE)))
  }

  melds
}

# TRUE if meld_a strictly beats meld_b.
# Tie-break order: points > card count > trump suit.
.meld_beats <- function(meld_a, meld_b)
{
  if(meld_a$points != meld_b$points) return(meld_a$points > meld_b$points)
  if(meld_a$length != meld_b$length) return(meld_a$length > meld_b$length)
  isTRUE(meld_a$trump) && !isTRUE(meld_b$trump)
}

# Return the best (highest-ranked) meld from a list; NULL if the list is empty.
.best_meld <- function(melds)
{
  if(length(melds) == 0L) return(NULL)
  best <- melds[[1L]]
  for(m in melds[-1L])
    if(.meld_beats(m, best)) best <- m
  best
}


#' Calculate meld scores for a round of Cross Jass
#'
#' @description Calculates meld scores based on each player's current hand.
#'   Scoring rules:
#'   \itemize{
#'     \item Sequence of 3: 20 points
#'     \item Sequence of 4: 50 points
#'     \item Sequence of 5+: 100 points
#'     \item Four Jacks (Under): 200 points
#'     \item Four Nines: 150 points
#'     \item Four Aces, Kings, Obers, or Banners: 100 points
#'     \item Marriage (King + Ober in trump suit): 20 points
#'   }
#'   Only the team with the highest individual meld scores their combined
#'   team meld points.  Tie-breaking: more cards wins; if still tied, the
#'   meld in the trump suit wins.
#'
#' @name meld
#' @rdname meld-methods
#'
#' @param obj A \code{Round} or \code{Game} object
#' @param ... Other arguments
#'
#' @return A list with four elements:
#'   \describe{
#'     \item{player_melds}{A list (one per player) of all individual melds for
#'       that player.  Each meld is itself a list with \code{type}, \code{length},
#'       \code{points}, and \code{trump}.}
#'     \item{best_per_player}{A list of each player's single highest-ranked meld
#'       (\code{NULL} if the player has no melds).}
#'     \item{winning_player}{Integer index of the player holding the overall best
#'       meld (\code{NA} if no player has any melds).}
#'     \item{team_scores}{Named integer vector giving the meld points awarded to
#'       each team (only the winning team receives points).}
#'   }
#' @export
setGeneric("meld",
           function(obj, ...) standardGeneric("meld"),
           signature = c('obj'))

#' @docType methods
#' @rdname meld-methods
setMethod('meld', 'Round', function(obj, ...)
{
  trump_suit <- if(length(obj@trump) > 0L) obj@trump else ''

  # All melds for each player
  player_melds <- lapply(obj@hands, function(h) .hand_melds(cards(h), trump_suit))

  # Best individual meld per player
  best_per_player <- lapply(player_melds, .best_meld)

  # Overall winning player (the one holding the best single meld)
  winning_player <- NA_integer_
  overall_best   <- NULL

  for(i in seq_along(best_per_player))
  {
    bp <- best_per_player[[i]]
    if(is.null(bp)) next
    if(is.null(overall_best) || .meld_beats(bp, overall_best))
    {
      overall_best   <- bp
      winning_player <- as.integer(i)
    }
  }

  # Only the winning team scores; their points are the sum of all
  # their players' individual melds
  team_scores <- c(`Team 1` = 0L, `Team 2` = 0L)

  if(!is.na(winning_player))
  {
    winning_team <- obj@teams[winning_player]
    team_players <- which(obj@teams == winning_team)
    total <- sum(sapply(team_players, function(i) {
      pts <- unlist(lapply(player_melds[[i]], `[[`, 'points'), use.names = FALSE)
      if(length(pts) == 0L) 0L else sum(pts)
    }))
    team_scores[winning_team] <- as.integer(total)
  }

  list(player_melds    = player_melds,
       best_per_player = best_per_player,
       winning_player  = winning_player,
       team_scores     = team_scores)
})

#' @docType methods
#' @rdname meld-methods
setMethod('meld', 'Game', function(obj, ...)
{
  meld(obj@round)
})
