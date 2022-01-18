# round_methods.R
# methods for governing a round of play

#utils::globalVariables(".y")

#' Deal a round of cards
#'
#' @description Deals a round of cards.
#' @name deal
#' @rdname round-methods
#'
#' @param obj An object of the correct class
#' @param game A character value defining the game being played
#' @param n A numeric value defining the number of players or hands to be dealt
#' @param ... Other arguments
#' @importFrom purrr map2
#' @export
setGeneric('deal',
           function(obj, game = NULL, n = NULL, ...) standardGeneric('deal'),
           signature = c('obj', 'game', 'n'))

#' @docType methods
#' @rdname round-methods
setMethod('deal', 'Round', function(obj, game = 'Cross Jass', n = 4, ...)
{
  if(game == 'Cross Jass')
  {
    if(length(obj@hands) != 4)
      stop('Number of players for Cross Jass must be equal to 4.')

    # shuffle cards (9 cards to each player)
    h <- sample(rep(1:4, 9))

    # deal cards to each player
    obj@hands <- purrr::map2(obj@hands, 1:4, function(.x, .y)
      {
        # discard any cards in the hand
        .x <- new('Hand')

        # put new cards in hand
        .x@cards$inhand <- h == .y

        .x
      })
  }

  obj
})

#' @docType methods
#' @rdname round-methods
setMethod('deal', 'Game', function(obj, game = 'Cross Jass', n = 4, ...)
{
  obj@round <- deal(obj@round)

  if(obj@start == 4)
  {
    obj@start <- as.integer(1)
  }else{
    obj@start <- as.integer(obj@start + 1)
  }

  obj@round@next_player <- obj@start

  obj
})


#' Play a card
#'
#' @description This generic plays a valid card based on a model provided and the current state of the game. Optionally, play continues to advance until it is a human's turn.
#' @name play
#' @rdname round-methods
#'
#' @param obj A Game object
#' @param to_play Character string identifying the card to play (for non-AI players)
#' @param rules A function to decide which card to play (for AI players)
#' @param state An object containing information about the game, used as input for the model defined by `rules`
#' @param auto A logical indicating play should continue until a human player's turn. By default, if no cards are specified in to_play, auto is TRUE.
#' @param verbose A logical. When TRUE, verbose output is printed.
#' @export
setGeneric("play",
           function(obj, ...) standardGeneric("play"),
           signature = c('obj'))

#' @docType methods
#' @rdname round-methods
setMethod('play', 'Game', function(obj, to_play = NULL, rules = pick_random_valid_card, state = NULL, auto = is.null(to_play), verbose = TRUE, ...)
{
  while(is_ai(obj, obj@round@next_player) |                       # if the next player is an AI keep going
        (!is_ai(obj, obj@round@next_player) & !is.null(to_play))) # if called by a human, to_play should not be NULL - let it run once, will stop if hitting a human after several AI players have played
  {
    player_turn <- obj@round@next_player

    # pick a card if one hasn't been supplied
    if(is.null(to_play))
    {
      to_play <- rules(obj@round@trick, obj@round@hands[[player_turn]], state)
    }

    # play the card
    # move card from player's hand to the trick
    cards(obj@round@hands[[player_turn]], draw = FALSE) <- to_play
    cards(obj@round@trick@played[[player_turn]]) <- to_play

    # set lead_suit if it hasn't been set yet
    if(obj@round@trick@lead_suit == '')
      obj@round@trick@lead_suit <- suitTranslation(substr(to_play, 1, 1))

    # advance play to the next player (if all players have played, set to NA)
    advance_play <- as.integer(c(2,3,4,1))
    obj@round@next_player <- advance_play[player_turn]

    # if auto is FALSE, break after the first time through the loop
    if(!auto)
      break

    # if we are continuing and all players have played, print status and advance to next trick
    if(nrow(cards(status(obj, verbose = FALSE)$trick)) >= length(obj@players))
    {
      stat <- status(obj, verbose = verbose)
      obj <- next_trick(obj)

      # clean up and break from the loop when the round is over
      if(nrow(cards(obj@round@hands[[1]])) == 0)
      {
        # tally scores for the round
        for(i in 1:length(obj@score))
        {
          obj@score[i] <- as.integer(obj@score[i] + stat$score_round[i])
        }

        # 5-point bonus for winning the last round + cards from the last trick
        won <- g@teams[stat$trick@cards$player[1]]
        obj@score[won] <- as.integer(obj@score[won] + 5 + with(stat$trick@cards, sum(card_value(face, trump))))

        # deal a new hand for the next round
        obj@round <- deal(new('Round'))
        obj@start <- advance_play[obj@start]
        obj@round@next_player <- obj@start
        break
      }
    }

    # reset to_play for the next time around
    to_play <- NULL
  }

  invisible(obj)
})


#' Game status
#'
#' @description This generic returns a list containing the current game status
#' @name status
#' @rdname round-methods
#'
#' @param obj An object of the correct class
#' @param verbose Logical indicating that we should print out status results
#' @param ... Other arguments for specific classes
#'
#' @importFrom purrr map_dbl
#' @importFrom dplyr mutate
#' @export
setGeneric("status",
           function(obj, verbose = TRUE, ...) standardGeneric("status"),
           signature = c('obj'))

#' @docType methods
#' @rdname round-methods
setMethod('status', 'Round', function(obj, verbose = TRUE, ...)
{
  # Cards on the table
  t <- cards(obj@trick)

  # current score for the round
  s <- purrr::map_dbl(1:length(obj@won), ~ with(cards(obj@won[[.x]]), sum(card_value(face, trump))))
  names(s) <- names(obj@won)

  retval <- list(trick = new('Hand', cards = dplyr::mutate(t, inhand = TRUE)),
                 lead_suit = obj@trick@lead_suit,
                 next_player = ifelse(nrow(t) == length(obj@hands), NA, obj@next_player),
                 trump = obj@trump,
                 score_round = s)

  if(verbose)
  {
    cat('Cards on the table:\n')
    print(cards(retval$trick, retval$lead_suit))
    cat('\nNext player:', retval$next_player, '\n')
    cat('\nTrump:', retval$trump, '\n')
    cat('\nCurrent score for the round:\n',
        paste0('    ', names(s), ': ', s, collapse = '\n'), sep = '')
  }

  invisible(retval)
})

#' @docType methods
#' @rdname round-methods
setMethod('status', 'Game', function(obj, verbose = TRUE, ...)
{
  retval <- status(obj@round, verbose = FALSE)

  retval$score <- obj@score + retval$score_round

  if(verbose)
  {
    cat('Cards on the table:\n')
    print(cards(retval$trick, retval$lead_suit))
    cat('\nNext player:', retval$next_player, '\n')
    cat('\nTrump:', retval$trump, '\n')
    cat('\nCurrent game score:\n',
        paste0('    ', names(retval$score), ': ', retval$score, collapse = '\n'), sep = '')
  }

  invisible(retval)
})


#' Advance to the next trick
#'
#' @description This generic advances the round to the next trick
#' @name next_trick
#' @rdname round-methods
#'
#' @param obj An object of the correct class
#' @param ... Other arguments for specific classes
#'
#' @export
setGeneric("next_trick",
           function(obj, ...) standardGeneric("next_trick"),
           signature = c('obj'))

#' @docType methods
#' @rdname round-methods
setMethod('next_trick', 'Round', function(obj, verbose = TRUE, ...)
{
  # decide who the winner is - they will go first in the next trick
  obj@next_player <- cards(obj@trick)$player[1]

  # move cards to the winner's pile
  cards(obj@won[[obj@teams[obj@next_player]]]) <- cards(obj@trick)

  # reset the trick
  obj@trick <- new('Trick')
  trump(obj@trick) <- obj@trump

  return(obj)
})

#' @docType methods
#' @rdname round-methods
setMethod('next_trick', 'Game', function(obj, verbose = TRUE, ...)
{
  obj@round <- next_trick(obj@round)

  return(obj)
})
