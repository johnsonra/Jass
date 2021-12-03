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
setGeneric('deal',
           function(obj, game = NULL, n = NULL, ...) standardGeneric('deal'),
           signature = c('obj', 'game', 'n'))

setMethod('deal', 'Round', function(obj, game = 'Cross Jass', n = 4, ...)
{
  if(game == 'Cross Jass')
  {
    if(length(obj@hands) != 4)
      stop('Number of players for Cross Jass must be equal to 4.')

    # shuffle cards (9 cards to each player)
    h <- sample(rep(1:4, 9))

    # deal cards to each player
    obj@hands <- purrr::map2(obj@hands, 1:4, function(.x, .y) .x$inhand <- h == .y)
  }

  obj
})


#' Play a card
#'
#' @description This generic plays a valid card based on the current state of the game
#' @name play
#' @rdname round-methods
#'
#' @param obj A Game object
#' @param rules A function to decide which card to play
setGeneric("play",
           function(obj, rules, ...) standardGeneric("play"),
           signature = c('obj', 'rules'))

setMethod('play', 'Game', function(obj, rules = pick_random_valid_card, ...)
{
  player_turn <- obj@round@next_player
  to_play <- rules(obj)

  # move card from player's hand to the trick
  cards(obj@players[[player_turn]]@hand, draw = FALSE) <- to_play
  cards(obj@round@trick@played[[player_turn]]@hand) <- to_play

  # advance play to the next player
  advance_play <- c(2,3,4,1)
  obj@round@next_player <- advance_play[player_turn]

  obj
})
