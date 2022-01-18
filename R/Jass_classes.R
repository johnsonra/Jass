#' S4 classes for Jass
#'
#' @description This contains a description of classes used in the Jass package
#'
#' @name Hand
#' @rdname Jass-classes
#' @slot cards A data.frame with 36 rows and the following columns: suit, face, trump (logical), and inhand (logical, indicating whether the given card is in the current hand)
#' @import methods
setClass("Hand",
         slots = c(cards = 'data.frame'),
         prototype = list(cards = data.frame(suit = rep(c('Bells', 'Flowers', 'Shields', 'Acorns'), each = 9),
                          face = rep(c('A', 'K', 'O', 'U', 'B', 9:6), 4),
                          trump = FALSE,
                          inhand = FALSE)))
# setValidity("Hand", function(object)
# {
#   if(<fail>)
#   {
#     return('string with failed message')
#   }else{
#     return(TRUE)
#   }
# })


#' @name Player
#' @rdname Jass-classes
#' @slot name Player name (character)
#' @slot AI Logical indicating an AI player
setClass("Player",
         slots = c(name = 'character',
                   AI = 'logical'),
         prototype = list(name = 'Player',
                          AI = TRUE)
         )


#' @name Trick
#' @rdname Jass-classes
#' @slot played A list of Jass hands, each containing 1 card. Cards are placed in the list in player order
#' @slot lead_suit A string identifying the suit for this trick - can be overridden by Trump
setClass("Trick",
         slots = c(played = 'list',
                   lead_suit = 'character'),
         prototype = list(played = list(new('Hand'), new('Hand'), new('Hand'), new('Hand')),
                          lead_suit = '')
         )

#' @name Round
#' @rdname Jass-classes
#' @slot trump A string listing the suit for this round
#' @slot trick A trick object to keep track of cards on the table
#' @slot next_player An integer designating which player's turn is next
#' @slot hands A list of hands containing hand for each player this round
#' @slot teams A vector of team names identifying the team for each player
#' @slot won A list of hands containing cards won in previous tricks (one for each player/team)
setClass("Round",
         slots = c(trump = 'character',
                   trick = 'Trick',
                   next_player = 'integer',
                   hands = 'list',
                   teams = 'character',
                   won = 'list'
                   ),
         prototype = list(trump = character(),
                          trick = new('Trick'),
                          next_player = as.integer(1),
                          hands = list(new('Hand'), new('Hand'), new('Hand'), new('Hand')),
                          teams = c('Team 1', 'Team 2', 'Team 1', 'Team 2'),
                          won = list(`Team 1` = new('Hand'), `Team 2` = new('Hand')))
         )


#' @name Game
#' @rdname Jass-classes
#' @slot players A list of Jass Player objects, one for each player
#' @slot game A character string defining what Jass variant is being played
#' @slot round A Round object containing hands
#' @slot teams A vector of team names identifying the team for each player
#' @slot score An integer vector to keep track of the score for the game
#' @slot start An integer indicating which player will start the current round (this is incremented when `deal()` is called on a Game object).
setClass("Game",
         slots = c(players = 'list',
                   game = 'character',
                   round = 'Round',
                   teams = 'character',
                   score = 'integer',
                   start = 'integer'),
         prototype = list(players = list(new('Player'), new('Player'), new('Player'), new('Player')),
                          game = 'Cross Jass',
                          round = new('Round'),
                          teams = c('Team 1', 'Team 2', 'Team 1', 'Team 2'),
                          score = c(`Team 1` = as.integer(0), `Team 2` = as.integer(0)),
                          start = as.integer(0))
         )
