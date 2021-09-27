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


#' @name Player
#' @rdname Jass-classes
#' @slot name Player name (character)
#' @slot hand The player's hand
#' @slot AI Logical indicating an AI player
setClass("Player",
         contains = 'Hand',
         slots = c(name = 'character',
                   hand = 'Hand',
                   AI = 'logical'),
         prototype = list(name = 'Player',
                          hand = new('Hand'),
                          AI = TRUE)
         )


#' @name Trick
#' @rdname Jass-classes
#' @slot played A list of Jass hands, each containing 1 card. Cards are placed in the list in player order
#' @slot lead_suit A string identifying the suit for this trick - can be overridden by Trump
setClass("Trick",
         contains = 'Player',
         slots = c(played = 'list',
                   lead_suit = 'character'),
         prototype = list(played = list(new('Player'), new('Player'), new('Player'), new('Player')),
                          lead_suit = character())
         )


#' @name Round
#' @rdname Jass-classes
#' @slot trump A string listing the suit for this round
#' @slot trick A trick object to keep track of cards on the table
#' @slot next_player An integer designating which player's turn is next
#' @slot played A list of hands containing cards won in previous tricks (one for each player or team)
setClass("Round",
         contains = 'Trick',
         slots = c(trump = 'character',
                   trick = 'Trick',
                   next_player = 'integer',
                   played = 'list'),
         prototype = list(trump = character(),
                          trick = new('Trick'),
                          next_player = as.integer(1),
                          played = list(new('Hand'), new('Hand')))
         )


#' @name Game
#' @rdname Jass-classes
#' @slot players A list of Jass Player objects, one for each player
#' @slot game A character string defining what Jass variant is being played
#' @slot round A Round object containing hands
#' @slot score An integer vector to keep track of the score for the game
setClass("Game",
         contains = 'Round',
         slots = c(players = 'list',
                   game = 'character',
                   round = 'Round',
                   score = 'integer'),
         prototype = list(players = list(new('Player'), new('Player'), new('Player'), new('Player')),
                          game = 'Cross Jass',
                          round = new('Round'),
                          score = as.integer(c(0,0)))
         )
