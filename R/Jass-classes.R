#' S4 classes for Jass
#' 
#' @description This contains a description of classes used in the Jass package
#' 
#' @name Hand
#' @rdname Jass-classes
#' @slot cards A data.frame with 36 rows and the following columns: suit, face, trump (logical), and inhand (logical, indicating whether the given card is in the current hand)
#' @import methods
setClass("Hand",
         representation(cards = 'data.frame'),

         # empty hand
         prototype(cards = data.frame(suit = rep(c('Schellen', 'Rosen', 'Schilten', 'Eichel'), each = 9),
                                      face = rep(c('A', 'K', 'O', 'U', 'B', 9:6), 4),
                                      trump = FALSE,
                                      inhand = FALSE)))


#' @name Player
#' @rdname Jass-classes
#' @slot name Player name (character)
#' @slot hand The player's hand
#' @slot AI Logical indicating an AI player
setClass("Player",
         representation(name = 'character',
                        hand = 'Hand',
                        AI = 'logical'),
         prototype(name = 'Player',
                   hand = new('Hand'),
                   AI = TRUE))


#' @name Trick
#' @rdname Jass-classes
#' @slot hands A list of Jass hands, each containing 1 card. Cards are placed in the list in the order they are played.
setClass("Trick",
         representation(hands = 'list'),
         prototype(hands = list()))