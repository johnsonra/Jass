# hand_methods.R
# methods for building and modifying hands

#' card_abbr
#'
#' @description Abbreviate cards names in a hand.
#' @name cards
#' @rdname hand-methods
#'
#' @param cards The cards slot from a hand object (or a subset)
#' @return A string of abbreviated card names
card_abbr <- function(cards)
{
  with(cards, paste0(substr(suit, 1, 1), face))
}

#' Display cards in a Jass Hand
#'
#' @description This generic will display a hand of Jass cards
#' @name cards
#' @rdname hand-methods
#'
#' @param obj An object of the proper class
#'
#' @importFrom dplyr filter
setGeneric("cards",
           function(obj, ...) standardGeneric("cards"),
           signature = c('obj'))

setMethod('cards', 'Hand', function(obj, ...)
{
  inhand <- NULL # avoid no visible binding error for `inhand` columns of `x`

  dplyr::filter(obj@cards, inhand)
})


#' Draw/discard cards
#'
#' @description This generic will add cards to or discard cards from a hand of Jass cards
#' @name cards<-
#' @rdname hand-methods
#'
#' @param x An object of the proper class
#' @param ... Other values used in specific methods (see Description)
#' @param value A object with cards to add to the hand
#'
#' @description Option arguments include: `draw`, which by default adds cards to the hand, but for hand objects causes the specified cards to be discarded by setting `draw` equal to FALSE.
setGeneric("cards<-",
           function(x, ..., value) standardGeneric("cards<-"),
           signature = c('x', 'value'))

setMethod('cards<-', 'Hand', function(x, draw = TRUE, ..., value)
{
  suit <- face <- NULL # avoid no visible binding error for `face` and `suit` columns of `x`

  # character vector
  if(class(value)[1] == 'character')
  {
    myCards <- card_abbr(x@cards) %in% value
  }

  # data.frame
  if('data.frame' %in% class(value))
  {
    myCards <- card_abbr(x@cards) %in% card_abbr(value)
  }

  # Hand
  if('Hand' %in% class(value))
  {
    myCards <- card_abbr(x@cards) %in% card_abbr(value@cards)
  }

  # check that the cards added/discarded are valid
  # (i.e. can't add cards that are already in the hand, can't discard cards that aren't in the hand)

  # draw/discard cards
  x@cards$inhand[myCards] <- draw

  # make sure hand is still valid

  x
})


#' Set trump
#'
#' @description This generic sets Trump for the object and any hands contained in the object
#' @name trump<-
#' @rdname hand-methods
#'
#' @param obj An object of the proper class
#' @param value A character string containing the trump suit
setGeneric("trump<-",
           function(x, value) standardGeneric("trump<-"),
           signature = c('x', 'value'))

setMethod('trump<-', 'Hand', function(x, value)
{
  suitTranslation <- c(Bells = 'Bells', B = 'Bells',
                       Flowers = 'Flowers', `F` = 'Flowers',
                       Shields = 'Shields', S = 'Shields',
                       Acorns = 'Acorns', A = 'Acornds')

  # validate input
  if(length(value) != 1 | !(value %in% names(suitTranslation)))
    stop('Bad suit.')

  # set trump
  x@cards$trump <- x@cards$suit == suitTranslation[value]

  # validate hand

  x
})


#' Display trump
#'
#' @description This generic gets Trump for the object and any hands contained in the object
#' @name trump
#' @rdname hand-methods
#'
#' @param obj An object of the proper class
#' @importFrom magrittr %>%
#' @importFrom dplyr select
setGeneric("trump",
           function(obj, ...) standardGeneric("trump"),
           signature = c('obj'))

setMethod('trump', 'Hand', function(obj, ...)
{
  suit <- NULL # avoid no visible binding error for `suit` column of `x`

  # set trump
  filter(obj@cards, trump) %>%
    dplyr::select(suit) %>%
    unique() %>%
    unlist()
})
