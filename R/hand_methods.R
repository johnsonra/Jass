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

#' suitTranslation
#'
#' @description Translate between suit names and abbreviations
#' @name suitTranslation
#' @rdname hand-methods
#'
#' @param suit Character string naming suit or suit abbreviation
#' @return
suitTranslation <- function(suit)
{
  c(Bells = 'Bells', B = 'Bells',
    Flowers = 'Flowers', `F` = 'Flowers',
    Shields = 'Shields', S = 'Shields',
    Acorns = 'Acorns', A = 'Acornds')[suit]
}

#' card order
#'
#' @description Return the order of cards for trump/non-trump suits
#' @name card_order
#' @rdname hand-methods
#'
#' @param face Character (or character vector) indicating the face to order
#' @param trump logical indicating that trump ordering should be used
#' @param game Character indicating the game being played. Most result in identical ordering.
#' @return Returns a number (or numeric vector) indicating card order (1 being first, 9 being last)
card_order <- function(face, trump, game = 'Cross Jass')
{
  if(game %in% c('Cross Jass'))
  {
    trump_order <- 1:9
    names(trump_order) <- c('U', 9, 'A', 'K', 'O', 'B', 8, 7, 6)

    regular_order <- 1:9
    names(regular_order) <- c('A', 'K', 'O', 'U', 'B', 9, 8, 7, 6)
  }else{
    stop(paste(game, 'not yet implemented in card_order'))
  }

  return(ifelse(trump, trump_order[face], regular_order[face]))
}

#' card value
#'
#' @description Return the point value of cards for trump/non-trump suits
#' @name card_value
#' @rdname hand-methods
#'
#' @param face Character (or character vector) indicating the face to order
#' @param trump logical indicating that trump ordering should be used
#' @param game Character indicating the game being played. Most result in identical ordering.
#' @return Returns a number (or numeric vector) indicating card point value(s)
card_value <- function(face, trump, game = 'Cross Jass')
{
  if(game %in% c('Cross Jass'))
  {
    trump_values <- c(20, 14, 11, 4, 3, 10, rep(0, 3))
    names(trump_values) <- c('U', 9, 'A', 'K', 'O', 'B', 8, 7, 6)

    regular_values <- c(11, 4, 3, 2, 10, rep(0, 4))
    names(regular_values) <- c('A', 'K', 'O', 'U', 'B', 9, 8, 7, 6)
  }else{
    stop(paste(game, 'not yet implemented in card_order'))
  }

  return(ifelse(trump, trump_values[face], regular_values[face]))
}

#' Display cards in a Jass Hand
#'
#' @description This generic will display a hand of Jass cards
#' @name cards
#' @rdname hand-methods
#'
#' @param obj An object of the proper class
#' @param player An integer indicating the player who's hand should be displayed (ignored when not needed)
#' @param lead_suit A character defining the suit that was lead in the current trick (ignored when NULL)
#'
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr select
#' @export
setGeneric("cards",
           function(obj, ...) standardGeneric("cards"),
           signature = c('obj'))

#' @export
#' @docType methods
#' @rdname hand-methods
setMethod('cards', 'Hand', function(obj, lead_suit = NULL, ...)
{
  # hack to avoid "no visible binding" note for columns of obj@cards
  inhand <- NULL
  face <- NULL
  suit <- NULL

  # flag the suit that was lead for this trick
  if(!is.null(lead_suit))
  {
    obj@cards$lead_suit <- obj@cards$suit == suitTranslation(lead_suit)
  }else{
    obj@cards$lead_suit <- FALSE
  }

  # get face rank for trump/non-trump cards
  obj@cards <- dplyr::mutate(obj@cards,
                             rank = card_order(face, trump))

  # filter by inhand and return in rank order
  dplyr::filter(obj@cards, inhand) %>%
    dplyr::arrange(dplyr::desc(trump), lead_suit, suit, rank) %>%
    dplyr::select(-inhand, -rank)
})

#' @docType methods
#' @rdname hand-methods
#' @importFrom purrr map_dfr
setMethod('cards', 'Trick', function(obj, ...)
{
  # gather all cards played by player
  jointHand <- purrr::map_dfr(1:length(obj@played), ~ cards(obj@played[[.x]], obj@lead_suit) %>%
                                                      dplyr::mutate(player = .x)) %>%
    mutate(inhand = TRUE)

  # display cards on the table
  new('Hand', cards = jointHand) %>%
    cards(obj@lead_suit)
})

#' @docType methods
#' @rdname hand-methods
setMethod('cards', 'Round', function(obj, player = 1, ...)
{
  cards(obj@hands[[player]], obj@trick@lead_suit)
})

#' @docType methods
#' @rdname hand-methods
setMethod('cards', 'Game', function(obj, player = 1, ...)
{
  cards(obj@round, player)
})

#' Draw/discard cards
#'
#' @description This generic will add cards to or discard cards from a hand of Jass cards
#' @name cards<-
#' @rdname hand-methods
#'
#' @param x An object of the proper class
#' @param draw Logical - adds cards to the hand when TRUE, but for hand objects causes the specified cards to be discarded by setting `draw` equal to FALSE.
#' @param ... Other values used in specific methods (see Description)
#' @param value A object with cards to add to the hand
#'
#' @export
setGeneric("cards<-",
           function(x, ..., value) standardGeneric("cards<-"),
           signature = c('x', 'value'))

#' @docType methods
#' @rdname hand-methods
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
#' @param x An object of the proper class
#' @param value A character string containing the trump suit
#' @export
setGeneric("trump<-",
           function(x, value) standardGeneric("trump<-"),
           signature = c('x', 'value'))

#' @docType methods
#' @rdname hand-methods
setMethod('trump<-', 'Hand', function(x, value)
{
  # validate input
  if(length(value) != 1 | is.na(suitTranslation(value)))
    stop('Bad suit.')

  # set trump
  x@cards$trump <- x@cards$suit == suitTranslation(value)

  # validate hand

  x
})

#' @docType methods
#' @rdname hand-methods
setMethod('trump<-', 'Round', function(x, value)
{
  x@trump <- suitTranslation(value)

  # flag trump in each hand
  for(i in 1:length(x@hands))
  {
    trump(x@hands[[i]]) <- value
  }

  # flag trump in the trick object
  for(i in 1:length(x@trick@played))
  {
    trump(x@trick@played[[i]]) <- value
  }

  # flag trump in the hand of cards won
  for(i in 1:length(x@won))
  {
    trump(x@won[[i]]) <- value
  }

  x
})

#' @docType methods
#' @rdname hand-methods
setMethod('trump<-', 'Game', function(x, value)
{
  trump(x@round) <- value

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
#' @export
setGeneric("trump",
           function(obj, ...) standardGeneric("trump"),
           signature = c('obj'))

#' @docType methods
#' @rdname hand-methods
setMethod('trump', 'Hand', function(obj, ...)
{
  suit <- NULL # avoid no visible binding error for `suit` column of `x`

  # set trump
  filter(obj@cards, trump) %>%
    dplyr::select(suit) %>%
    unique() %>%
    unlist()
})
