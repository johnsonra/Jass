# Cross_Jass.R

# score

#' Pick a random, valid card to play
#'
#' @description This is simplest function to pick a card to play. It picks a random following the basic Jass rules.
#' @name pick_random_valid_card
#' @rdname Cross-Jass
#'
#' @param trk A Trick object
#' @param h A Hand object
#' @return A character value of the card to play
#' @importFrom dplyr bind_rows
#' @importFrom rlang .data
pick_random_valid_card <- function(trk, h)
{
  if(trk@lead_suit == '')
  {
    cards(h) %>%
      card_abbr() %>%
      sample(size = 1) %>%
      return()
  }else{
    # any cards with lead_suit?
    valid <- filter(cards(h), .data$suit == trk@lead_suit)

    # if no cards from the lead suit, you can play anything
    if(dim(valid)[1] == 0)
      valid <- cards(h)

    # you can always play trump
    valid <- filter(cards(h), .data$trump) %>%
      bind_rows(valid) %>%
      unique()

    card_abbr(valid) %>%
      sample(size = 1) %>%
      return()
  }
}
