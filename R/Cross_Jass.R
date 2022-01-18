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
#' @param ... Other arguments that are ignored by this function
#' @return A character value of the card to play
#' @importFrom dplyr bind_rows
#' @importFrom rlang .data
#' @export
pick_random_valid_card <- function(trk, h, ...)
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
#' @export
pick_random_trump <- function(g, p, strategy = 'Max points', ...)
{
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
