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
#' 
#' @return A modified object of the same type as `obj`
#' 
#' @export
#' @importFrom purrr map2
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

  invisible(obj)
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

  invisible(obj)
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
#' @param advance_trick A logical. When \code{TRUE} (default) and \code{auto = FALSE}, \code{next_trick()} is called automatically after the fourth card is played. Set to \code{FALSE} to leave the completed trick on the table so it can be inspected before advancing.
#' @param verbose A logical. When TRUE, verbose output is printed.
#' @param ... Additional arguments passed to \code{rules}
#' @export
setGeneric("play",
           function(obj, ...) standardGeneric("play"),
           signature = c('obj'))

#' @docType methods
#' @rdname round-methods
setMethod('play', 'Game', function(obj, to_play = NULL, rules = pick_random_valid_card, state = NULL, auto = is.null(to_play), advance_trick = TRUE, verbose = TRUE, ...)
{
  while(is_ai(obj, obj@round@next_player) |                       # if the next player is an AI keep going
        (!is_ai(obj, obj@round@next_player) & !is.null(to_play))) # if called by a human, to_play should not be NULL - let it run once, will stop if hitting a human after several AI players have played
  {
    player_turn <- obj@round@next_player

    # pick a card if one hasn't been supplied
    if(is.null(to_play))
    {
      to_play <- rules(obj@round@trick, obj@round@hands[[player_turn]], state, ...)
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

    # if auto is FALSE, break after the first time through the loop;
    # but first advance to the next trick if all players have played,
    # so that obj@round@trick is always in a clean state when we return
    if(!auto)
    {
      if(advance_trick && nrow(cards(obj@round@trick)) >= length(obj@players))
        obj <- next_trick(obj)
      break
    }

    # if we are continuing and all players have played, print status and advance to next trick
    if(nrow(cards(obj@round@trick)) >= length(obj@players))
    {
      # capture round scores and last trick before next_trick() moves them
      score_round <- purrr::map_dbl(1:length(obj@round@won),
                                    ~ with(cards(obj@round@won[[.x]]), sum(card_value(face, trump))))
      last_trick <- obj@round@trick

      status(obj, verbose = verbose)
      obj <- next_trick(obj)

      # clean up and break from the loop when the round is over
      if(nrow(cards(obj@round@hands[[1]])) == 0)
      {
        # tally scores for the round
        for(i in 1:length(obj@score))
        {
          obj@score[i] <- as.integer(obj@score[i] + score_round[i])
        }

        # 5-point bonus for winning the last trick + card values from that trick
        won <- obj@teams[cards(last_trick)$player[1]]
        obj@score[won] <- as.integer(obj@score[won] + 5 + with(cards(last_trick), sum(card_value(face, trump))))

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
#' @importFrom dplyr mutate filter
#' @export
setGeneric("status",
           function(obj, verbose = TRUE, ...) standardGeneric("status"),
           signature = c('obj'))

#' @docType methods
#' @rdname round-methods
#' @importFrom utils capture.output
setMethod('status', 'Round', function(obj, verbose = TRUE, ...)
{
  # take care of annoying no visible binding notes
  if(FALSE)
    inhand <- NULL
  
  # Cards on the table              only display cards that are on the table
  trk <- dplyr::mutate(cards(obj@trick), inhand = TRUE) |>
    dplyr::select(-inhand)
  
  if(verbose) # assume this is for human viewing in the console
  {
    trk <- capture.output(print(trk))
  }else{
    trk <- c(paste("Lead suit:", obj@trick@lead_suit),
             paste("Player", trk$player, 'played the', faceTranslation(trk$face), 'of', trk$suit))
  }

  # current score for the round
  scr <- purrr::map_dbl(1:length(obj@won), ~ with(cards(obj@won[[.x]]), sum(card_value(face, trump))))
  names(scr) <- names(obj@won)
  
  if(verbose)
  {
    scr <- capture.output(print(scr))
  }else{
    scr <- paste(names(scr), 'has', scr, 'points in the round')
  }

  retval <- c('Cards on the table:',
              trk,
              paste('\nNext player:', obj@next_player),
              paste('\nTrump suit:', obj@trump),
              '\nCurrent score for the round:',
              scr)

  if(verbose)
  {
    cat(retval, sep = '\n')
  }

  invisible(retval)
})

#' @docType methods
#' @rdname round-methods
#' @importFrom utils capture.output
setMethod('status', 'Game', function(obj, verbose = TRUE, ...)
{
  retval <- status(obj@round, verbose)

  game_score <- obj@score + 
    purrr::map_dbl(1:length(obj@round@won), ~ with(cards(obj@round@won[[.x]]), sum(card_value(face, trump))))

  if(verbose)
  {
    game_score <- capture.output(print(game_score))
  }else{
    game_score <- paste(names(game_score), 'has', game_score, 'points in the round')
  }
  
  game_score <- c('\nCurrent game score:',
                  game_score)

  if(verbose)
  {
    cat(game_score, sep = '\n')
  }
  
  invisible(c(retval, game_score))
})


#' Print round history
#' 
#' @description Prints the tricks that have been played this round
#' @name round_history
#' @rdname round-methods
#' 
#' @param obj An object of the correct class
#' @param ... Other arguments for specific classes
#' @param verbose Logical. Print output to the screen when TRUE
#' 
#' @export
setGeneric("round_history",
           function(obj, ...) standardGeneric("round_history"),
           signature = c('obj'))

#' @docType methods
#' @rdname round-methods
setMethod('round_history', 'Round', function(obj, verbose = TRUE, ...)
{
  # if there is no history, return an empty hand
  if(length(obj@history) == 0)
  {
    if(verbose)
      cat("No current history\n")
    
    return(invisible((new('Hand') |> cards())))
  }
  
  retval <- list()
  
  for(i in 1:length(obj@history))
  {
    retval[[i]] <- cards(obj@history[[i]])
    
    if(verbose)
    {
      cat("Trick ", i, ":\n", sep = '') 
      print(cards(obj@history[[i]]))
    }
  }
  
  invisible(retval)
})

#' @docType methods
#' @rdname round-methods
setMethod('round_history', 'Game', function(obj, ...)
{
  retval <- round_history(obj@round, ...)
  
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
#' @param verbose Logical. Print output to the screen when TRUE
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

  # save history
  obj@history[[length(obj@history) + 1]] <- obj@trick
  
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
