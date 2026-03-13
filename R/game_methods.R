# game_methods.R

#' Check if a player is an AI
#'
#' @description Checks if a given player is played by an AI algorithm.
#' @name is_ai
#' @rdname game-methods
#'
#' @param obj A Game object
#' @param player Integer or player name identifying the player to check
#' @param ... Additional objects
#' @export
setGeneric("is_ai",
           function(obj, ...) standardGeneric("is_ai"),
           signature = c('obj'))

#' @docType methods
#' @rdname game-methods
setMethod('is_ai', 'Game', function(obj, player, ...)
{
  return(obj@players[[player]]@AI)
})

#' Flag a player as human
#'
#' @description Flag the indicated player as human.
#' @name set_human
#' @rdname game-methods
#'
#' @param obj A Game object
#' @param player Integer or player name identifying the players who will be played by a human
#' @export
setGeneric("set_human",
           function(obj, ...) standardGeneric("set_human"),
           signature = c('obj'))

#' @docType methods
#' @rdname game-methods
setMethod('set_human', 'Game', function(obj, player, ...)
{
  for(i in player)
  {
    obj@players[[i]]@AI <- FALSE
  }

  return(obj)
})
