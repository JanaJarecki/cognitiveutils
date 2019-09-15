#' Apply soft maximum (softmax) choice rule for binary predictions
#'
#' @param x A numeric vector or matrix with probabilistic predictions for actions. If \code{x} is a vector, binary actions are assumed. If \code{x} is a matrix, column 1 holds predictions for action 1, column 2 for action 2, etc. \code{x} can also be a value difference such as \code{x = price - utility}.
#' @param tau A number above 0 making action selection more random (aka temperature parameter). Large values make actions equiprobable, small values close to zero generate deterministic choices, close to arg max choices or Softmax-greedy choices.
#' @return A matrix holding the probability to select each action in a column
#' @examples
#' # No examples
#' @references
#' Sutton, R. S., & Barto, A. G. (1998). Reinforcement learning: An introduction. Cambridge, MA: MIT Press
#' @export
cr_softmax <-function(x, tau) {
  x <- as.matrix(x)
  if ( ncol(x) == 1 ) {
    return(cr_softmax(cbind(x, 1-x), tau)[,1])
  }
  x[x==0] <- .0000001 # ensure no NAs
  # For numerical stability, normalize to avoid instabilities due to large exponentials, see http://cs231n.github.io/linear-classify/#softmax
  s <- apply(x, 1, max) # shift (exp is shift invariant)
  y <- exp(((x - s)/tau))
  return( y / rowSums(y) )
}