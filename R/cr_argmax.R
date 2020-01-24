#' Apply an arg max choice rule
#'
#' @param x A numeric vector or matrix with probabilistic predictions for actions. If \code{x} is a vector, binary actions are assumed. If \code{x} is a matrix, column 1 holds predictions for action 1, column 2 for action 2, etc.
#' @return A numeric vector or matrix (depending on the input) of indicators of the maximum.
#' @examples
#' # No examples
#' @references
#' Sutton, R. S., & Barto, A. G. (1998). Reinforcement learning: An introduction. Cambridge, MA: MIT Press
#' @details The returned values are typically 1 for the row maximum or 0, except if n actions have the maximum probability the result is 1/n, correcting ties by equal choice probability. If the iput is a vector it is silently coerced to a two-column matrix.
#' @examples
#' cr_argmax(c(0.1, 0.5, 0.8))                    # Returns a vector
#' cr_argmax(cbind(c(0.1, 0.9), c(0.5, 0.4)))      # Returns a matrix
#' @export
cr_argmax <-function(x, tol = sqrt(.Machine$double.eps)) {
  x <- as.matrix(x)
  if ( ncol(x) == 1 ) {
    return(cr_argmax(cbind(x, 1-x))[, 1])
  }
  x[x==0] <- tol
  x[] <- t(apply(x, 1, function(z) ((max(z) - z) < tol) / sum((max(z) - z) < tol)))
  return(x)
}