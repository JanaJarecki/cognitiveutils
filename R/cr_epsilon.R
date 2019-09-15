#' Apply Epsilon greedy choice rule
#'
#' @param x A numeric vector or matrix with probabilistic predictions for actions. If \code{x} is a vector, binary actions are assumed. If \code{x} is a matrix, column 1 holds predictions for action 1, column 2 for action 2, etc.
#' @param eps A number between 0 an 1: the probability to make random choices across all options.
#' @return A matrix holding the probability to select each action in the columns
#' @examples
#' # No examples
#' @references
#' Sutton, R. S., & Barto, A. G. (1998). Reinforcement learning: An introduction. Cambridge, MA: MIT Press
#' @export
cr_epsilon <- function(x, eps) {
  x <- as.matrix(x)
  if (any(eps < 0L) | any(eps > 1L)) {
    stop("'eps' must lie in [0 - 1], but is ", eps, ".")
  }
  if (ncol(x) == 1) {
    return(cr_epsilon(cbind(x, 1-x), eps = eps)[, 1L])
  }

  if ( !isTRUE(all.equal(rep(1, nrow(x)), rowSums(x))) ) {
    id <- which(apply(x, 1,  function(z) !isTRUE(all.equal(1, sum(z)))))
    stop("The choicerule epsilon works on probabilities, but the input rows ", .brackify(id), " are no probabilities because they don't sum to 1. -> Check the input x to the choicerule.")
  }
  return(x * (1 - eps)  + eps / ncol(x))
}