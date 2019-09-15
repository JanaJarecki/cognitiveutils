#' Apply Luce choie rule, aka Clarke choice rule
#'
#' @param x A numeric vector or matrix with probabilistic predictions for actions. If \code{x} is a vector, binary actions are assumed. If \code{x} is a matrix, column 1 holds predictions for action 1, column 2 for action 2, etc.
#' @return A matrix holding the probability to select each action in a column
#' @examples
#' # No examples
#' @references
#' Luce, R. D. (1959). Individual Choice Behavior: A Theoretical Analysis. John Wiley and sons. New York.
#' Clarke, F. R. (1957). Constant‐ratio rule for confusion matrices in speech communication. The Journal of the Acoustical Society of America, 29(6), 715-720.
#' @export
cr_luce <-function(x) {
  if (is.null(dim(x))) {
    x <- as.matrix(x)
  }
  out <- x / rowSums(x)
  out[rowSums(x) == 0] <- 0.5
  return(drop(out))
}