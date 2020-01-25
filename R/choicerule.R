#' Apply a choice rule (action selection rule) to probabilistic predictions.
#'
#' @param x A numeric vector or matrix with probabilistic predictions for actions. If \code{x} is a vector, binary actions are assumed. If \code{x} is a matrix, column 1 holds predictions for action 1, column 2 for action 2, etc. \code{0 <= x <= 1} is required.
#' @param type The choice rule (action selection rule), can be one of \code{c("luce", "argmax", "softmax", "epsilon")}, abbreviations are allowed. \code{luce} is Luce's choice rule, \code{argmax} is an arg max choice rule, \code{softmax} is a soft maximum, \code{epsilon} is an epsilon-greedy rule assuming a constant error
#' @param tolerance **optional** A small number. Usually all rows in the predictions need to sum up to 1 if predictions contain more than 1 column. A \code{tolerance} allows the rows to sum up to 1 - tolerance, should be small.
#' @param ... Other parameters to be passed to the choice rule methods: \code{softmax} needs a parameter \code{tau}, \code{epsilon} needs a parameter \code{eps}.
#' @return A matrix holding the probability to select each action in the columns.
#' @references See the individual functions.
#' @examples
#' # Some predictions
#' predictions <- cbind(A = c(.1,.5,.4), B = c(.3,.1,.4), C = c(.6, .4, .2))
#' # Apply various choice rules
#' \donttest{
#' choicerule(predictions, "luce")
#' choicerule(predictions, "argmax")
#' choicerule(predictions, "epsilon", eps = .2)
#' 
#' # Some binary predictions
#' binaryPredictions <- c(.22, .5, .73)
#' # Apply a choice rule
#' choicerule(binaryPredictions, "softmax", tau = 2)
#' }
#' @export
choicerule <- function(x, type = c("softmax", "luce", "argmax", "epsilon"), tolerance = sqrt(.Machine$double.eps), ...) {
  # if(any(is.na(x))) {
  #   ("Argument x (predictions) cannot contain NA values, but has NAs.")
  # }
  x <- as.matrix(x)
  nas <- is.na(x)
  x[nas] <- 0L
  
  # if ( any(x < 0 | x > 1) ) {
  #   stop("Argument x (predictions) needs to lie in [0, 1], but has: ", paste0(head(x[x>1|x<0]), collapse = ", "))
  # }

  if (ncol(x) == 1) {
    x <- cbind(x, 1 - x)
  }

  # if (!isTRUE(all.equal(rowSums(x), rep(1, nrow(x)), tolerance = tolerance, check.attributes = FALSE))) {
  #   stop("Argument x (predictions) need to sum to 1 in each row.\n  You can set the parameter 'tolerance' to allow for a tolerance.")
  # }

  type <- match.arg(type)
  args <- list(x = x, ...)
  out <- switch(type,
        luce =    do.call(cr_luce, args),
        softmax = do.call(cr_softmax, args),
        argmax  = do.call(cr_argmax, args),
        epsilon = do.call(cr_epsilon, args),
        )

  if (any(is.na(out) | out == -Inf | out == Inf)) {
    stop("There are NAs or Inf values in your prediction, this is not allowed.")
  }
  out[nas] <- NA
  return(drop2(out))
}