#' Classification Accuracy
#' \code{Accuracy()} computes the percent identical values, controlling for perfect chance-level
#' @import stats
#' @inheritParams gof
#' @inheritParams APE
#' @return The percent of accurate predictions between 0 and 1, inclusive, where 1 indicates 100% accuracy.
#' @details
#' Note: Accuracy corrects for perfect chance. Perfect chance predictions for discrete responses (e.g. 0.50 for binary responses) yield chance-level accuracy (50 %), rather than zero accuracy.
#' @family goodness of fit functions
#' @examples
#' Accuracy(obs = c(1,0,0), pred = c(1,1,1))        # % accuracy
#' @export
Accuracy <- function(obs, pred, na.rm = FALSE, response = c('discrete', 'continuous'), ...) {
  response <- match.arg(response)
  if (response == "discrete") {
    pred <- cogsciutils::cr_argmax(pred)
  }
  return(mean((1 - abs(as.matrix(obs) - as.matrix(pred)))[,1]))
}
