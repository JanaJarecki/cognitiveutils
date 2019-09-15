#' \code{RMSE()} computes the root mean squared error
#' @import stats
#' @inheritParams SSE
#' @return Root mean squared deviations between \code{obs} and \code{pred}
#' @family goodness of fit functions
#' @examples
#' # None so far

#' @export
RMSE <- function(obs, pred, na.rm = FALSE, weighted = FALSE, n = NULL, ...) {
  .args <-  as.list(match.call()[-1])
  return ( sqrt( do.call(MSE, .args) ) )
}