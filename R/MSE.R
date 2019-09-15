#' \code{MSE()} computes the mean squared error
#' @import stats
#' @inheritParams SSE
#' @param ... other arguments are ignored
#' @return Mean of squared deviations between \code{obs} and \code{pred}
#' @family goodness of fit functions
#' @examples
#' # None so far

#' @export
MSE <- function(obs, pred, na.rm = FALSE, weighted = FALSE, n = NULL,  ...) {
  .args <-  c(list(obs, pred, na.rm, weighted, n), list(...))
  n <- .format_obs_pred_n(obs, pred, n, na.rm)[['n']]
  n <- sum(n)
  return ( do.call(SSE, .args) / n )
}