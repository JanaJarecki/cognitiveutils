#' Sum of squared errors
#' 
#' \code{SSE()} computes the sum of squared errors.
#' @import stats
#' @inheritParams gof
#' @param weighted (optional) Logical (dfault \code{FALSE}), if \code{TRUE} variance-weighted sums of squares are calculated, otherwise unweighted sums of squares.
#' @param n (optional), number of integer vector, required if \code{weighted = TRUE}. Number of observations underlying each entry in \code{obs}. One number if each entry in \code{obs} is the aggregate of the same number, a vector if different numbers of observations underly entries in code{obs}.
#' @param ... Other parameters to be passed to the methods
#' @return Sum of squared deviations between \code{obs} and \code{pred}, or weighted sum of squared deviations.
#' @family goodness of fit functions
#' @examples
#' # Example from Busemeyer & Diederich (2010), p. 55 - 56
#' # Observed relative frequencies of choices as proportions of choice 1, split up by 11 conditions
#' obs <- c(.9538, .9107, .9204, .9029, .8515, .9197,
#'           .7970, .8228, .8191, .7277, .7276)
#' # Predictions for each of the 11 conditions
#' pred <- c(.9526, .9168, .8721, .8229, .7736, .7277,
#'           .6871, .6523, .6232, .5993, .5798)
#' n <- 200 # all  or n = rep(200, 11) # number of observations per condition
#' 
#' sse <- SSE(obs = obs, pred = pred)
#' # Sum of squared errors (SSE) in paper equals 0.1695
#' # all.equal(sse, 0.1695, 0.001) # TRUE
#' 
#' wsse <- SSE(obs = obs, pred = cbind(pred, 1-pred), weighted = TRUE, n = 200)
#' # Weighted sum of squared errors (WSSE) in paper equals 158.4059
#' # all.equal(wsse, 158.4059, 0.001) TRUE
#' # minute difference is due to rounding
#' @references Busemeyer, J. R., & Diederich, A. (2010). Nonlinear parameter estimation. In Cognitive Modeling (pp. 43–84). Thousand Oaks, CAL: SAGE Publications.
#' @export
SSE <- function(obs, pred, na.rm = FALSE, weighted = FALSE, n = NULL, ...) {
 if( is.null(n) ) {
   if (weighted) {
      stop('Argument n is missing in weighted sum of squares.')
    }
  }

  TMP <- .format_obs_pred_n(obs, pred, n, na.rm = na.rm)
  obs <- TMP[['obs']]
  pred <- TMP[['pred']]
  n <- TMP[['n']]

  # Todo: extend this to more than two responses
  if (ncol(obs) > 2 | ncol(pred) > 2) {
    stop('SSE not yet implemented for > 2 columns in obs or pred.')
  }

  se <- (obs[, 1] - pred[, 1])^2
  if (weighted) {
    var <- (pred[, 1] * (1-pred[, 1])) / n
    return(sum(se / var))
  } else {
    return(sum(se))
  }
}
