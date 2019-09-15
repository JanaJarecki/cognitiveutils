#' Compute goodness of fit measures
#' 
#' @rdname gof
#' @examples
#' # Example from Busemeyer and Diederich (2010)
#' # Observed relative frequencies of binary choices
#' obs <- c(0.9538, 0.9107, 0.9204, 0.9029, 0.8515, 0.9197,
#'           0.7970, 0.8228, 0.8191, 0.7277, 0.7276)
#' # Predictions for each of the 11 conditions
#' pred <- c(.9526, .9168, .8721, .8229, .7736, .7277,
#'           .6871, .6523, .6232, .5993, .5798)
#' 
#' \donttest{
#' #
#' # GOF from aggregated data over 200 observations
#' # --------------------------------------------------------------------------
#' gof_discrete(obs, pred, "sse", n=200)   # SSE  (paper: 0.1695)
#' gof_discrete(obs, pred, "wsse", n=200)  # Weighted SSE (paper: 158.4059)
#' gof_discrete(obs, pred, "logl", n=200)  # Loglik. (paper: -969.9514 < 0.1% diff)
#' gof_discrete(obs, obs, "loglik", n=200) # Saturated LL (paper: -879.9013)
#' # 
#' # GOF from raw data
#' # --------------------------------------------------------------------------
#' # Recreate the raw data (observations 0 or 1)
#' n <- 200 # number of observations
#' obsraw  <- rep(rep(0:1, 11), round(c(t(cbind(1-obs, obs))) * n))
#' predraw <- rep(pred, each = n)
#' gof_discrete(obsraw, predraw, "acc")          # 85% Accuracy
#' gof_discrete(obsraw, predraw, "sse")          # SSE (not useful w/ raw data)
#' gof_discrete(obsraw, predraw, "log")          # Loglik (paper: -969.9514)
#' gof_discrete(obs, obsraw, "loglik", saturated=TRUE, n=200) # Saturated LL (paper: -879.9013)
#' }
#' @export
gof_continuous <- function(obs, pred, type, ...) {
  .args <- c(as.list(match.call()[-1]), list(options = list(response = 'continuous')))
  do.call(gof, .args)
}

#gof_continuous(cbind(1,.5), cbind(.7), n=1)

# .format_obs_pred_n(obs=cbind(1,.5), pred=.7, n=1, na.rm=F)