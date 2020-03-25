#' Log likelihood (log loss)
#'
#' @import stats
#' @import utils
#' @importFrom truncnorm dtruncnorm
#' @importFrom rlang call_standardise
#' @inheritParams gof
#' @param pdf A string, probability density function;, allowed are \code{"binomial"}, \code{"normal"}, \code{"truncnormal"}. When using \code{"normal"} then \code{pred} needs two columns: observed means and observed standard deviations.
#' @param saturated (optional) logical, compute saturated model (observed proportions as prediction, which approximates the highest possible likelihood). If so \code{obs} must be raw data, and \code{pred} can be omitted. Equal to \code{obs} being raw and \code{pred} being proportions.
#' @param sigma (optional) Standard deviation of the normal distribution, \bold{required} for \code{pdf = "normal", "truncnormal"}).
#' @param a,b (optional) A number, lower/upper bound of the truncated pdf, required if \code{pdf = "truncnormal"}.
#' @param binomial.coef (optional) Logical (defaul is \code{FALSE}). If \code{TRUE} the binomial coefficient is used with \code{pdf="binomial"}. Mimicks results from \code{\link{dbinom}}. Ignored for other \code{pdf}s.
#' @param eps (optional) small numeric to offset predictions from zero in  binomial pdf by substituting \code{log(eps)} for \code{log(0) = -Inf}. Don't change.
#' @param ... Other parameters, currently ignored.
#' @return The log likelihood of \code{obs} given  \code{pred}. Lokelihoods are computed without the binomial coefficient.
#' @examples
#' \donttest{
#' # Example from Busemeyer & Diederich (2010)
#' # Observed relative frequencies of choice 1 from 11 conditions
#' obs <- c(.9538, .9107, .9204, .9029, .8515, .9197,
#'           .7970, .8228, .8191, .7277, .7276)
#' # Predictions for each of the 11 conditions
#' pred <- c(.9526, .9168, .8721, .8229, .7736, .7277,
#'           .6871, .6523, .6232, .5993, .5798)
#' n <- 200 # number of observations
#' # lnL in paper is -969.9514, < 0.1 % deviation
#' ll <- Loglikelihood(obs = obs, pred = pred, n = 200)
#' # all.equal(ll, -969.9514, 0.001)
#' # lnLS in paper is -879.9013, < 1 % deviation due to rounding
#' llsat <- Loglikelihood(obs = obs, n = 200, saturated = TRUE)
#' # all.equal(llsat, -879.9013, .01)
#'
#' # Using the raw data
#' # Recreate the raw data (observations 0 or 1)
#' obsraw  <- rep(rep(0:1, 11), round(c(t(cbind(1-obs, obs))) * 200))
#' predraw <- rep(pred, each = 200)
#' ll <- Loglikelihood(obs = obsraw, pred = predraw)
#' # all.equal(ll, -969.9514, 0.001)
#' llsat <- Loglikelihood(obs = obsraw, pred = predraw, saturated = TRUE)
#' # all.equal(llsat, -879.9013, .01)
#' }
#' @references { Busemeyer, J. R., & Diederich, A. (2010). Nonlinear parameter estimation. In Cognitive Modeling (pp. 43–84). Thousand Oaks, CAL: SAGE Publications. }
#' @export
Loglikelihood <- function(obs, pred, pdf = c("binomial", "normal", "truncnormal"), na.rm = FALSE, n = NULL, saturated = FALSE, eps = sqrt(.Machine$double.eps), binomial.coef = FALSE, sigma = NULL, a = NULL, b = NULL, ...) {

  pdf <- match.arg(pdf) # todo: add multinomial
  .args <-  as.list(rlang::call_standardise(match.call())[-1])
  if ( saturated & missing(pred) ) {
    pred <- obs
  }
  TMP <- .format_obs_pred_n(obs, pred, n, na.rm)
  obs <- TMP[['obs']]
  pred <- TMP[['pred']]
  n <- TMP[['n']]


  # For normal density we need sigma
  if (!is.null(sigma)) {
    sigma <- array(sigma, dim = dim(pred))
    sigma <- .format_obs_pred_n(sigma, pred, n, na.rm)[["obs"]]
    sigma <- .format_obs_pred_n(sigma, obs, n, na.rm)[["obs"]]
    obs <- .format_obs_pred_n(TMP$obs, sigma, n, na.rm)[["obs"]]
    pred <- .format_obs_pred_n(sigma, TMP$pred, n, na.rm)[["pred"]]
  }

  if (pdf == "normal") {
    return(sum(dnorm(x = obs, mean = pred, sd = sigma, log = TRUE)))
  } else if (pdf == "truncnormal") {
    a <- ifelse(is.null(.args[["a"]]), -Inf, .args[["a"]])
    b <- ifelse(is.null(.args[["b"]]),  Inf, .args[["b"]])
    dens <- truncnorm::dtruncnorm(x = obs, mean = pred, sd = sigma, a = a, b = b)
    dens[dens==0] <- eps # ensure dens > 0
    return(sum(log(dens)))
  }

  if ( pdf == "binomial" ) {
    if (!.isbetween(pred, 0, 1)) {
      stop("Prediction must range from 0.00 to 1.00, but they range from ", paste(sprintf("%.2f", range(pred)), collapse = " to "), ".",
      "\n  * Are you sure your predictions are binary or proportions (0 to 1)?",
      "\n  * Are you sure you want the log likelihood with binomial PDF?"
      , call. = FALSE)
    }
    pred <- pred[,1]
    obs <- obs[,1]
    pred[pred == 0] <- eps # ensure 0 < ped < 1
    pred[pred == 1] <- 1 - eps
    bc <- if (binomial.coef) { choose(n, round(obs*n)) } else { 1 }
    return( sum( log(bc) + log(pred) * n * obs + log(1-pred) * n * (1-obs) ) )
  } 
}