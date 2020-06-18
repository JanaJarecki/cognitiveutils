#' Compute goodness of fit measures
#' 
#' \code{gof()} is for general goodness of model fits, \code{gof_continuous()} for continuous data, \code{gof_discrete()} for discrete data.
#'
#' @rdname gof
#' @param obs A numeric vector or matrix, the observed data. Can be continuous values or dicrete.  Can be aggregated, and if so you \strong{must} supply \code{n} (see below). The default assumes raw data.
#' @param pred A numeric vector or matrix with predictions, in the same order as \code{obs}.
#' @param type A string (default \code{"loglikelihood"}) specifying the goodnes-of-fit or error measure, allowed are \code{"sse"} (sum of squared error), \code{"mse"} (mean squared error), \code{"rmse"} (root-mean squared error), \code{"wmse"} (weighted mean squared error), \code{"mape"} (mean absolute percentage error), and \code{"mdape"} (median absolute percentage error), code{"accuracy"} (percent of \code{obs} equal to \code{pred}, after applying an \code{\link{cr_argmax}} choice rule to probabilistic predictions of discrete data).
#' @param n (optional) Integer or integer vector (default: 1), number of observations underlying obs if `obs`, `pred` or both are \bold{aggregated}: `n=10` means each aggregate represents 10 data points, a vector `n=c(10,20)` means the first aggregate represents 10, the second 20 data points, etc.
#' @param na.rm (optional) Logical (default `FALSE`). `TRUE` removes all `NA` rows in `pred` or `obs` jointly (list-wise removal).
#' @param response (optional) String, the type of observed data, `"discrete"` or `"continuous"`. Can be eabbreviated. Will be guessed as discrete if `"obs"` is a factor or character, and as continuous if `"pred"` is not in 0-1.
#' @param pdf (optional) String, probability density function in log likelihood, allowed values see [Loglikelihood()]
#' @param saturated (optional) Logical (default `FALSE`) `TRUE` returns saturated log likelihood.
#' @param binomial.coef (optional) Logical (default `FALSE`), `TRUE` adds the binomial coefficient to a binomial log likelihood.
#' @param ... more arguments to be passed on to the fitting functions, see e.g. [loglikelihood()]
#' @return The goodness of fit.
#' @details 
#' The observations can be discrete or continuous response data. If `response = "discrete"` then `obs` can be either a vector with the different choices, or a matrix with as many columns as there are choice options. Each column contains a 0 if not chosen and 1 for chosen.
#' Predictions can be aggregated or for individual observations. Predictions are individual if `n` is not supplied: each `pred` predicts the corresponding row in `obs` or, if `pred` is a single value, this value predicts the mean of `obs`.
#' Predictions are aggregated if \code{n} is supplied: Each value in \code{pred} predicts \code{n} observations. If \code{obs} and \code{pred} are equally long and \code{n} is supplied, then it is assumed that \code{obs} represent the mean predictions across \code{n} values.
#' @examples
#' gof(c(.33, .66), c(1,0), "mse")
#' gof(c(.33, .66), c(1,0), "loglikelihood")
#' gof(c(.33, .66), c(1,0), "loglikelihood", options = list(response = "d"))
#' @references Busemeyer, J. R., & Diederich, A. (2010). Nonlinear parameter estimation. In Cognitive Modeling (pp. 43–84). Thousand Oaks, CAL: SAGE Publications.
#' @md
#' @family goodness of fit functions
#' @export
gof <- function(obs, pred, type = c('loglikelihood', 'mse', 'wmse', 'rmse', 'sse', 'wsse', 'mape', 'mdape', 'accuracy'), na.rm = FALSE, pdf = NULL, response = NULL, saturated = FALSE, binomial.coef = FALSE, ..., n = NULL) {
  options <- c(list(
    pdf = pdf,
    response = response,
    saturated = saturated,
    binomial.coef = binomial.coef),
    list(...))
  type <- match.arg(type)
  if ( is.null(options$response) ) {
    tmp <- .format_obs_pred_n(obs, pred, n, na.rm)
    response <- .guessResponse(tmp$obs, tmp$pred, tmp$n)
    if ( is.null(response) ) {
      response <- 'discrete'
      message("Setting response to discrete. \n  * If 'obs' is contiuous, set 'response = \"continuous\"'.")
      }
  } else { 
    response <- match.arg(options$response, c('discrete', 'continuous'))
  }
  options$response <- response

  # Sanitize PDF for log likelihood
  if (type == "loglikelihood") {
    if (is.null(options$pdf)) {
      options$pdf <- switch(response,
        discrete = "binomial",
        continuous = "normal")
    } else {
      options$pdf <- match.arg(options$pdf, c("binomial", "normal", "truncnormal", "multinomial"))
    }
  }
  
  #obs <- as.vector(obs)
  pred <- as.matrix(pred)
  if ( ncol(pred) == 1 ) {
    pred <- as.vector(pred)
  } else {
    pred <- .check_matrix(pred, obs, response)
  }
  if ( is.character(obs) | is.factor(obs) ) {
    obs <- .check_factor(obs, pred)
  }
  # obs[, 1, drop = FALSE]
  TMP <- .format_obs_pred_n(obs, pred, n, na.rm)
  obs <- TMP$obs
  pred <- TMP$pred
  n <- TMP$n
  if ( length(unlist(obs)) != length(unlist(pred)) & type != 'loglikelihood') {
    stop('"obs" must be as long as "pred", length(pred): ', length(pred), ', length(obs): ',length(obs), '.')
  }
  .a <-  as.list(match.call()[-1])
  if (type == "loglikelihood" & response == "continuous" & is.null(.a["sigma"])) stop("Must supply an argument 'sigma' for loglikelihood with continuous response data.")

  .a[['obs']] <- obs
  .a[['pred']] <- pred
  .a[['n']] <- n
  return(
    switch(type,
      loglikelihood = do.call(cognitiveutils::loglikelihood, .a),
      sse = do.call(cognitiveutils::SSE, .a),
      wsse  = do.call(cognitiveutils::SSE, c(.a, weighted = TRUE)),
      mse = do.call(cognitiveutils::MSE, .a),
      wmse = do.call(cognitiveutils::MSE, c(.a, weighted = TRUE)),
      rmse = do.call(cognitiveutils::RMSE, c(.a)),
      mape = do.call(cognitiveutils::MAPE, c(.a)),
      mdape = do.call(cognitiveutils::MDAPE, c(.a)),
      accuracy = do.call(cognitiveutils::Accuracy, c(.a))
    )
  )
}

.check_vector <- function(x) {
  if ( !is.vector(x) | ncol(as.matrix(x)) != 1) {
    stop('Argument "pred" in gof() must be a vector when "obs" is a vector,\nbut "pred" is a <', class(x), '>.')
  } else {
    return(as.vector(x))
  }
}

.check_matrix <- function(x, obs, response) {
  if ( ncol(x) == 1 ) {
    return(as.vector(x))
  } else if ( response == "discrete" ) {
      return(x[obs!=0])
  } else {
    return(x)
  }
}


.check_factor <- function(obs, pred) {
  if ( ncol(as.matrix(pred)) > 1 ) {
    if ( ncol(pred) < length(unique(obs)) ) {
      stop("'pred' in gof() must have the same number of columns as the number of unique value in 'obs', but 'pred' has ", ncol(pred), " columns, and 'obs' has ", length(unique(obs)), "unique values.")
    } else if( is.null(colnames(pred)) ) {
        return(factor)
      } else  {
        return(factor(obs, levels = colnames(pred)))
    }
  }
  if ( is.ordered(obs) ) {
    return(as.numeric(obs))
  } else {
    return(as.numeric(levels(obs))[obs])
  }
}
