#' Compute akaike weights
#' 
#' @param x A numeric vector or matrix with AIC or log likelihood values
#' @param measure A string (default \code{"aic"}), the type of values in \code{x}. Can be one of \code{"aic"}, \code{"loglikelihood"}; can be abbreviated.
#' @references {Wagenmakers, E., & Farrell, S. (2004). AIC model selection using Akaike weights. \emph{Psychonomic Bulletin & Review, 11(1)}, 192–196. \url{https://doi.org/10.3758/BF03206482}}
#' @examples
#' aic_values <- c(204, 202, 206, 206, 214) # some AIC values
#' akaike_weight(aic_values)
#' 
#' @export
akaike_weight <- function(x, measure = c("aic", "loglikelihood")) {
  measure <- match.arg(measure)
  if (is.null(dim(x))) {
    x <- t(matrix(unlist(x)))
  }
  if (nrow(x) > 1) {
    return(t(apply(x, 1, akaike_weight, measure = measure)))
  }
  x <- as.numeric(x)
  best <- ifelse(measure == "aic", base::min(x), base::max(x))
  deltas <- (x - best) * (-0.5)^(measure == "aic")

  # The next line ensures we don't have too large numbers, running into NAs
  deltas <- deltas - max(deltas) # shift (exp is shift invariant)

  return(as.vector(exp(deltas) / sum(exp(deltas))))
}