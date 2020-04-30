#' sAccuracy
#' 
#' @import stats
#' @inheritParams gof
#' @inheritParams APE
#' @return The percent of accurate predictions between 0 and 1, inclusive, where 1 indicates 100% accuracy.
#' 
#' @param obs A numeric vector or matrix, the observed data.
#' @param response (optional) A string (default \code{"discrete"}), data format. Can also be \code{"continuous"}. Can be abbreviated. If \code{"discrete"}, will apply an \code{\link{cr_argmax}} before computing accuracy.
#' 
#' @details
#' \emph{Note.} Accuracy corrects for perfect chance. Perfect chance predictions for discrete responses (e.g. 0.50 for binary responses) yield chance-level accuracy (50 \%), rather than zero accuracy.
#' 
#' @family goodness of fit functions
#' 
#' @examples
#' Accuracy(obs = c(1,0,0), pred = c(1,1,1))        # 33% accuracy
#' 
#' # Note how it deals with perfect-chance predictions (0.5)
#' Accuracy(obs = c(1,0,0), pred = c(1,0.5,0.5))        # 66% accuracy
#' 
#' @export
Accuracy <- function(obs, pred, na.rm = FALSE, response = c("discrete", "continuous"), ...) {
  response <- match.arg(response, c("discrete", 'continuous'))
  if (response == "discrete") {
    pred <- cognitiveutils::cr_argmax(pred)
  }
  return(mean((1 - abs(as.matrix(obs) - as.matrix(pred)))[,1]))
}
