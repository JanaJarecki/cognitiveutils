#' \code{APE()} calculates the average percentage error
#' @import stats
#' @inheritParams gof
#' @param response (default \code{"discrete"}) A string with response format, \code{"continuous"} or \code{"discrete"} (e.g., choices). Can be abbreviated. Will be inferred if missing from \code{obs}, \code{pred}.
#' @return Absolute percentage error between \code{obs} and \code{pred}.
#' @author Jana B. Jarecki, \email{jj@janajarecki.com}, Florian Seitz
#' @family goodness of fit functions
#' @examples
#' ### APE
#' # --------------------------------------------------------------------------
#' # First example from Gilliland, M. (2010). 
#' # The business forecasting deal: exposing myths, eliminating bad practices,
#' # providing practical solutions (Vol. 27). John Wiley & Sons. (Exhibit A.3):
#' pred <- rep(1250, 6)
#' obs <- c(1150, 1400, 1200, 1350, 1100, 1300)
#' APE(obs, pred, response = "c")
#' 
#' obs <- diag(x = 1, ncol = 3, nrow = 3)
#' pred <- matrix(c(0.7, 0.2, 0.1,
#'                  0.3, 0.4, 0.3,
#'                  0.1, 0.1, 0.8), byrow = TRUE, nrow = 3)
#' APE(obs, pred, response = 'd')
#' obs <- 1:3
#' pred <- matrix(c(0.7, 0.2, 0.1,
#'                  0.3, 0.4, 0.3,
#'                  0.1, 0.1, 0.8), byrow = TRUE, nrow = 3)
#' APE(obs, pred, response = 'd')
#'                  
#' obs <- diag(x = 1, ncol = 3, nrow = 3)
#' pred <- c(0.7, 0.4, 0.8)
#' APE(obs, pred, response = 'd')

#' @export
APE <- function(obs, pred, response = .guessResponse(obs, pred), ...) {
  response <- match.arg(response, c("discrete", "continuous"))
  pred <- as.matrix(pred)
  obs <- as.matrix(obs)
  
  if (response == "continuous" & max(ncol(obs), ncol(pred)) > 1) {
    stop('"obs" and "pred" can only have one column if response = continuous.')
  }  
  if (response == "discrete" & any(c(ncol(pred), ncol(obs)) > 1) ) {
    if (ncol(obs) == ncol(pred)) {
      # In this case obs is an indicator matrix (0 or 1)
      #     and pred has multiple columns with the resp. predictions
      pred <- as.matrix(pred[as.logical(obs)])
    } else if (ncol(obs) == 1) {
      # in this case obs has the column indices (1, 2, 3, ...)
      #     and pred has multiple columns with indicators (0, 1)
      obs <- obs - min(obs) # ensure min(obs) == 0
      pred <- as.matrix(pred[1:nrow(pred) + nrow(pred) * obs])
    }
    obs <- matrix(1, nrow = length(pred), ncol = 1)
  } 
  
  if (ncol(obs) != ncol(pred)) {
    stop('Arguments "obs" and "pred" in APE need the same number of columns\n\tbut ncol(obs) = ', ncol(obs), 'and ncol(pred) = ', ncol(pred), '.')
  }

  # Error measures
  out <- abs(pred - obs)
  if( response == 'continuous' ) {
    return(out/obs)
  } else {
    return (out)
  }
}
