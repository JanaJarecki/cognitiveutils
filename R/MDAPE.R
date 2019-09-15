#' \code{MDAPE()} calculate median absolute percentage error
#' @import stats
#' @inheritParams APE
#' @return Median absolute percentage error between \code{obs} and \code{pred}
#' @author Jana B. Jarecki, \email{jj@janajarecki.com}, Florian Seitz
#' @family goodness of fit functions
#' @examples
#' # First example from Gilliland, M. (2010). 
#' # The business forecasting deal: exposing myths, eliminating bad practices,
#' # providing practical solutions (Vol. 27). John Wiley & Sons. (Exhibit A.3):
#' pred <- rep(1250, 6)
#' obs <- c(1150, 1400, 1200, 1350, 1100, 1300)
#' MDAPE(obs, pred, response = "con")

#' @export
MDAPE <- function(obs, pred, response = .guessResponse(obs, pred), ...) {
  return(median(APE(obs = obs, pred = pred, response = response, ...)))
}
