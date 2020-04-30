#' Compuate mean and (finite-sample corrected) standard deviation
#'
#' @param x A numeric vector
#' @param na.rm  Logical, whether to remove \code{NA} values in \code{x}.
#' @return A named list with the mean and the standard deviation
#' @examples
#' x <- sample(1:100)
#' meansd(x)
#' @export
meansd <- function(x, na.rm = TRUE) {
  res <- list(m = mean(x), sd = sd(x))
  class(res) <- "meansd"
  return(res)
}


#' Print method for objects of class \code{"meansd"}
#'
#' @param x A numeric vector
#' @param label (default \code{TRUE}) Logical, \code{TRUE} adds "M" and "SD" as labels.
#' @param na.rm Logical, whether to remove \code{NA} values in \code{x}.
#' @param digits The number of digits to print.
#' @return A string holding the mean m und standard deviation sd as \code{"m (sd)"} or \code{"M = m, SD = sd"}
print.meansd <- function(x, digits = 2, na.rm = TRUE, label = FALSE){
  if (all(is.na(x))) {
    return(NA_character_)
  }
  x <- sprintf(unlist(x), fmt = paste0("%.", digits, "f"))
  label <- if (label == TRUE) {
    c("M=", ", SD=", "")
  } else {
    c("", " (SD=", ")")
  }
  cat(paste0(label[1], x[1], label[2], x[2], label[3]), "\n")
}