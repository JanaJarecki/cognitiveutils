# all non-exported / unused internal (utility) functions

# convert a vector like c(1, 4, 3, 2) into a string like [1, 4, 3, 2]
#   (common aggregation method for error messages)
# From data.table
.brackify = function(x) {
  # arbitrary cutoff
  if (length(x) > 10L) x = c(x[1:10], '...')
  sprintf('[%s]', paste(x, collapse = ', '))
}

#----------------------------------------------------------------------
# Guess if response is continuous or discrte
#----------------------------------------------------------------------
.guessResponse <- function(x, y, n) {
  # y is the prediction
  if ( missing(n) ) {
    n <- 1
  }
  if ( is.factor(x) ||  is.character(x) ) { # clearly discrete
    return('discrete')
  }
  if ( !missing(y) ) {
    if ( !.isbetween(y, 0, 1) ) { # prediction is not a probability?
      return('continuous')
    } else if ( max(x, na.rm = TRUE) - min(x, na.rm = TRUE) > 1 ) { # prediction prob and observation binary
      return('discrete')
    } else if ( .isbetween(y, 0, 1) & .isbetween(x, 0, 1) ) #obs & pred in [0,1]
      if ( ncol(as.matrix(x) ) > 1 ) { # sd specified in obs
        return('continuous')
      } else if (any(n > 1)) {
        message('Guessing response as discrete, if this is wrong add options=list(response="continuous")')
        return('discrete') # aggregated discrete data
      }
  }
  return(NULL)
}

#----------------------------------------------------------------------
# Format the obs or pred when calculating goodness of fit
#----------------------------------------------------------------------
.format_obs_pred_n <- function(obs, pred, n, na.rm) {
  obs <- as.matrix(obs)
  pred <- as.matrix(pred)
  if ( is.null(n) ) {
    n <- 1
  } else {
    n <- as.vector(as.numeric(n))
  }
  if ( length(n) == 1 ) {
    n <- rep(n, min(nrow(obs),nrow(pred)) )
  }
  if ( na.rm ) {
    complete <- complete.cases(cbind(obs, pred, n))
    obs <- obs[complete, , drop = FALSE]
    pred <- pred[complete, , drop = FALSE]
    n <- n[complete]
  }
  if ( length(n) != min(nrow(obs),nrow(pred)) ) {
      stop('"n" needs length 1 or min(nrow(obs), nrow(pred)),\n\tlength(n)=', length(n), ' and min nrow=', min(nrow(obs),nrow(pred)), '. Check "n".', call.=FALSE)
  }
  if ( (nrow(pred) != nrow(obs)) & (sum(n) != max(nrow(obs),nrow(pred))) ) {
    stop('"n" does not fit to the length of the data. If you want to aggregate, cumsum(n) must amount to the length of the raw data. If obs and pred are aggregates, check if "obs" and "pred" are equally long.')
  }
  if ( nrow(pred) < nrow(obs) ) { # pred is aggregated
    obs <- aggregate(obs, list(rep(seq_along(n), times=n)), mean)[,-1, drop=FALSE]
  }
  if ( nrow(pred) > nrow(obs) ) { # obs is aggregated
    pred <- aggregate(pred, list(rep(seq_along(n), times=n)), mean)[,-1, drop=FALSE]
  }
  return(list(obs = as.matrix(obs), pred = as.matrix(pred), n = n))
}

#----------------------------------------------------------------------
# Matrix with rows that sum up to x
#----------------------------------------------------------------------
RowSumMatrix <- function(rs, ncol, intervalls = NULL, boundary.offset = 0.001, nrow = round(sqrt(10^ncol*2))) {
   if (missing(intervalls)) {
      mat <- matrix(rbeta(ncol * nrow, 2, 2), ncol = ncol)
      mat <- sweep(mat, 1, rowSums(mat), FUN = "/") * rs
   }
   if (!missing(intervalls)) {
      mat <- t(combinat::xsimplex(ncol, round(rs / intervalls), function(x) x/sum(x)))
   }

   mat[mat == 0] <- boundary.offset
   mat[mat == rs] <- 1 - boundary.offset
   mat <- mat / rowSums(mat)

   return(mat)
}

#----------------------------------------------------------------------
# Format the obs or pred when calculating goodness of fit
#----------------------------------------------------------------------
.isbetween <- function(x, ll = 0, ul = 1, tol = 1e-10) {
  ll <- ll - tol
  ul <- ul + tol
  return(!(any((x < ll) | any(x > ul))))
}

#----------------------------------------------------------------------
# Spell a number as word if <= 10
# For participants() print function
#----------------------------------------------------------------------
.spell <- function(x) {
    if (is.numeric(x)) {
        return(ifelse(x > 10, x, as.character(english::as.english(x))))
    }
}


#----------------------------------------------------------------------
# Add a plural s to a word
# For participants() print function
#----------------------------------------------------------------------
.pluralize <- function(x, n) {
    if ( is.null(x) ) {
        return(x)
    }
    x <- trimws(x)
    if ( all(x == toupper(x)) ) {
        # Check if x seems to be an abbreviation (e.g. CHF, USA, etc)
        return(x)
    }
    if ( all( grepl("[0-9]", substr(x, nchar(x), nchar(x)) ) )  ) {
        # if last charaxter is a number, dont .pluralize
        return(x)
    }
    if (n != 1) {
        return(switch(x,
            was = "were",
            has = "have",
            is = "are",
            paste0(x, "s")
            ))
    } else {
        return(x)
    }
}
.pluralize <- Vectorize(.pluralize)


#----------------------------------------------------------------------
# Print a summary of a variable
# For participants() print function
#----------------------------------------------------------------------
.printsummary <- function(x, variable = "", unit = "", digits = NULL, ...) {
  num_na <- sum(is.na(x))
  if (num_na > 0) {
    x <- x[!is.na(x)]
    if ( length(x) == 0 ) {
      stop("One variable you specified contains only NA values, check ", unit, ".")
    }
  }

  if ( is.numeric(x) ) {
    if ( is.null(digits) ) {
      digits <- .guessdigits(x)
    }
    z <- sprintf(paste0("%.", digits, "f"), round(c(mean(x), sd(x), median(x), min(x), max(x)), digits = digits))
    if ( isTRUE(all.equal(max(x), min(x))) ) {
        txt <- paste(variable, "was", z[0])
    }

    txt <- paste("mean", variable, z[1], .pluralize(unit, mean(x)), " (",
      "med = ", z[3], ", ",
      "sd = ", z[2], ", ",
      "range", paste0(z[4], "-", z[5]),
      .pluralize(unit, 2),
      ")")
    }

  if ( is.POSIXct(x) ) {
    old_lctime <- Sys.getlocale("LC_TIME")
    Sys.setlocale("LC_TIME", "C")
    if ( format(min(x), "%B %Y") == format(max(x), "%B %Y") ) {
      txt <- "in"
    } else if (format(min(x), "%Y") == format(max(x), "%Y")) {
      txt <- paste("from", format(min(x), "%B"), "to")
    } else {
      txt <- paste("from", format(min(x), "%B %Y"), "to")
    }
    txt <- paste(txt, format(max(x), "%B %Y"))
    Sys.setlocale("LC_TIME", old_lctime)
    }

  if ( is.character(x) | is.factor(x) ) {
    tab <- table(x, useNA = "ifany")
    if ( !is.ordered(x) ) {
      tab <- tab[order(tab, decreasing = TRUE)]
    }
    N <- paste(variable, .itemize(apply(rbind(tab, .pluralize(names(tab), tab)), 2, paste, collapse = " ")))
    digits <- 2
    while ( min(round(prop.table(tab) * 100, digits)) > 0 ) {
      relN <- paste0("(", .itemize(paste0(round(prop.table(tab) * 100, digits), "%")), ", respectively)")
      digits <- digits - 1
        if (digits == -1) { break }
      }        
    txt <- paste(N, relN, collapse = " ")
    }

  if ( num_na > 0 ) {
    txt <- paste0(txt, "(", num_na, " missing)")
    txt <- sub("\\)\\(", "; ", txt)
  }
  
  return(txt)
}


#----------------------------------------------------------------------
# Guess how many digits to print
# For participants() print function
#----------------------------------------------------------------------
.guessdigits <- function(x) {
  dist <- abs(max(x) - min(x))
  if (dist <= 0.01 ) {
    return(3)
  } else if (dist <= 1) {
    return(2)
  } else if (dist <= 10) {
    return(1)
  } else {
    return(0)
  }
}


#----------------------------------------------------------------------
# Guess the gender of a variable
# For participants() print function
#----------------------------------------------------------------------
.genderize <- function(x) {
    x <- tolower(x)
    x[grepl("^f", x)] <- "female"
    x[grepl("^m", x)] <- "male"
    return(x)
}
.genderize <- Vectorize(.genderize)

#----------------------------------------------------------------------
# Print a vetor 1 2 3 as "[1, 2, 3, ...]"
# For error msg
#----------------------------------------------------------------------
.itemize <- function(x) {
    if ( length(x) == 1 ) {
        return(x)
    }
    return(paste(paste(head(x, -1), collapse = ", "), "and", tail(x, 1)))
}


#----------------------------------------------------------------------
# Clean up text
# For participants() print function
#----------------------------------------------------------------------
.cleantxt <- function(x) {
    x <- trimws(x)
    while(any(grepl(",$|;$|\\.$", x))) {
        x <- gsub(",$|;$|\\.$", "", x)
    }
    x <- paste0(x, ".")
    while ( any(grepl("\\( | \\)", x)) ) {
        x <- gsub("\\( ", "\\(", x)
        x <- gsub(" \\)", "\\)", x)
    }
    while (any(grepl("; ;| ;", x))) {
        x <- gsub("; ;| ;", "; ", x)
    }
    while(any(grepl(" ,", x))) {
        x <- gsub(" ,", ", ", x)
    }
    while(any(grepl("  ", x))) {
        x <- gsub("  ", " ", x)
    }
    while(any(grepl("\\.\\.|\\. \\.", x))) {
        x <- gsub("\\.\\.|\\. \\.", "\\.", x)
    }
    while(any(grepl(",,|, ,", x))) {
        x <- gsub(",,|, ,", ", ", x)
    }
    while(any(grepl(";,|; ,", x))) {
        x <- gsub("|; ,", "; ", x)
    }
    while(any(grepl(",;|, ;", x))) {
        x <- gsub(",;|, ;", ", ", x)
    }
    while(any(grepl(",\\.|, \\.", x))) {
        x <- gsub(",\\.|, \\.", "\\. ", x)
    }
    while(any(grepl("  ", x))) {
        x <- gsub("  ", " ", x)
    }
    x <- gsub("(^|\\.)\\s*([a-z])", "\\1 \\U\\2", x, perl = TRUE)
    return(trimws(x))
}



#' Drop the second dimension
#' 
#' Drop the second dimension of an array or matrix
#' 
#' @importFrom abind adrop
#' @param x An object of type array or matrix
#' @export
drop2 <- function(x) {
  d <- dim(x)
  if (is.null(d)) {
    return(x)
  } else {
    if (d[2] != 1) {
      return(x)
    } else {
      return(abind::adrop(x, drop = 2))
    }
  }
}