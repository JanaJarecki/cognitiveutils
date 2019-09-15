#' Print the participant section for a manuscript from a data.frame
#'
#' @importFrom english english
#' @importFrom lubridate is.POSIXct
#' @import data.table
#' @param data data object for which the demographics should be printed.
#' @param id a string, name of the column in \code{data} that holds subject identifiers, defaults to \code{"id"}.
#' @param age a string, name of the column in \code{data} that holds the age, defaults to \code{"age"}.
#' @param gender a string, name of the column in \code{data} that holds the gender, defaults to \code{"gender"}. If gender is numeric you should supply the argument \code{gender.label}.
#' @param excl a string or a number. A number specifies how many participants have been excluded from \code{data}; and the \code{data} must be \emph{after} list-wise exclusion. Alternatively, a string specifies the name of the variable in \code{data} holding reasons for excluding participants with \code{NA} or \code{""} values for including participants. In this case \code{data} must \emph{contain} the participants to be excluded. Exclusion will be performed in the function \emph{before} computing demographics.
#' @param date (optional) a string, either the data collection period (e.g., \code{"April to May 2019"}), or the name of the variable in \code{data} holding time stamps. Ideally, this variable is in datetime format (with \link[base]{as.POSIXlt}), if it is a character/string the format will be guessed, or can be specified by \code{date.format}.
#' @param recruitedfrom (optional) a string, where were participants recruited? E.g., "Amazon Mechanical Turk" or "Harvard's subject pool".
#' @param collectedat (optional) string, where was the data collected? E.g. "online" or Stanford University".
#' @param approvedby (optional) string, which ethics board approved the study? E.g., "the Oxford University's ethics comittee"
#' @param compensation (optional) a string, either \code{"course credit"}, or the name of the variable in \code{data} holding how much participants received. Consider specifying the payment currency, see the argument \code{currency}.
#' @param gender.label (optional) if the values of the gender variable in data are numeric, specify which number represents which gender, e.g. \code{c(1 = "female", 2 = "male", 3 = "no answer")}.
#' @param date.format (optional) if \code{date} in \code{data} has character format (and not POSIXct, which is the R date format) it may be ambiguous if \code{"02-10-2020"} means 02. Oct. or 10. Feb. In this case an error is thrown and \code{date.format} needs to be supplied. If \code{"02-10-2020"} means 02. Oct. use \code{date.format = "\%d\%m\%Y"}, if \code{"02-10-2020"} means 10. Feb. use \code{date.format = "\%m\%d\%Y"}.
#' @param currency (optional) string, only if the argument \code{compensation} refers to a monetary values, specify the currency, default is \code{currency = "US dollar"}.
#' @param more (optional) list, specifying additional variables to be summarized. See details.
#' @param file (optional) string, output file location, can be *.tex or *.txt.
#' @return A string with the participant description.

#' @references
#' Sutton, R. S., & Barto, A. G. (1998). ReINforcement learnINg: An INtroduction. Cambridge, MA: MIT Press
#' @details The argument \code{more} works as follows: You want to print "Mean study duration was ... minutes". Your data contains a variable \code{dur_min}. Specify this: \code{more = list("study duration" = c("time_min", "minutes"))}, the name \code{"study duration"} is the name to be printed, "time_min" is the column name in your data, and \code{"minute"} determines the unit (in singular).
#' @examples
#' mydata <- data.frame(
#'     id =   1:3,
#'     date = c("01102010-10:30", "02102010-10:30", "03102010-10:30"),
#'     payed = c(10,7,1.56),
#'     age =   c(20,22,21),
#'     fem =   c("m","f","m"),
#'     dur_min = c(22, 25, 30),
#'     income = c(1200, 1500, 2400))
#' \donttest{
#' # Print minimal demographics
#' participants(mydata, id = "id", age = "age", gender = "fem", excl = 0)
#' 
#' # Result:
#' # In total three participants completed the study (zero were excluded),
#' # 2 males and 1 female (67% and 33%, respectively), mean age 21 years
#' # (med = 21, sd = 1, range 20-22 years). 
#' # Note, this is too little information according to APA.
#' 
#' # Add the date
#' participants(mydata, id = "id", age = "age", gender = "fem", excl = 0,
#'              date = "date",
#'              date.format = "%d%m%Y") # since date is not un-ambiguous
#' 
#'# Add where the data was collected
#' participants(mydata, id = "id", age = "age", gender = "fem", excl = 0, 
#'              date = "date", date.format = "%d%m%Y", 
#'              collectedat = "the University of Basel") # local
#' participants(mydata, id = "id", age = "age", gender = "fem", excl = 0,
#'              date = "date", date.format = "%d%m%Y", collectedat = "online")
#' 
#' # Add further variables: income
#' participants(mydata, id = "id", age = "age", gender = "fem", excl = 0,
#'             date = "date", date.format = "%d%m%Y", collectedat = "online",
#'             more = list("income" = c("income", "CHF")))
#'
#' # Exclude one participant
#' mydata$exclusion <- c(NA, NA, 'failing attention checks') 
#' participants(mydata, id = "id", age = "age", gender = "fem", excl = "exclusion")
#' }
#' 
#' # Save result to file
#' participants(mydata, id = "id", age = "age", gender = "fem", excl = 0, file = "participants.txt")
#' @export
participants <- function(data, id, age, gender, excl, date = NULL, recruitedfrom = NULL, collectedat = NULL, approvedby = NULL, compensation = NULL, gender.label = NULL, date.format = NULL, currency = "US dollar",  more = NULL, file = NULL) {

    data <- as.data.table(data)

    # Deduplicate
    ids <- subset(data, select = id)
    if ( any(duplicated(ids)) ) {
        message("data has duplicated ids! The duplicates were discarded.")
    }
    data = data[!duplicated(ids), ]

    # Check --------------------------------------------------------------------
    if ( missing(more) ) {
        more <- NULL
    }   
    vars <- c(id = id, age = age, gender = gender, date = date, compensation = compensation, unlist(lapply(more, "[", index = 1)))
    if ( !is.numeric(excl) ) {
      vars <- c(vars, excl = excl)
    }
    if ( !is.null(compensation) & any(grepl("^credit|^credits|credit|credits", compensation)) ) {
        vars <- vars[-which(names(vars)=="compensation")]
    }
    sapply(seq_along(vars), function(i) if(!vars[i] %in% names(data) & !is.null(vars[i])) {
        stop("The variable '", names(vars[i]), " = ", vars[i],"' is not a column name in data.")
        })
    data <- subset(data, select = vars)

    # setup the TXT ------------------------------------------------------------
    TXT = "In total _N0 participants _REC completed the study _NE _WHY _N1; _NGEN, _AGE, _REM, data were collected _LOC _DATE, _ET."
    IN <- list("_N0" = NA, "_NGEN" = "", "_AGE"  = "", "_REC" = "", "_NE"   = "", "_WHY"  = "", "_N1"   = "", "_REM"  = "", "_DATE" = "", "_LOC"  = "", "_ET"   = "")

    # recruited
    if ( !is.null(recruitedfrom) ) {
      IN["_REC"] <- paste("recruited from", recruitedfrom)
    }

    # collectedat
    if ( !is.null(collectedat) ) {
        if ( collectedat == "online" ) {
            TXT <- sub("the study", "an online study", TXT)
            collectedat <- NULL
        } else {
            IN["_LOC"] <- paste("at", collectedat)
        }    
    }

    # both are null
    if ( is.null(collectedat) & is.null(date) ) {
      TXT <- gsub("data were collected _LOC _DATE, ", "", TXT)
    }

    # approvedby
    if ( !is.null(approvedby) ) {
      IN["_ET"] <- paste("the study was approved by", approvedby)
    }

    # compensation or remuneration
    if ( !is.null(compensation) ) {
      IN["_REM"] <- ifelse(grepl("^credit|^credits|credit|credits", compensation), "Participants received course credits", "")
      compensation <- NULL
    }
    
    # total sample size
    n0 <- n1 <- nrow(data)
    if ( is.numeric(excl) ) {
        n0 <- n0 + excl
        why <- ''
    } else {
        excl <- unlist(subset(data, select = excl))
        excl_ids <- unlist(which( !(is.na(excl) | excl == "") ))
        why <- unique(excl[excl_ids])
        data <- data[-excl_ids, ]
        n1 <- nrow(data)
    }
    ne <- n0 - n1
    IN["_N0"]  <- .spell(n0)

    # excluded particpants
    IN["_NE"]  <- ifelse(ne > 0, paste(",", .spell(ne), .pluralize("was", ne), "excluded"), "(zero were excluded), ")
    IN["_WHY"] <- ifelse(ne > 0 & !is.numeric(excl), paste("(for", .itemize(why), ")"), IN["_WHY"])
    IN["_N1"] <- ifelse(ne > 0, paste(", leaving a final sample of N = ", n1), IN["_N1"])

    # Age
    age <- unlist(subset(data, select = age))
    IN["_AGE"] <- .printsummary(age, "age", "year", 0)

    # Compensaton
    if ( !is.null(compensation) ) {
      comp <- unlist(subset(data, select = compensation))
      IN["_REM"] <- ifelse(IN["_REM"] == "", .printsummary(comp, "remuneration was", currency), IN["_REM"])
    }

    # Gender
    g <- unlist(subset(data, select = gender))
    if ( is.numeric(g) ) {
        g <- factor(g, levels = as.numeric(names(gender.label)), labels = gender.label)
    }
    IN["_NGEN"] <- .printsummary(.genderize(g))

    # Recruitment time period
    if ( !is.null(date) ) {
      args <- as.list(setnames(subset(data, select = date), "x"))
      if ( !is.null(date.format) ) {
          args <- c(args, format = date.format)
      }
      IN["_DATE"] <- .printsummary( do.call(as.POSIXct, args) )
    }


    # Result
    for (i in 1:length(IN)) {
        TXT <- .cleantxt(gsub(names(IN[i]), IN[i], TXT))
    }

    # more
    MORE <- NULL
    if ( !is.null(more) ) {
        MORE <- sapply(1:length(more), function(i) {
            v <- more[[i]][[1]]
            x <- unlist(subset(data, select = v))
            variable = names(more)[i]
            unit = more[[i]][2]
            if ( length(more[[i]]) > 2) {
                digits = more[[i]][3]
            } else {
              digits <- NULL # guess digits
            }
            if (is.character(x) | is.factor(x) ) {
                variable <- paste(variable, "n =")
            }
            .printsummary(x, variable, unit, digits)
        })
    }
    TXT <- paste(TXT, paste(MORE, collapse = "; "), collapse = ".")

    cat("\n", .cleantxt(TXT), "\n")
   
    if ( !is.null(file) ) {
        if( grepl(".tex", file) ) {
            # Change percentage sign to \% if we prINt to a LaTeX file
            TXT = gsub("%", "\\\\%", TXT)
            TXT = gsub("=", "$=$", TXT)
            TXT = gsub("SD \\+/\\-", "$ SD \\\\; \\\\pm$ ", TXT)
        } else {
            write(x = TXT, file = file)
        }
    }
}