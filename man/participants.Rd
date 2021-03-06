% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/participants.R
\name{participants}
\alias{participants}
\title{Print the participant section for a manuscript from a data.frame}
\usage{
participants(
  data,
  id,
  age,
  gender,
  excl,
  date = NULL,
  recruitedfrom = NULL,
  collectedat = NULL,
  approvedby = NULL,
  compensation = NULL,
  gender.label = NULL,
  date.format = NULL,
  currency = "US dollar",
  more = NULL,
  file = NULL
)
}
\arguments{
\item{data}{data object for which the demographics should be printed.}

\item{id}{a string, name of the column in \code{data} that holds subject identifiers, defaults to \code{"id"}.}

\item{age}{a string, name of the column in \code{data} that holds the age, defaults to \code{"age"}.}

\item{gender}{a string, name of the column in \code{data} that holds the gender, defaults to \code{"gender"}. If gender is numeric you should supply the argument \code{gender.label}.}

\item{excl}{a string or a number. A number specifies how many participants have been excluded from \code{data}; and the \code{data} must be \emph{after} list-wise exclusion. Alternatively, a string specifies the name of the variable in \code{data} holding reasons for excluding participants with \code{NA} or \code{""} values for including participants. In this case \code{data} must \emph{contain} the participants to be excluded. Exclusion will be performed in the function \emph{before} computing demographics.}

\item{date}{(optional) a string, either the data collection period (e.g., \code{"April to May 2019"}), or the name of the variable in \code{data} holding time stamps. Ideally, this variable is in datetime format (with \link[base]{as.POSIXlt}), if it is a character/string the format will be guessed, or can be specified by \code{date.format}.}

\item{recruitedfrom}{(optional) a string, where were participants recruited? E.g., "Amazon Mechanical Turk" or "Harvard's subject pool".}

\item{collectedat}{(optional) string, where was the data collected? E.g. "online" or Stanford University".}

\item{approvedby}{(optional) string, which ethics board approved the study? E.g., "the Oxford University's ethics comittee"}

\item{compensation}{(optional) a string, either \code{"course credit"}, or the name of the variable in \code{data} holding how much participants received. Consider specifying the payment currency, see the argument \code{currency}.}

\item{gender.label}{(optional) if the values of the gender variable in data are numeric, specify which number represents which gender, e.g. \code{c(1 = "female", 2 = "male", 3 = "no answer")}.}

\item{date.format}{(optional) if \code{date} in \code{data} has character format (and not POSIXct, which is the R date format) it may be ambiguous if \code{"02-10-2020"} means 02. Oct. or 10. Feb. In this case an error is thrown and \code{date.format} needs to be supplied. If \code{"02-10-2020"} means 02. Oct. use \code{date.format = "\%d\%m\%Y"}, if \code{"02-10-2020"} means 10. Feb. use \code{date.format = "\%m\%d\%Y"}.}

\item{currency}{(optional) string, only if the argument \code{compensation} refers to a monetary values, specify the currency, default is \code{currency = "US dollar"}.}

\item{more}{(optional) list, specifying additional variables to be summarized. See details.}

\item{file}{(optional) string, output file location, can be *.tex or *.txt.}
}
\value{
A string with the participant description.
}
\description{
Print the participant section for a manuscript from a data.frame
}
\details{
The argument \code{more} works as follows: You want to print "Mean study duration was ... minutes". Your data contains a variable \code{dur_min}. Specify this: \code{more = list("study duration" = c("time_min", "minutes"))}, the name \code{"study duration"} is the name to be printed, "time_min" is the column name in your data, and \code{"minute"} determines the unit (in singular).
}
\examples{
mydata <- data.frame(
    id =   1:3,
    date = c("01102010-10:30", "02102010-10:30", "03102010-10:30"),
    payed = c(10,7,1.56),
    age =   c(20,22,21),
    fem =   c("m","f","m"),
    dur_min = c(22, 25, 30),
    income = c(1200, 1500, 2400))
\donttest{
# Print minimal demographics
participants(mydata, id = "id", age = "age", gender = "fem", excl = 0)

# Result:
# In total three participants completed the study (zero were excluded),
# 2 males and 1 female (67\% and 33\%, respectively), mean age 21 years
# (med = 21, sd = 1, range 20-22 years). 
# Note, this is too little information according to APA.

# Add the date
participants(mydata, id = "id", age = "age", gender = "fem", excl = 0,
             date = "date",
             date.format = "\%d\%m\%Y") # since date is not un-ambiguous

# Add where the data was collected
participants(mydata, id = "id", age = "age", gender = "fem", excl = 0, 
             date = "date", date.format = "\%d\%m\%Y", 
             collectedat = "the University of Basel") # local
participants(mydata, id = "id", age = "age", gender = "fem", excl = 0,
             date = "date", date.format = "\%d\%m\%Y", collectedat = "online")

# Add further variables: income
participants(mydata, id = "id", age = "age", gender = "fem", excl = 0,
            date = "date", date.format = "\%d\%m\%Y", collectedat = "online",
            more = list("income" = c("income", "CHF")))

# Exclude one participant
mydata$exclusion <- c(NA, NA, 'failing attention checks') 
participants(mydata, id = "id", age = "age", gender = "fem", excl = "exclusion")
}

# Save result to file
participants(mydata, id = "id", age = "age", gender = "fem", excl = 0, file = "participants.txt")
}
\references{
Sutton, R. S., & Barto, A. G. (1998). ReINforcement learnINg: An INtroduction. Cambridge, MA: MIT Press
}
