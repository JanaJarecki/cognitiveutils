# cogsciutils
An R-package with helper functions for social scientists and cognitive psychologists. Note, the package is currently being developed (bleeding edge).

# What you can do

 - **Goodness of fit** `gof()` is general function to calculate different goodness-of-fit measures, e.g.
   - Loglikelihood, see also `Loglikelihood()`
   - Squared error measures, see also `MSE()`, `SSE()`
   - Percent accuracy, see also `Accuracy()`
   - and more.
 - **Choice rules**  `choicerule()` is a general function to apply choice rules (action selection rules) to predictions, e.g.,
   - Soft maximum, see also`cr_softmax()`
   - Luce's rule, see also `cr_luce()`
   - Argmax, see also `cr_argmax()`
   - Epsilon greedy, see also `cr_epsilon()`
   - and more.
  - **Demographics** `participants()` is a function to nicely print demographic summary sections of your data

# Installation
## First time installation
```R
library(devtools) #maybe install.packages('devtools')
devtools::install_github('janajarecki/cogsciutils')
```
## Usage
```R
library(cogsciutils)
```

### Participants
The `paricipants()` function summarizes demographic data and prints a text like this
```R
participants(mydata, id = "id", age = "age", gender = "fem", excl = 0)
```
> In total ten participants completed the study (zero were excluded), 6 males and 4 females (60% and 40%, respectively), mean age 50 years (med = 51, sd = 18, range 21-73 years).
#### Details of participants()
```R
# Let's make sample data
set.seed(232)
N <- 10
mydata <- data.frame(id = 1:N,
                    age = sample(18:75, N),
                    fem = sample(c("m","f"), N, T))
head(mydata, 3)
#   id age fem
# 1  1  73   f
# 2  2  71   m
# 3  3  23   f
participants(mydata, id = "id", age = "age", gender = "fem", excl = 0)
```
#### Further usage
- Save the ouput: `file = "filename.txt"`
  ```R
  participants(mydata, "id", "age", "fem", excl = 0, file = "test.txt")
  ```
- Did you exclude participants? `excl = 2`
  ```R
  participants(mydata, "id", "age", "fem", excl = 2)
  # Or specify, for each participant, the reasons and NA if included
  mydata$exclude_because <- c("no variance on scales", "inattentiveness", rep(NA, 8))
  participants(mydata, "id", "age", "fem", excl = "exclude_because")
  ```  
- Print location(s): `collectedat = "..."`
  ```R
  participants(mydata, "id", "age", "fem", excl = 0, collectedat = "the University of Basel")
  participants(mydata, "id", "age", "fem", excl = 0, collectedat = "online")
  ```
- Add ethics approval: `approvedby = "..."` 
  ```R
  participants(mydata, "id", "age", "fem", excl = 0, approvedby = "the University of Basel ethics comittee")
  ```
- Add the time of data collection: `date = "variable_name"`
  ```R
  mydata$date <- Sys.Date() + 1:N # make a mock date variable
  participants(mydata, "id", "age", "fem", excl = 0, date = "date")
  ```
- Add a payoff summary: `compensation = "variable_name"`
  ```R
  mydata$payout <- runif(N) * 5 # Payout
  participants(mydata, "id", "age", "fem", excl = 0, compensation = "payout")
  participants(mydata, "id", "age", "fem", excl = 0, compensation = "payout", currency = "EUR")
  ```
- Add further variables to be summarized: `more = list(x = c(...))`
  ```R
  mydata$duration_min <- sample(20:30,N) # study duration
  mydata$income_eur <- c(sample(1200:4000,N-1), NA) # we have one missing value!
  mydata$income_categ <- c(sample(c("1000-2000", "2000-3000", "> 3000"), N, T))
  # use the more argument
  # The format is  a list of vectors c("what to print" = c("variable_name", "unit"))
  participants(mydata, "id", "age", "fem", excl = 0,
    more = list("completion duration was" = c("duration_min", "minute")))
  participants(mydata, "id", "age", "fem", excl = 0,
    more = list("completion duration was" = c("duration_min", "minute"),
                "income was" = c("income_eur", "EUR")))
  participants(mydata, "id", "age", "fem", excl = 0,
    more = list("completion duration was" = c("duration_min", "minute"),
                "income categories were" = c("income_categ", "EUR")))
  ```
- Alltogether
  ```R
  participants(mydata, "id", "age", "fem",
    excl = "exclude_because", date ="date",
    compensation = "payout", currency = "EUR",
    recruitedfrom ="Amazon MTurk", collectedat ="online",
    approvedby = "the IRB at the University of Basel", 
    more = list("completion duration was" = c("duration_min", "minute"),
                "income categories were" = c("income_categ", "EUR")))
  ```
  > In total ten participants recruited from Amazon MTurk completed an online study, two were excluded (for no variance on scales and inattentiveness), leaving a final sample of N = 8; 5 males and 3 females (62% and 38%, respectively), mean age 53 years (med = 50, sd = 13, range 35-73 years), mean remuneration was 3.2 EUR (med = 3.2, sd = 0.9, range 1.7-4.3 EUR), data were collected in March 2019, the study was approved by the IRB at the University of Basel. Mean completion duration was 25.1 minutes (med = 25.5, sd = 3.0, range 20.0-29.0 minutes); income categories were n = 4 > 3000 and 4 2000-3000 (50% and 50%, respectively). 
> 

### Choice rules
#### Binary data: from a vector
Calculate Luce choice rule
```R
library(cogsciutils)
# Binary predictions
binaryPredictions <- c(.22, .5, .73)
choicerule(binaryPredictions, "softmax", tau = 2)
choicerule(binaryPredictions, "argmax")
choicerule(binaryPredictions, "epsilon", eps = .2)
```

#### Predictions for three or more variables: from a matrix
```R
# Make some predictions for three variables A, B, C
predictions <- cbind(A = c(.1,.5,.4), B = c(.3,.1,.4), C = c(.6, .4, .2))
choicerule(predictions, "luce")
choicerule(predictions, "argmax")
choicerule(predictions, "epsilon", eps = .2)
```

### Goodness of fit
#### Fit from raw observed data
Calculate the log likelihood for some observatinos
```R
library(cogsciutils)

# N = 100 observations
set.seed(121)
x <- sample(0:1, 100, rep = T) # observations of 0s and 1s
y <- rep(0.55, 100) # predicting to observe 1 with probability 55 %
gof(obs = x, pred = y, type = "loglik") # log likelihood
gof(obs = x, pred = y, type = "mse") # mean squared error, average squared deviations
gof(obs = x, pred = y, type = "sse") # sum of squared errors, sum of squared deviations
gof(obs = x, pred = y, type = "loglik", saturated = TRUE) # saturated log lik

# Ignore first 5 observations
gof(obs = x, pred = y, type = "loglik", discount = 5)
gof(obs = x, pred = y, type = "loglik", saturated = TRUE, discount = 5) # saturated log lik
```

#### Fit from proportions observed of responses
Calculate the fit from the aggregated relative frequencies of responses. In this case you can also calculate the weighted sum of squares `wsse` and weighted mean squared error `wmse`. Weighting means predictions close to 0.5 are discounted in the fit, and predictions closer to 0 and 1 are more important. Weighting is done by dividing through the variance (var = p * (1-p)) of each unique prediction.
```R
library(cogsciutils)

# Sum of squared errors (SSE) between predictions and observations
x <- c(.96, .78) # observations as proportions of responses (aggregated)
y <- c(.85, .95) # predictions for the response

# How is the goodness of fit?
gof(obs = x, pred = y, type = "loglik") # log likelihood
gof(obs = x, pred = y, type = "mse") # mean squared deviations
gof(obs = x, pred = y, type = "sse") # sum of squared deviations

# If you want the weighted SSE the function needs to know
# how many data points underly the observed proportions
n <- c(100, 100)
gof(obs = x, pred = y, type = "wmse", n = n) # mean squared deviations
gof(obs = x, pred = y, type = "wsse", n = n) # sum of squared deviations
```
