test_that("Binomial loglik values", {
  # Busemeyer & Diederich (2010)
  n <- 200
  o <- c(.9538, .9107, .9204, .9029, .8515, .9197, .7970, .8228, .8191, .7277, .7276)
  p <- c(.9526, .9168, .8721, .8229, .7736, .7277, .6871, .6523, .6232, .5993, .5798)
  or  <- rep(rep(0:1, 11), round(c(t(cbind(1-o, o))) * n))
  pr <- rep(p, each = n)
  ll <- -969.95 # Busemeyer p. 58
  expect_equal(loglikelihood(or, pr), ll, tol = .001)
  expect_equal(loglikelihood(o, p, n = n), ll, tol = .001)
  expect_equal(loglikelihood(o, p, n = rep(200, 11)), ll, tol = .001)
  expect_equal(loglikelihood(or, p, n = n), ll, tol = .001)

  # Myung (2003)
  o2 <- c(0.94,0.77,0.40,0.26,0.24,0.16) # p. 96
  p2mle <- 1.070 * exp(-0.131 * c(1,3,6,9,12,18)) # Table 1
  n2 <- 100
  ll <- -305.31 # Myung, Table 1
  expect_equal(loglikelihood(o2, p2mle, n = n2), ll, tol = .0001)
  expect_equal(loglikelihood(o2, p2mle, n = rep(n2, length(o2))), ll, tol = .0001)
})

test_that("Multinomial loglik values", {
  # Myung (2003)
  o2 <- c(0.94,0.77,0.40,0.26,0.24,0.16) # p. 96
  p2mle <- 1.070 * exp(-0.131 * c(1,3,6,9,12,18)) # Table 1
  expect_equal(loglikelihood(o2, p2mle, n = 100, pdf = "multi"), -305.31, tol = .0001) #result: # Myung, Table 1

  # own example
  ll <- dmultinom(cbind(1:3), NULL, cbind(.2,.3,.5), log=T) - log(factorial(6)) + log(factorial(3)) + log(factorial(2))
  expect_equal(loglikelihood(1:3, c(.2,.3,.5), pdf = "multi"), ll)
  expect_equal(loglikelihood(obs = rbind(c(1,2,3), c(1,2,3)), pred = rbind(c(.2,.3,.5), c(.2,.3,.5)), pdf = "multi"), 2 * ll)
})


test_that("Saturated loglikelihood values", {
  # Busemeyer & Diederich (2010)
  n <- 200
  o <- c(.9538, .9107, .9204, .9029, .8515, .9197, .7970, .8228, .8191, .7277, .7276)
  p <- c(.9526, .9168, .8721, .8229, .7736, .7277, .6871, .6523, .6232, .5993, .5798)
  or  <- rep(rep(0:1, 11), round(c(t(cbind(1-o, o))) * n))
  pr <- rep(p, each = n)
  ll <- -879.9013
  expect_equal(loglikelihood(or, o, n = n), ll, tol = .01)
  expect_equal(loglikelihood(o, n = n, saturated = TRUE), ll, tol = .01)
  expect_equal(loglikelihood(o, o, n = n, saturated = TRUE), ll, tol = .001)
})


test_that("loglikelihood input formats", { #todo
  expect_error(loglikelihood(obs = 1:2, pred = 1))
  expect_error(loglikelihood(obs = 1:2, pred = 1:2, pdf = "binomial"))
})