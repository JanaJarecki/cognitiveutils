Sys.setenv("R_TESTS" = "")
context("Goodness of fit measures")

test_that("Loglikelihood values", {
  # Busemeyer & Diederich (2010)
  n <- 200
  o <- c(.9538, .9107, .9204, .9029, .8515, .9197, .7970, .8228, .8191, .7277, .7276)
  p <- c(.9526, .9168, .8721, .8229, .7736, .7277, .6871, .6523, .6232, .5993, .5798)
  or  <- rep(rep(0:1, 11), round(c(t(cbind(1-o, o))) * n))
  pr <- rep(p, each = n)
  ll <- -969.95 # Busemeyer p. 58
  expect_equal(Loglikelihood(or, pr), ll, tol = .001)
  expect_equal(Loglikelihood(o, p, n = n), ll, tol = .001)
  expect_equal(Loglikelihood(o, p, n = rep(200, 11)), ll, tol = .001)
  expect_equal(Loglikelihood(or, p, n = n), ll, tol = .001)
  expect_equal(gof(or, pr), ll, tol = .001)
  expect_equal(gof(o, p, options = list(response = 'd'), n = n), ll, tol = .001)
  expect_equal(gof(or, p, options = list(response = 'd'), n = n), ll, tol = .001)
  expect_equal(gof(o, p, n = n), ll, tol = .001)
  expect_equal(gof(or, p, n = n), ll, tol = .001)
  # Myung (2003)
  o2 <- c(0.94,0.77,0.40,0.26,0.24,0.16) # p. 96
  p2mle <- 1.070 * exp(-0.131 * c(1,3,6,9,12,18)) # Table 1
  p2lse <- 1.092 * exp(-0.141 * c(1,3,6,9,12,18)) # Table 1
  n2 <- 100
  ll <- -305.31 # Myung, Table 1
  expect_equal(Loglikelihood(o2, p2mle, n = n2), ll, tol = .0001)
  expect_equal(Loglikelihood(o2, p2mle, n = rep(n2, length(o2))), ll, tol = .0001)
  expect_equal(gof(o2, p2mle, r = 'd', n = n2), ll, tol = .001) 
})

test_that("Saturated loglikelihood values", {
  # Busemeyer & Diederich (2010)
  n <- 200
  o <- c(.9538, .9107, .9204, .9029, .8515, .9197, .7970, .8228, .8191, .7277, .7276)
  p <- c(.9526, .9168, .8721, .8229, .7736, .7277, .6871, .6523, .6232, .5993, .5798)
  or  <- rep(rep(0:1, 11), round(c(t(cbind(1-o, o))) * n))
  pr <- rep(p, each = n)
  ll <- -879.9013
  expect_equal(Loglikelihood(or, o, n = n), ll, tol = .01)
  expect_equal(Loglikelihood(o, n = n, saturated = TRUE), ll, tol = .01)
  expect_equal(Loglikelihood(o, o, n = n, saturated = TRUE), ll, tol = .001)
  expect_equal(gof(o, o, n = n, options = list(r = 'd', saturated = TRUE)), ll, tol = .01)
})

test_that("Loglikelihood input formats", { #todo
  expect_error(Loglikelihood(obs = 1:2, pred = 1))
  expect_error(Loglikelihood(obs = 1:2, pred = 1:2, pdf = "binomial"))
  expect_error(gof(obs = 1:2, pred = 1:2))
  expect_error(gof(obs = 1:2, pred = 1))
})

test_that("SSE values", {
  # Busemeyer & Diederich (2010)
  n <- 200
  o <- c(.9538, .9107, .9204, .9029, .8515, .9197, .7970, .8228, .8191, .7277, .7276)
  p <- c(.9526, .9168, .8721, .8229, .7736, .7277, .6871, .6523, .6232, .5993, .5798)
  or  <- rep(rep(0:1, 11), round(c(t(cbind(1-o, o))) * n))
  pr <- rep(p, each = n)
  sse <- 0.1695 # Busemeyer p. 55
  expect_equal(SSE(o, p), sse, tol = .0001)
  expect_equal(SSE(or, p, n=n), sse, tol = .01)
  expect_equal(gof(o, p, 'sse'), sse, tol = .0001)
  expect_equal(gof(or, p, 'sse', n=n), sse, tol = .01)
  # Myung (2003)
  o2 <- c(0.94,0.77,0.40,0.26,0.24,0.16) # p. 96
  p2mle <- 1.070 * exp(-0.131 * c(1,3,6,9,12,18)) # Table 1
  p2lse <- 1.092 * exp(-0.141 * c(1,3,6,9,12,18)) # Table 1
  n2 <- 100
  sse <- 0.0169 # Myung Table 1
  expect_equal(SSE(o2, p2lse), sse, tol = .0001)
  expect_equal(gof(o2, p2lse, 'sse'), sse, tol = .0001)
})

test_that("MSE values", {
  expect_equal(MSE(c(1,1,1), c(.6,.7,.8)), 0.0966, tol = .001)
  expect_equal(gof(c(1,1,1), c(.6,.7,.8), 'mse'), 0.0966, tol = .001)
})

test_that("RMSE values", {
  # Busemeyer & Diederich (2010)
  n <- 200
  o <- c(.9538, .9107, .9204, .9029, .8515, .9197, .7970, .8228, .8191, .7277, .7276)
  p <- c(.9526, .9168, .8721, .8229, .7736, .7277, .6871, .6523, .6232, .5993, .5798)
  wsse <- 158.4059 # Busemeyer, p. 56
  expect_equal(SSE(o, p, weighted = TRUE, n = n), wsse, tol = .001)
  expect_equal(gof(o, p, 'wsse', n = n), wsse, tol = .001)
})

test_that("Accuracy values", {
  expect_equal(Accuracy( c(1,0,0), c(1,1,1)), 0.333, tol = .001)
  expect_equal(Accuracy( c(1,0,0), c(.6,.7,1)), 0.333, tol = .001)
  expect_equal(gof(c(1,0,0), c(1,1,1), 'accuracy'), 0.333, tol = .001)
  expect_equal(gof(c(1,0,0), c(.6,.7,1), 'accuracy'), 0.333, tol = .001)
})