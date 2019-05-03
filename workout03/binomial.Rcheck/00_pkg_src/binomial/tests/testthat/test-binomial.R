library(testthat)

#source("R/binom_funcs.R")


context("Checks the checker methods")

test_that("Checks 0 <= prob <= 1", {
  expect_equal(check_prob(0.5), TRUE)
  #expect_error(check_prob(1.1))
  #expect_error(check_prob(-0.5))
  expect_length(check_prob(1), 1)
})


test_that("Checks trials are non negative integers", {
  expect_equal(check_trials(1), TRUE)
  expect_error(check_trials(-1))
  expect_type(check_trials(1), "logical")
})


test_that("Checks successes are non negative integers that are less than or equal to the number of trials",{
  expect_equal(check_success(success = 1, trials = 2), TRUE)
  expect_length(check_success(success = 2, trials = 3), 1)
  expect_error(check_success(success = 3, trials = 2))
  
})

context("Checks the auxiliary methods")

test_that("Checks the mean of the binomial distribution is correct",{
  expect_equal(aux_mean(trials = 10, prob = 0.3), 3)
  expect_type(aux_mean(trials = 10, prob = 0.5), "double")
  expect_length(aux_mean(trials = 3, prob = 0.5), 1)
})

test_that("Checks the variance of the binomial distribution is correct",{
  expect_equal(aux_variance(trials = 10, prob = 0.3), 2.1)
  expect_type(aux_variance(trials = 10, prob = 0.5), "double")
  expect_length(aux_variance(trials = 3, prob = 0.5), 1)
})

test_that("Checks the mode of the binomial distribution is correct",{
  expect_equal(aux_mode(trials = 10, prob = 0.3), 3)
  expect_type(aux_mode(trials = 10, prob = 0.5), "double")
  expect_length(aux_mode(trials = 3, prob = 0.5), 1)
})

test_that("Checks the skewness of the binomial distribution is correct",{
  expect_equal(aux_skewness(trials = 10, prob = 0.3), 0.2760262, tolerance = 1e-5)
  expect_type(aux_skewness(trials = 10, prob = 0.5), "double")
  expect_length(aux_skewness(trials = 3, prob = 0.5), 1)
  
})

test_that("Checks the kurtosis of the binomial distribution is correct",{
  expect_equal(aux_kurtosis(trials = 10, prob = 0.3), -0.1238095, tolerance = 1e-05)
  expect_type(aux_kurtosis(trials = 10, prob = 0.5), "double")
  expect_length(aux_kurtosis(trials = 3, prob = 0.5), 1)
})


context("Checks bionmial methods")

test_that("Checks bin_choose() returns the number of ways there can be k successes in n trials",{
  expect_equal(bin_choose(n = 5, k = 2), 10)
  expect_equal(bin_choose(n = 5, k = 1:3), c(5, 10, 10))
  expect_type(bin_choose(n = 5, k = 1), "double")
})

test_that("Checks bin_probability() returns the probability of a binomial distribution given the number of successes, tirals and the probability for success",{
  expect_equal(bin_probability(success = 2, trials = 5, prob = 0.5), 0.3125)
  expect_equal(bin_probability(success = 0:2, trials = 5, prob = 0.5), c(0.03125, 0.15625, 0.31250))
  expect_type(bin_probability(success = 55, trials = 100, prob = 0.45), "double")
})

test_that("Checks bin_distribution() returns the correct binomial distribution", {
  dist1 <- data.frame(success = c(0:5), probability = c(0.03125, 0.15625, 0.31250, 0.31250, 0.15625, 0.03125))
  class(dist1) <- c("bindis", "data.frame")
  expect_equal(bin_distribution(trials = 5, prob = 0.5), dist1)
  expect_s3_class(bin_distribution(trials = 5, prob = 0.5), "bindis")
  expect_error(bin_distribution(trials = 5, prob = -0.5))
})

test_that("Checks bin_cumulative() returns the correct binomial distribution and cumulative probabilities",{
  dist2 <- data.frame(success = c(0:5), probability = c(0.03125, 0.15625, 0.31250, 0.31250, 0.15625, 0.03125), cumulative = c(0.03125, 0.18750, 0.50000, 0.81250, 0.96875, 1.00000))
  class(dist2) <- c("bincum", "data.frame")
  expect_equal(bin_cumulative(trials = 5, prob = 0.5), dist2)
  expect_s3_class(bin_cumulative(trials = 5, prob = 0.5), c("bincum", "data.fram."))
  expect_error(bin_cumulative(trials = -1, prob = 0.5))
})
