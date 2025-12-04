library(testthat)

library(MATH4753F25masonsharum)

context("Testing myncurve function")

test_that("myncurve returns correct mu", {
  result <- myncurve(mu = 10, sigma = 2, a = 12)
  expect_equal(result$mu, 10)
})

test_that("myncurve returns correct sigma", {
  result <- myncurve(mu = 10, sigma = 2, a = 12)
  expect_equal(result$sigma, 2)
})

test_that("myncurve returns correct probability", {
  result <- myncurve(mu = 10, sigma = 2, a = 12)
  expected_prob <- round(pnorm(12, mean = 10, sd = 2), 4)
  expect_equal(round(result$probability, 4), expected_prob)
})

test_that("myncurve returns correct a", {
  result <- myncurve(mu = 10, sigma = 2, a = 12)
  expect_equal(result$a, 12)
})
