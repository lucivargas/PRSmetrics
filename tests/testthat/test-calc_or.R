# tests/testthat/test-calc_OR.R
library(testthat)
library(PRSmetrics)  # Replace with your package name

test_that("calc_OR returns a tibble with expected columns", {

  # Simulate PGS dataset
  set.seed(123)
  n <- 10000
  pgs <- rnorm(n, mean = 0, sd = 4)
  beta <- 0.3
  mu <- qlogis(0.3)
  prob_case <- plogis(mu + beta * pgs)
  outcome <- rbinom(n, size = 1, prob = prob_case)

  # Run the function
  res <- calc_or(pgs, outcome)

  # Check class
  expect_type(res, "list")

  # Check column names
  expect_named(res, c("results", "quantiles", "plot"))

  # Check table
  expect_s3_class(res$results, "tbl_df")
  expect_true(all(c("quantile", "OR", "OR_ci1", "OR_ci2", "pval") %in% colnames(res$results)))

  # Check that number or quantiles match number of rows in result
  expect_type(res$quantiles, "double")
  expect_equal(length(res$quantiles)-1, nrow(res$results))

  # Check that OR values are numeric
  expect_type(res$results$OR, "double")
  expect_type(res$results$pval, "double")

})
