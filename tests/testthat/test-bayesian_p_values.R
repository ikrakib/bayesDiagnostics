test_that("bayesian_p_values: basic computation", {
  y <- c(1, 2, 3, 4, 5)
  yrep <- matrix(c(
    1, 2, 3, 4, 5,  # Same as y
    2, 3, 4, 5, 6,  # Higher
    0, 1, 2, 3, 4   # Lower
  ), nrow = 3, byrow = TRUE)

  res <- bayesian_p_values(yrep, y, statistic = mean)

  expect_type(res, "list")
  expect_true("observed_value" %in% names(res))
  expect_true("replicated_values" %in% names(res))
  expect_true("p_value" %in% names(res))
})

test_that("bayesian_p_values: mean statistic", {
  y <- c(1, 2, 3)
  yrep <- matrix(c(
    1, 2, 3,  # mean = 2
    2, 3, 4,  # mean = 3
    0, 1, 2   # mean = 1
  ), nrow = 3, byrow = TRUE)

  res <- bayesian_p_values(yrep, y, statistic = mean)

  expect_equal(res$observed_value, 2)
  expect_equal(res$replicated_values, c(2, 3, 1))
  expect_equal(res$p_value, 2/3) # Two means >= 2 out of three
})

test_that("bayesian_p_values: max statistic", {
  y <- c(1, 2, 3)
  yrep <- matrix(c(
    1, 2, 3,   # max = 3
    2, 3, 4,   # max = 4
    0, 1, 2    # max = 2
  ), nrow = 3, byrow = TRUE)

  res <- bayesian_p_values(yrep, y, statistic = max)

  expect_equal(res$observed_value, 3)
  # Obs max = 3. Rep maxes = 3, 4, 2.
  # P-value = mean(c(3,4,2) >= 3) = mean(c(TRUE, TRUE, FALSE)) = 2/3
  expect_equal(res$p_value, 2/3)
})

test_that("bayesian_p_values: sd statistic", {
  y <- c(1, 2, 3)
  yrep <- matrix(c(
    1, 2, 3,
    2, 3, 4,
    0, 1, 2
  ), nrow = 3, byrow = TRUE)

  res <- bayesian_p_values(yrep, y, statistic = sd)

  # expect_numeric is from checkmate, not testthat. Use expect_type(..., "double")
  expect_type(res$observed_value, "double")
  expect_length(res$observed_value, 1)

  expect_type(res$replicated_values, "double")
  expect_length(res$replicated_values, 3)

  expect_type(res$p_value, "double")
  expect_length(res$p_value, 1)
})

test_that("bayesian_p_values: p_value is between 0 and 1", {
  y <- rnorm(10)
  yrep <- matrix(rnorm(50), nrow = 5)

  res <- bayesian_p_values(yrep, y, statistic = mean)

  expect_gte(res$p_value, 0)
  expect_lte(res$p_value, 1)
})

test_that("bayesian_p_values: custom function", {
  y <- c(1, 2, 3, 4, 5)
  yrep <- matrix(c(
    1, 2, 3, 4, 5,
    2, 3, 4, 5, 6,
    0, 1, 2, 3, 4
  ), nrow = 3, byrow = TRUE)

  # Custom function: range
  custom_stat <- function(x) max(x) - min(x)

  res <- bayesian_p_values(yrep, y, statistic = custom_stat)

  expect_equal(res$observed_value, 4) # max(y) - min(y) = 5 - 1 = 4
  expect_type(res$replicated_values, "double")
  expect_length(res$replicated_values, 3)
})

test_that("bayesian_p_values: handles constant data", {
  y <- c(5, 5, 5, 5)
  yrep <- matrix(c(
    5, 5, 5, 5,
    4, 4, 4, 4,
    6, 6, 6, 6
  ), nrow = 3, byrow = TRUE)

  res <- bayesian_p_values(yrep, y, statistic = mean)

  expect_equal(res$observed_value, 5)
  expect_equal(res$p_value, 2/3) # Two means >= 5
})

test_that("bayesian_p_values: validates input - must be matrix", {
  y <- c(1, 2, 3)
  yrep <- data.frame(1:5)

  expect_error(bayesian_p_values(yrep, y, statistic = mean))
})

test_that("bayesian_p_values: validates input - must be numeric", {
  y <- c("a", "b", "c")
  yrep <- matrix(1:9, nrow = 3)

  expect_error(bayesian_p_values(yrep, y, statistic = mean))
})

test_that("bayesian_p_values: validates input - function required", {
  y <- c(1, 2, 3)
  yrep <- matrix(1:9, nrow = 3)

  expect_error(bayesian_p_values(yrep, y, statistic = "mean"))
})

test_that("bayesian_p_values: extreme p-values", {
  y <- c(100, 100, 100)  # Very high values
  yrep <- matrix(c(
    1, 1, 1,
    2, 2, 2,
    3, 3, 3
  ), nrow = 3, byrow = TRUE)

  res <- bayesian_p_values(yrep, y, statistic = mean)

  expect_equal(res$p_value, 0) # No replicated means >= 100
})

test_that("bayesian_p_values: large yrep matrix", {
  y <- rnorm(100)
  yrep <- matrix(rnorm(1000), nrow = 100)

  res <- bayesian_p_values(yrep, y, statistic = mean)

  expect_type(res, "list")
  expect_type(res$p_value, "double")
})
