library(testthat)
library(bayesDiagnostics)

test_that("bayesian_p_values: calculates p-value correctly", {
  set.seed(789)
  yrep <- matrix(rnorm(1000, mean = 0, sd = 1), nrow = 100, ncol = 10)
  y <- rnorm(10)
  result <- bayesDiagnostics::bayesian_p_values(yrep = yrep, y = y, statistic = mean)
  expect_true(is.list(result))
  expect_true(all(c("observed_value", "replicated_values", "p_value") %in% names(result)))
  expect_true(result$p_value >= 0 && result$p_value <= 1)
})

test_that("bayesian_p_values: works with different statistics", {
  set.seed(101)
  yrep <- matrix(rnorm(1000, mean = 0, sd = 1), nrow = 100, ncol = 10)
  y <- rnorm(10)
  result_max <- bayesDiagnostics::bayesian_p_values(yrep = yrep, y = y, statistic = max)
  result_min <- bayesDiagnostics::bayesian_p_values(yrep = yrep, y = y, statistic = min)
  expect_true(result_max$p_value >= 0 && result_max$p_value <= 1)
  expect_true(result_min$p_value >= 0 && result_min$p_value <= 1)
})

test_that("bayesian_p_values: returns correct data types", {
  set.seed(202)
  yrep <- matrix(rnorm(1000), nrow = 100, ncol = 10)
  y <- rnorm(10)
  result <- bayesDiagnostics::bayesian_p_values(yrep, y, mean)
  expect_type(result$observed_value, "double")
  expect_type(result$p_value, "double")
  expect_equal(length(result$replicated_values), nrow(yrep))
})
