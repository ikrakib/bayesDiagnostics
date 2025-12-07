# Tests for prior_elicitation_helper function

test_that("prior_elicitation_helper validates inputs correctly", {
  # Test invalid parameter type
  expect_error(
    prior_elicitation_helper(
      expert_beliefs = list(
        parameter_name = "test",
        plausible_range = c(-2, 2),
        most_likely_value = 0,
        confidence = 0.8
      ),
      parameter_type = "invalid_type"
    )
  )

  # Test invalid method
  expect_error(
    prior_elicitation_helper(
      expert_beliefs = list(
        parameter_name = "test",
        plausible_range = c(-2, 2),
        most_likely_value = 0,
        confidence = 0.8
      ),
      method = "invalid_method"
    )
  )
})

test_that("prior_elicitation_helper creates object correctly", {
  expert_input <- list(
    parameter_name = "effect_size",
    plausible_range = c(-2, 2),
    most_likely_value = 0.5,
    confidence = 0.8
  )

  result <- prior_elicitation_helper(
    expert_beliefs = expert_input,
    parameter_type = "continuous",
    method = "quantile",
    visualize = FALSE
  )

  # Check object class
  expect_s3_class(result, "prior_elicitation")

  # Check required components exist
  expect_true(!is.null(result$parameter_name))
  expect_true(!is.null(result$recommended_prior))
  expect_true(!is.null(result$alternatives))
  expect_true(!is.null(result$sensitivity_note))
})

test_that("prior_elicitation_helper captures expert beliefs", {
  expert_input <- list(
    parameter_name = "intercept",
    plausible_range = c(0, 10),
    most_likely_value = 5,
    confidence = 0.75
  )

  result <- prior_elicitation_helper(
    expert_beliefs = expert_input,
    parameter_type = "continuous",
    visualize = FALSE
  )

  # Check that expert inputs are preserved
  expect_equal(result$parameter_name, "intercept")
  expect_equal(result$parameter_summary$most_likely, 5)
  expect_equal(result$parameter_summary$confidence, 0.75)
  expect_equal(
    result$parameter_summary$plausible_range,
    c(0, 10)
  )
})

test_that("prior_elicitation_helper handles continuous parameters", {
  expert_input <- list(
    parameter_name = "coefficient",
    plausible_range = c(-5, 5),
    most_likely_value = 0,
    confidence = 0.9
  )

  result <- prior_elicitation_helper(
    expert_beliefs = expert_input,
    parameter_type = "continuous",
    visualize = FALSE
  )

  # Check that prior is recommended
  expect_true(!is.null(result$recommended_prior))

  # Check alternatives provided
  expect_true(length(result$alternatives) >= 2)
})

test_that("prior_elicitation_helper handles discrete parameters", {
  expert_input <- list(
    parameter_name = "count",
    plausible_range = c(0, 100),
    most_likely_value = 50,
    confidence = 0.8
  )

  result <- prior_elicitation_helper(
    expert_beliefs = expert_input,
    parameter_type = "discrete",
    visualize = FALSE
  )

  # Check object is created
  expect_s3_class(result, "prior_elicitation")
  expect_equal(result$parameter_summary$parameter_type, "discrete")
})

test_that("prior_elicitation_helper handles proportion parameters", {
  expert_input <- list(
    parameter_name = "success_probability",
    plausible_range = c(0, 1),
    most_likely_value = 0.6,
    confidence = 0.85
  )

  result <- prior_elicitation_helper(
    expert_beliefs = expert_input,
    parameter_type = "proportion",
    visualize = FALSE
  )

  # Check object is created
  expect_s3_class(result, "prior_elicitation")
  expect_equal(result$parameter_summary$parameter_type, "proportion")
})

test_that("prior_elicitation_helper generates alternatives", {
  expert_input <- list(
    parameter_name = "effect",
    plausible_range = c(-1, 1),
    most_likely_value = 0.2,
    confidence = 0.7
  )

  result <- prior_elicitation_helper(
    expert_beliefs = expert_input,
    parameter_type = "continuous",
    visualize = FALSE
  )

  # Should have at least 3 alternatives
  expect_true(length(result$alternatives) >= 3)

  # Alternatives should be named
  expect_true(length(names(result$alternatives)) > 0)
})

test_that("prior_elicitation_helper provides sensitivity guidance", {
  expert_input <- list(
    parameter_name = "slope",
    plausible_range = c(-2, 2),
    most_likely_value = 0,
    confidence = 0.8
  )

  result <- prior_elicitation_helper(
    expert_beliefs = expert_input,
    parameter_type = "continuous",
    visualize = FALSE
  )

  # Check sensitivity note exists and is informative
  expect_type(result$sensitivity_note, "character")
  expect_true(nchar(result$sensitivity_note) > 0)
  expect_true(grepl("sensitivity", tolower(result$sensitivity_note)))
})

test_that("prior_elicitation_helper respects confidence levels", {
  # High confidence
  expert_input_high <- list(
    parameter_name = "test",
    plausible_range = c(-1, 1),
    most_likely_value = 0,
    confidence = 0.95
  )

  result_high <- prior_elicitation_helper(
    expert_beliefs = expert_input_high,
    parameter_type = "continuous",
    visualize = FALSE
  )

  # Low confidence
  expert_input_low <- list(
    parameter_name = "test",
    plausible_range = c(-1, 1),
    most_likely_value = 0,
    confidence = 0.5
  )

  result_low <- prior_elicitation_helper(
    expert_beliefs = expert_input_low,
    parameter_type = "continuous",
    visualize = FALSE
  )

  # Both should be valid prior_elicitation objects
  expect_s3_class(result_high, "prior_elicitation")
  expect_s3_class(result_low, "prior_elicitation")
})

test_that("prior_elicitation_helper handles different methods", {
  expert_input <- list(
    parameter_name = "param",
    plausible_range = c(0, 10),
    most_likely_value = 5,
    confidence = 0.8
  )

  # Test quantile method
  result_quantile <- prior_elicitation_helper(
    expert_beliefs = expert_input,
    method = "quantile",
    visualize = FALSE
  )
  expect_equal(result_quantile$expert_method, "quantile")

  # Test histogram method
  result_histogram <- prior_elicitation_helper(
    expert_beliefs = expert_input,
    method = "histogram",
    visualize = FALSE
  )
  expect_equal(result_histogram$expert_method, "histogram")

  # Test interactive method
  result_interactive <- prior_elicitation_helper(
    expert_beliefs = expert_input,
    method = "interactive",
    visualize = FALSE
  )
  expect_equal(result_interactive$expert_method, "interactive")
})

test_that("print.prior_elicitation works correctly", {
  expert_input <- list(
    parameter_name = "beta",
    plausible_range = c(-3, 3),
    most_likely_value = 0.5,
    confidence = 0.8
  )

  result <- prior_elicitation_helper(
    expert_beliefs = expert_input,
    parameter_type = "continuous",
    visualize = FALSE
  )

  # Check that print output contains key information
  expect_output(print(result), "Prior Elicitation Results")
  expect_output(print(result), "beta")
})

test_that("prior_elicitation_helper handles data_sample parameter", {
  skip_if_not_installed("ggplot2")

  expert_input <- list(
    parameter_name = "location",
    plausible_range = c(0, 100),
    most_likely_value = 50,
    confidence = 0.8
  )

  # With data sample (should attempt visualization)
  data_sample <- rnorm(100, mean = 50, sd = 10)

  result <- prior_elicitation_helper(
    expert_beliefs = expert_input,
    parameter_type = "continuous",
    data_sample = data_sample,
    visualize = TRUE
  )

  # Check object is created
  expect_s3_class(result, "prior_elicitation")
})

test_that("prior_elicitation_helper handles bounded parameters", {
  expert_input <- list(
    parameter_name = "rate",
    plausible_range = c(0.1, 10),  # Positive bounded
    most_likely_value = 1,
    confidence = 0.8
  )

  result <- prior_elicitation_helper(
    expert_beliefs = expert_input,
    parameter_type = "continuous",
    visualize = FALSE
  )

  # Should handle positive bounds appropriately
  expect_s3_class(result, "prior_elicitation")
  expect_true(!is.null(result$recommended_prior))
})

test_that("prior_elicitation_helper stores elicitation timestamp", {
  expert_input <- list(
    parameter_name = "test",
    plausible_range = c(-1, 1),
    most_likely_value = 0,
    confidence = 0.8
  )

  result <- prior_elicitation_helper(
    expert_beliefs = expert_input,
    parameter_type = "continuous",
    visualize = FALSE
  )

  # Should have timestamp
  expect_true(!is.null(result$elicitation_time))
  expect_true(inherits(result$elicitation_time, "POSIXct"))
})
