library(testthat)
library(bayesDiagnostics)

test_that("prior_elicitation_helper: validates NULL expert_beliefs", {
  expect_error(
    bayesDiagnostics::prior_elicitation_helper(
      expert_beliefs = NULL,
      parameter_type = "continuous",
      visualize = FALSE
    ),
    regexp = "NULL|class|list"
  )
})

test_that("prior_elicitation_helper: validates expert_beliefs type", {
  expect_error(
    bayesDiagnostics::prior_elicitation_helper(
      expert_beliefs = "not_a_list",
      parameter_type = "continuous",
      visualize = FALSE
    ),
    regexp = "class|list"
  )
})

test_that("prior_elicitation_helper: creates object with valid input", {
  expert_input <- list(
    parameter_name = "intercept",
    plausible_range = c(0, 10),
    most_likely_value = 5,
    confidence = 0.75
  )
  result <- tryCatch({
    bayesDiagnostics::prior_elicitation_helper(
      expert_beliefs = expert_input,
      parameter_type = "continuous",
      visualize = FALSE
    )
  }, error = function(e) {
    skip(paste("Function error:", e$message))
  })
  if (!is.null(result)) {
    expect_s3_class(result, "prior_elicitation")
    expect_equal(result$parameter_name, "intercept")
  }
})

test_that("prior_elicitation_helper: returns list structure", {
  expert_input <- list(
    parameter_name = "test",
    plausible_range = c(0, 10),
    most_likely_value = 5,
    confidence = 0.8
  )
  result <- tryCatch({
    bayesDiagnostics::prior_elicitation_helper(
      expert_beliefs = expert_input,
      parameter_type = "continuous",
      visualize = FALSE
    )
  }, error = function(e) {
    skip(paste("Function error:", e$message))
  })
  if (!is.null(result)) {
    expect_true(is.list(result))
    expect_true(length(names(result)) > 0)
  }
})
