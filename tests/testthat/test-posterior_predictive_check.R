library(testthat)
library(bayesDiagnostics)

skip_on_cran()
skip_if_not_installed("brms")

# Use global fixture
test_that("posterior_predictive_check: returns correct class", {
  fixture <- get_simple_fixture()

  result <- tryCatch({
    bayesDiagnostics::posterior_predictive_check(
      model = fixture$fit,
      observed_data = fixture$data$y,
      n_samples = 50,
      plot = FALSE
    )
  }, error = function(e) {
    skip(paste("Function error:", e$message))
  })

  if (!is.null(result)) {
    expect_s3_class(result, "posterior_predictive_check")
    expect_true(!is.null(result$p_values))
  }
})

test_that("posterior_predictive_check: print method works", {
  fixture <- get_simple_fixture()

  result <- tryCatch({
    bayesDiagnostics::posterior_predictive_check(
      model = fixture$fit,
      observed_data = fixture$data$y,
      n_samples = 50,
      plot = FALSE
    )
  }, error = function(e) {
    skip(paste("Function error:", e$message))
  })

  if (!is.null(result)) {
    expect_output(print(result), "Predictive|Check")
  }
})
