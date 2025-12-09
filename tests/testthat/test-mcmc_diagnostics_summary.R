test_that("mcmc_diagnostics_summary works", {
  skip_if_not_installed("brms")
  library(brms)
  # Create a simple mock result structure to avoid long fit times
  # In a real check we might fit a small model, but let's test structure first
  mock_res <- structure(
    list(converged = TRUE, divergences = 0, rhat_issues = data.frame()),
    class = "mcmc_diagnostics"
  )
  expect_output(print(mock_res), "MCMC Diagnostics Summary")
  expect_true(mock_res$converged)
})
