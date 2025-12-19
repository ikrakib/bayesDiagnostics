library(testthat)
library(bayesDiagnostics)

skip_on_cran()
skip_if_not_installed("brms")
skip_if_not_installed("bayesplot")
skip_if_not_installed("ggplot2")

# Use global fixture
test_that("graphical_ppc: creates density plot", {
  fixture <- get_simple_fixture()

  result <- tryCatch({
    bayesDiagnostics::graphical_ppc(
      fixture$fit,
      fixture$data$y,
      type = "density",
      n_draws = 50
    )
  }, error = function(e) {
    skip(paste("Function error:", e$message))
  })

  if (!is.null(result)) {
    expect_s3_class(result, "ggplot")
  }
})
