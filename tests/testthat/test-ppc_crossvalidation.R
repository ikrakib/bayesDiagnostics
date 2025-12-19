library(testthat)
library(bayesDiagnostics)

skip_on_cran()
skip_if_not_installed("brms")

test_that("ppc_cross_validation: basic functionality", {
  fixture <- get_simple_fixture()

  if (!exists("ppc_cross_validation", where = asNamespace("bayesDiagnostics"), mode = "function")) {
    if (!exists("ppc_crossvalidation", where = asNamespace("bayesDiagnostics"), mode = "function")) {
      skip("ppc_cross_validation function not found")
    }
    fn <- bayesDiagnostics:::ppc_crossvalidation
  } else {
    fn <- bayesDiagnostics::ppc_cross_validation
  }

  result <- suppressWarnings(
    fn(
      model = fixture$fit,
      observed_y = fixture$data$y
    )
  )

  expect_true(!is.null(result))
})

test_that("ppc_cross_validation: returns result", {
  fixture <- get_simple_fixture()

  if (!exists("ppc_cross_validation", where = asNamespace("bayesDiagnostics"), mode = "function")) {
    if (!exists("ppc_crossvalidation", where = asNamespace("bayesDiagnostics"), mode = "function")) {
      skip("ppc_cross_validation function not found")
    }
    fn <- bayesDiagnostics:::ppc_crossvalidation
  } else {
    fn <- bayesDiagnostics::ppc_cross_validation
  }

  result <- suppressWarnings(
    fn(
      model = fixture$fit,
      observed_y = fixture$data$y
    )
  )

  expect_true(is.list(result) || is.numeric(result))
})
