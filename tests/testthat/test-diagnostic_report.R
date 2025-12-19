library(testthat)
library(bayesDiagnostics)

skip_on_cran()
skip_if_not_installed("brms")

test_that("diagnostic_report: generates report without errors", {
  fixture <- get_simple_fixture()
  tmpfile <- tempfile(fileext = ".pdf")

  result <- bayesDiagnostics::diagnostic_report(
    model = fixture$fit,
    output_file = tmpfile,
    output_format = "pdf",
    include_sections = c("model_summary", "convergence"),
    open_report = FALSE
  )

  expect_true(file.exists(tmpfile))
  if (file.exists(tmpfile)) {
    file.remove(tmpfile)
  }
})

test_that("diagnostic_report: includes all sections", {
  fixture <- get_simple_fixture()
  tmpfile <- tempfile(fileext = ".pdf")

  result <- bayesDiagnostics::diagnostic_report(
    model = fixture$fit,
    output_file = tmpfile,
    output_format = "pdf",
    include_sections = c("model_summary", "convergence", "posterior_summary"),
    open_report = FALSE
  )

  expect_true(file.exists(tmpfile))
  if (file.exists(tmpfile)) {
    file.remove(tmpfile)
  }
})

test_that("diagnostic_report: handles custom output", {
  fixture <- get_simple_fixture()
  tmpfile <- tempfile(fileext = ".pdf")

  result <- bayesDiagnostics::diagnostic_report(
    model = fixture$fit,
    output_file = tmpfile,
    output_format = "pdf",
    include_sections = c("model_summary"),
    open_report = FALSE
  )

  expect_true(file.exists(tmpfile))
  if (file.exists(tmpfile)) {
    file.remove(tmpfile)
  }
})
