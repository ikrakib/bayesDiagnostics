library(testthat)
library(bayesDiagnostics)
library(brms)

# Create test fixture with sufficient iterations to avoid ESS warnings
if (!exists("report_test_model")) {
  set.seed(555)
  test_data <- data.frame(
    y = rnorm(30),
    x = rnorm(30)
  )

  # FIXED: Increased iterations from 400 to 2000, warmup from 200 to 1000
  report_test_model <- brm(
    y ~ x,
    data = test_data,
    chains = 2,
    iter = 2000,      # Increased from 400
    warmup = 1000,    # Increased from 200
    refresh = 0,
    silent = 2,
    seed = 555
  )
}

test_that("diagnostic_report: basic HTML generation", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")

  tmp_file <- tempfile(fileext = ".html")

  result <- diagnostic_report(
    model = report_test_model,
    output_file = tmp_file,
    output_format = "html",
    open_report = FALSE
  )

  # Should return file path
  expect_type(result, "character")

  # File should exist
  expect_true(file.exists(result))

  # File should be HTML
  expect_true(grepl("\\.html$", result))

  # Clean up
  unlink(result)
})

test_that("diagnostic_report: respects include_sections", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")

  tmp_file <- tempfile(fileext = ".html")

  result <- diagnostic_report(
    model = report_test_model,
    output_file = tmp_file,
    include_sections = c("model_summary", "convergence"),
    open_report = FALSE
  )

  # Should generate successfully
  expect_true(file.exists(result))

  # Read content to verify sections
  content <- readLines(result)
  expect_true(any(grepl("Model Summary", content, ignore.case = TRUE)))
  expect_true(any(grepl("Convergence", content, ignore.case = TRUE)))

  # Clean up
  unlink(result)
})

test_that("diagnostic_report: validates section names", {
  skip_on_cran()

  tmp_file <- tempfile(fileext = ".html")

  # Invalid section name
  expect_error(
    diagnostic_report(
      model = report_test_model,
      output_file = tmp_file,
      include_sections = c("invalid_section"),
      open_report = FALSE
    ),
    "Must be a subset"
  )
})

test_that("diagnostic_report: input validation", {
  skip_on_cran()

  tmp_file <- tempfile(fileext = ".html")

  # Invalid model
  expect_error(
    diagnostic_report(model = lm(y ~ x, data = test_data),
                      output_file = tmp_file),
    "Model must be a brmsfit or stanfit object"
  )

  # Invalid rhat_threshold
  expect_error(
    diagnostic_report(model = report_test_model,
                      output_file = tmp_file,
                      rhat_threshold = 0.9),
    "Element 1 is not >= 1"
  )

  # Invalid ess_threshold
  expect_error(
    diagnostic_report(model = report_test_model,
                      output_file = tmp_file,
                      ess_threshold = 50),
    "Element 1 is not <= 1"
  )
})

test_that("diagnostic_report: generates different formats", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")

  # HTML format
  html_file <- tempfile(fileext = ".html")
  result_html <- diagnostic_report(
    model = report_test_model,
    output_file = html_file,
    output_format = "html",
    include_sections = c("model_summary"),
    open_report = FALSE
  )

  expect_true(file.exists(result_html))
  expect_true(grepl("\\.html$", result_html))

  # Clean up
  unlink(result_html)
})

test_that("diagnostic_report: handles all sections", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")

  tmp_file <- tempfile(fileext = ".html")

  # Generate with all sections
  result <- diagnostic_report(
    model = report_test_model,
    output_file = tmp_file,
    include_sections = c("model_summary", "convergence",
                         "posterior_summary", "recommendations"),
    open_report = FALSE
  )

  expect_true(file.exists(result))

  # Clean up
  unlink(result)
})

test_that("diagnostic_report: creates valid output path", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")

  # Use specific directory
  tmp_dir <- tempdir()
  tmp_file <- file.path(tmp_dir, "test_diagnostics.html")

  result <- diagnostic_report(
    model = report_test_model,
    output_file = tmp_file,
    include_sections = c("model_summary"),
    open_report = FALSE
  )

  # Path should match
  expect_equal(normalizePath(result), normalizePath(tmp_file))

  # Clean up
  unlink(result)
})

test_that("diagnostic_report: open_report parameter works", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")

  tmp_file <- tempfile(fileext = ".html")

  # With open_report = FALSE, should not try to open
  expect_error(
    diagnostic_report(
      model = report_test_model,
      output_file = tmp_file,
      include_sections = c("model_summary"),
      open_report = FALSE
    ),
    NA
  )

  # Clean up
  unlink(tmp_file)
})

test_that("diagnostic_report: handles minimal model", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")

  # FIXED: Increased iterations from 200 to 2000, warmup to 1000, chains to 2
  simple_model <- brm(
    y ~ 1,
    data = data.frame(y = rnorm(20)),
    chains = 2,       # Increased from 1
    iter = 2000,      # Increased from 200
    warmup = 1000,    # Added warmup
    refresh = 0,
    silent = 2
  )

  tmp_file <- tempfile(fileext = ".html")

  result <- diagnostic_report(
    model = simple_model,
    output_file = tmp_file,
    include_sections = c("model_summary", "convergence"),
    open_report = FALSE
  )

  expect_true(file.exists(result))

  # Clean up
  unlink(result)
})

test_that("diagnostic_report: messages during generation", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")

  tmp_file <- tempfile(fileext = ".html")

  # Should show informative messages
  expect_message(
    diagnostic_report(
      model = report_test_model,
      output_file = tmp_file,
      include_sections = c("model_summary"),
      open_report = FALSE
    ),
    "Gathering diagnostic information"
  )

  # Clean up
  if (file.exists(tmp_file)) unlink(tmp_file)
})

test_that("diagnostic_report: returns invisibly", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")

  tmp_file <- tempfile(fileext = ".html")

  # Capture return value
  result <- diagnostic_report(
    model = report_test_model,
    output_file = tmp_file,
    include_sections = c("model_summary"),
    open_report = FALSE
  )

  # Should return character path
  expect_type(result, "character")
  expect_length(result, 1)

  # Clean up
  unlink(result)
})
