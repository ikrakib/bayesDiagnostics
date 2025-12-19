skip_if_cran <- function() {
  if (identical(Sys.getenv("NOT_CRAN"), "")) {
    testthat::skip("Skipped on CRAN")
  }
}
