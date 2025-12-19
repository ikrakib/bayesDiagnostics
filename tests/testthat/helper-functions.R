safe_extract_parameter <- function(fit, param_name) {
  tryCatch({
    draws <- posterior::as_draws_df(fit)
    if (param_name %in% names(draws)) {
      return(draws[[param_name]])
    } else {
      # Try to find similar parameter
      possible <- names(draws)[grepl(param_name, names(draws))]
      if (length(possible) > 0) {
        return(draws[[possible[1]]])
      }
      return(NULL)
    }
  }, error = function(e) {
    NULL
  })
}

# Validate metric value safely
validate_metric <- function(metric_val) {
  if (is.null(metric_val)) return(FALSE)
  if (is.na(metric_val)) return(FALSE)
  if (!is.numeric(metric_val)) return(FALSE)
  return(TRUE)
}

# Safely compare values with NA handling
safe_compare <- function(val, threshold, op = `>`) {
  if (is.na(val)) return(FALSE)
  if (!is.numeric(val)) return(FALSE)
  return(op(val, threshold))
}
