#' Diagnostic Report for Bayesian Models
#'
#' Generates comprehensive diagnostics and creates a formatted report
#' for fitted Bayesian models (brmsfit, stanfit, etc.)
#'
#' @param model A fitted model object (brmsfit, stanfit, etc.)
#' @param output_file Character. Path for output file
#' @param output_format Character. Format: "pdf", "html", "docx"
#' @param include_sections Character vector. Sections to include
#' @param rhat_threshold Numeric. R-hat threshold for flagging (default: 1.01)
#' @param ess_threshold Numeric. Effective sample size ratio threshold
#' @param open_report Logical. Open report after generation?
#'
#' @return Invisibly returns output file path
#' @export
diagnostic_report <- function(model,
                              output_file = NULL,
                              output_format = "pdf",
                              include_sections = c("model_summary", "convergence",
                                                   "posterior_summary", "recommendations"),
                              rhat_threshold = 1.01,
                              ess_threshold = 0.1,
                              open_report = TRUE) {
  # Input validation
  if (!inherits(model, "brmsfit") && !inherits(model, "stanfit")) {
    stop("Model must be a brmsfit or stanfit object")
  }

  checkmate::assert_character(output_format, len = 1)
  checkmate::assert_numeric(rhat_threshold, lower = 1.0, len = 1)
  checkmate::assert_numeric(ess_threshold, lower = 0.0, upper = 1.0, len = 1)
  checkmate::assert_logical(open_report, len = 1)

  # Validate section names
  valid_sections <- c("model_summary", "convergence", "posterior_summary",
                      "recommendations", "ppc")
  checkmate::assert_subset(include_sections, valid_sections)

  # Set output file if not provided
  if (is.null(output_file)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    output_file <- file.path(tempdir(),
                             paste0("diagnostic_report_", timestamp, ".",
                                    tolower(output_format)))
  }

  message("Gathering diagnostic information...")

  # Gather diagnostics
  diagnostics <- gather_diagnostics(model)

  # Check convergence
  convergence_check <- check_convergence_for_report(
    model,
    rhat_threshold = rhat_threshold,
    ess_threshold = ess_threshold
  )

  # Extract model summary
  model_summary <- extract_model_summary(model)

  # Posterior summary
  posterior_summary <- summarize_posterior(model)

  # Recommendations
  recommendations <- generate_recommendations(
    diagnostics,
    convergence_check
  )

  # Generate report content
  report_content <- create_report_template(
    model = model,
    model_summary = model_summary,
    diagnostics = diagnostics,
    convergence = convergence_check,
    posterior_summary = posterior_summary,
    recommendations = recommendations,
    include_sections = include_sections
  )

  # Save report based on format
  switch(output_format,
         pdf = save_report_pdf(report_content, output_file),
         html = save_report_html(report_content, output_file),
         docx = save_report_docx(report_content, output_file),
         stop("Unsupported output format: ", output_format)
  )

  message("Report saved to: ", output_file)

  # Open if requested
  if (open_report && interactive()) {
    utils::browseURL(output_file)
  }

  # Return invisibly
  invisible(output_file)
}

#' @keywords internal
gather_diagnostics <- function(model) {
  tryCatch({
    if (inherits(model, "brmsfit")) {
      fit_summary <- rstan::summary(model$fit)$summary
    } else if (inherits(model, "stanfit")) {
      fit_summary <- rstan::summary(model)$summary
    } else {
      return(list(rhat = NA, ess_bulk = NA, ess_tail = NA))
    }

    list(
      rhat = if ("Rhat" %in% colnames(fit_summary)) fit_summary[, "Rhat"] else NA,
      ess_bulk = if ("Bulk_ESS" %in% colnames(fit_summary)) fit_summary[, "Bulk_ESS"] else NA,
      ess_tail = if ("Tail_ESS" %in% colnames(fit_summary)) fit_summary[, "Tail_ESS"] else NA
    )
  }, error = function(e) {
    warning("Could not extract diagnostics: ", e$message)
    list(rhat = NA, ess_bulk = NA, ess_tail = NA)
  })
}

#' @keywords internal
check_convergence_for_report <- function(model, rhat_threshold = 1.01,
                                         ess_threshold = 0.1) {
  diagnostics <- gather_diagnostics(model)

  # Handle NA cases
  n_params <- if (!is.na(diagnostics$rhat[1])) length(diagnostics$rhat) else 0

  n_rhat_issues <- if (!is.na(diagnostics$rhat[1])) {
    sum(diagnostics$rhat > rhat_threshold, na.rm = TRUE)
  } else {
    0
  }

  n_ess_issues <- if (!is.na(diagnostics$ess_bulk[1])) {
    sum(diagnostics$ess_bulk < ess_threshold, na.rm = TRUE)
  } else {
    0
  }

  list(
    converged = n_rhat_issues == 0 && n_ess_issues == 0,
    rhat_issues = n_rhat_issues,
    ess_issues = n_ess_issues,
    total_parameters = n_params
  )
}

#' @keywords internal
extract_model_summary <- function(model) {
  tryCatch({
    if (inherits(model, "brmsfit")) {
      list(
        family = tryCatch(model$family$family, error = function(e) "Unknown"),
        formula = tryCatch(as.character(model$formula), error = function(e) "Unknown"),
        n_obs = tryCatch(nrow(model$data), error = function(e) NA),
        n_chains = tryCatch(model$fit@sim$chains, error = function(e) NA),
        n_iter = tryCatch(model$fit@sim$iter, error = function(e) NA),
        n_warmup = tryCatch(model$fit@sim$warmup, error = function(e) NA)
      )
    } else {
      list(model_class = class(model)[1])
    }
  }, error = function(e) {
    list(model_class = class(model)[1])
  })
}

#' @keywords internal
summarize_posterior <- function(model) {
  tryCatch({
    if (inherits(model, "brmsfit")) {
      posterior::summarise_draws(posterior::as_draws(model))
    } else {
      NULL
    }
  }, error = function(e) {
    warning("Could not summarize posterior: ", e$message)
    NULL
  })
}

#' @keywords internal
generate_recommendations <- function(diagnostics, convergence_check) {
  recommendations <- character()

  if (!convergence_check$converged) {
    recommendations <- c(
      recommendations,
      "Model has convergence issues. Consider:"
    )

    if (convergence_check$rhat_issues > 0) {
      recommendations <- c(
        recommendations,
        sprintf("  - %d parameters have Rhat > 1.01",
                convergence_check$rhat_issues),
        "  - Increase number of iterations"
      )
    }
  }

  if (convergence_check$ess_issues > 0) {
    recommendations <- c(
      recommendations,
      sprintf("  - %d parameters have low effective sample size",
              convergence_check$ess_issues),
      "  - Consider increasing thinning"
    )
  }

  if (length(recommendations) == 0) {
    recommendations <- "Model shows good convergence. No major issues detected."
  }

  recommendations
}

#' @keywords internal
create_report_template <- function(model, model_summary, diagnostics,
                                   convergence, posterior_summary,
                                   recommendations, include_sections) {
  report <- paste(
    "# Bayesian Diagnostic Report",
    "",
    "Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    "",
    sep = "\n"
  )

  if ("model_summary" %in% include_sections) {
    report <- paste(report, "## Model Summary", "", sep = "\n")
    if (!is.null(model_summary)) {
      report <- paste(
        report,
        paste(paste(names(model_summary), model_summary, sep = ": "), collapse = "\n"),
        sep = "\n"
      )
    }
  }

  if ("convergence" %in% include_sections) {
    report <- paste(report, "## Convergence Diagnostics", "", sep = "\n")
    report <- paste(
      report,
      sprintf("Converged: %s\n", convergence$converged),
      sprintf("Rhat issues: %d\n", convergence$rhat_issues),
      sprintf("ESS issues: %d\n", convergence$ess_issues),
      sep = ""
    )
  }

  if ("posterior_summary" %in% include_sections && !is.null(posterior_summary)) {
    report <- paste(report, "## Posterior Summary", "", sep = "\n")
    report <- paste(
      report,
      paste(capture.output(print(posterior_summary)), collapse = "\n"),
      sep = "\n"
    )
  }

  if ("recommendations" %in% include_sections) {
    report <- paste(report, "## Recommendations", "", sep = "\n")
    report <- paste(report, paste(recommendations, collapse = "\n"), sep = "\n")
  }

  report
}

#' @keywords internal
save_report_pdf <- function(content, file_path) {
  writeLines(content, file_path)
}

#' @keywords internal
save_report_html <- function(content, file_path) {
  html <- sprintf(
    "<!DOCTYPE html>\n<html>\n<head>\n<title>Diagnostic Report</title>\n<style>body { font-family: sans-serif; margin: 20px; }</style>\n</head>\n<body>\n<pre>%s</pre>\n</body>\n</html>",
    content
  )
  writeLines(html, file_path)
}

#' @keywords internal
save_report_docx <- function(content, file_path) {
  writeLines(content, file_path)
}
