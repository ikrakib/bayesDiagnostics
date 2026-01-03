# bayesDiagnostics 0.1.0

## Initial CRAN Release

### New Features

* **MCMC Diagnostics** (3 functions)
  - `mcmc_diagnostics_summary()`: Comprehensive convergence checks
  - `effective_sample_size_diagnostics()`: Detailed ESS analysis
  - `hierarchical_convergence()`: Hierarchical model diagnostics

* **Posterior Predictive Checks** (5 functions)
  - `posterior_predictive_check()`: Full PPC with test statistics
  - `automated_ppc()`: Automated diagnostic screening
  - `graphical_ppc()`: Publication-quality visualizations
  - `ppc_crossvalidation()`: LOO-PIT calibration checks
  - `bayesian_p_values()`: Custom test statistic utilities

* **Prior Specification & Sensitivity** (3 functions)
  - `prior_elicitation_helper()`: Expert knowledge → priors
  - `prior_sensitivity()`: Robustness assessment
  - `prior_robustness()`: Multi-dimensional sensitivity

* **Model Comparison** (3 functions)
  - `model_comparison_suite()`: LOO/WAIC/Bayes R² comparison
  - `bayes_factor_comparison()`: Bridge sampling Bayes Factors
  - `predictive_performance()`: Predictive accuracy metrics

* **Utilities & Reporting** (4 functions)
  - `extract_posterior_unified()`: Cross-package posterior extraction
  - `diagnostic_report()`: HTML/PDF report generation

### Package Integration

* Seamless integration with `brms`, `rstan`, and `rstanarm`
* Support for `cmdstanr` and `coda` packages
* Comprehensive vignette with 18 function examples
* Complete workflow demonstration

### Documentation

* 800+ line introductory vignette
* Complete function reference documentation
* Interpretation guidelines for all diagnostics
* Further reading section with key references
