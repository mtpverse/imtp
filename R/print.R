#' @export
print.imtp <- function(x, ...) {
  cat("\n")
  cli::cli_text("{.strong IPSI Estimator}: {x$estimator}")
  cli::cli_text(cat("         "), "{.strong delta}: ", cli::col_blue(cli::style_italic("{x$delta}")))
  cat("\n")
  cli::cli_text("{.strong Population intervention estimate}")
  cli::cli_text(cat("      "), "{.strong Estimate}: {round(x$theta, 4)}")
  cli::cli_text(cat("    "), "{.strong Std. error}: {round(x$standard_error, 4)}")
  cli::cli_text(cat("        "), "{.strong 95% CI}: ({round(x$low, 4)}, {round(x$high, 4)})")
  if (x$estimator %in% c("substitution", "IPW")) no_stderr_warning(x$estimator)
  cat("\n")
}
