#' @importFrom generics tidy
#' @export
generics::tidy

#' Tidy a(n) lmtp object
#'
#' @param x A `lmtp` object produced by a call to [imtp::imtp_tmle()], [imtp::imtp_sdr()].
#' @param ... Unused, included for generic consistency only.
#'
#' @examples
#' \donttest{
#' a <- c("A1", "A2")
#' nodes <- list(c("L1"), c("L2"))
#' cens <- c("C1", "C2")
#' y <- "Y"
#' fit <- imtp_tmle(sim_cens, a, y, time_vary = nodes, cens = cens, shift = NULL, folds = 2)
#' tidy(fit)
#' }
#'
#' @export
tidy.imtp <- function(x, ...) {
  out <- data.frame(estimator = x$estimator,
                    estimate = x$theta,
                    std.error = x$standard_error,
                    conf.low = x$low,
                    conf.high = x$high)
  class(out) <- c("tbl_df", "tbl", "data.frame")
  out
}
