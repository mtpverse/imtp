#' Estimate simultaneous confidence intervals
#'
#' @param fits [\code{list}]\cr
#'  A list of \code{imtp} fits.
#' @param reps [\code{numeric(1)}]\cr
#'  The number of repitions to use for the multiplier bootstrap. The default is 1e5.
#' @param level [\code{numeric(1)}]\cr
#'  The desired confidence level. The default is 0.95.
#'
#' @return A \code{data.frame} with estimates.
#' @export
#'
#' @examples
#' n <- 1000
#' W <- matrix(rnorm(n*3), ncol = 3)
#' A <- rbinom(n, 1, 1/(1 + exp(-(.2*W[,1] - .1*W[,2] + .4*W[,3]))))
#' Y <- A + 2*W[,1] + W[,3] + W[,2]^2 + rnorm(n)
#' R <- rbinom(n, 1, 0.9)
#' tmp <- data.frame(W, A, R, Y = ifelse(R == 1, Y, NA_real_))
#' deltas <- seq(0.1, 2, length.out = 5)
#' fits <- lapply(deltas, function(d) imtp_tmle(tmp, "A", "Y", paste0("X", 1:3), cens = "R", folds = 1, delta = d, outcome_type = "continuous"))
#' imtp_simul(fits)
imtp_simul <- function(fits, reps = 1e5, level = 0.95) {
	assertImtpList(fits)
	assertConsistentNs(fits)

	cv <- simul::simul(lapply(fits, function(x) x$theta),
										 lapply(fits, function(x) x$eif),
										 nobs = nrow(fits[[1]]$density_ratios),
										 reps = reps,
										 level = level)

	out <- lapply(fits, function(x) {
		ci_low <- x$theta - (cv * x$standard_error)
		ci_high <- x$theta + (cv * x$standard_error)

		data.frame(delta = x$delta,
							 theta = x$theta,
							 mult.conf.low = ci_low,
							 mult.conf.high = ci_high)
	})

	Reduce(rbind, out)
}
