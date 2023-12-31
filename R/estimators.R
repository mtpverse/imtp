#' Incremental Propensity Score Intervention Targeted Maximum Likelihood Estimator
#'
#' @param data \[\code{data.frame}\]\cr
#'  A \code{data.frame} in wide format containing all necessary variables
#'  for the estimation problem. Must not be a \code{data.table}.
#' @param trt \[\code{character}\]\cr
#'  A vector containing the column names of treatment variables ordered by time.
#' @param outcome \[\code{character}\]\cr
#'  The column name of the outcome variable. In the case of time-to-event
#'  analysis, a vector containing the columns names of intermediate outcome variables and the final
#'  outcome variable ordered by time. Only numeric values are allowed. If the outcome type
#'  is binary, data should be coded as 0 and 1.
#' @param baseline \[\code{character}\]\cr
#'  An optional vector containing the column names of baseline covariates to be
#'  included for adjustment at every time point.
#' @param time_vary \[\code{list}\]\cr
#'  A list the same length as the number of time points of observation with
#'  the column names for new time-varying covariates introduced at each time point. The list
#'  should be ordered following the time ordering of the model.
#' @param cens \[\code{character}\]\cr
#'  An optional vector of column names of censoring indicators the same
#'  length as the number of time points of observation. If missingness in the outcome is
#'  present or if time-to-event outcome, must be provided.
#' @param delta \[\code{numeric(1)}\]\cr
#' @param k \[\code{integer(1)}\]\cr
#'  An integer specifying how previous time points should be
#'  used for estimation at the given time point. Default is \code{Inf},
#'  all time points.
#' @param outcome_type \[\code{character(1)}\]\cr
#'  Outcome variable type (i.e., continuous, binomial, survival).
#' @param id \[\code{character(1)}\]\cr
#'  An optional column name containing cluster level identifiers.
#' @param bounds \[\code{numeric(2)}\]\cr
#'  An optional, ordered vector of the bounds for a continuous outcomes. If \code{NULL},
#'  the bounds will be taken as the minimum and maximum of the observed data.
#'  Should be left as \code{NULL} if the outcome type is binary.
#' @param learners_outcome \[\code{character}\]\cr A vector of \code{SuperLearner} algorithms for estimation
#'  of the outcome regression. Default is \code{"SL.glm"}, a main effects GLM.
#' @param learners_trt \[\code{character}\]\cr A vector of \code{SuperLearner} algorithms for estimation
#'  of the exposure mechanism. Default is \code{"SL.glm"}, a main effects GLM.
#' @param folds \[\code{integer(1)}\]\cr
#'  The number of folds to be used for cross-fitting.
#' @param weights \[\code{numeric(nrow(data))}\]\cr
#'  An optional vector containing sampling weights.
#' @param .bound \[\code{numeric(1)}\]\cr
#'  Determines that maximum and minimum values (scaled) predictions
#'  will be bounded by. The default is 1e-5, bounding predictions by 1e-5 and 0.9999.
#' @param .trim \[\code{numeric(1)}\]\cr
#'  Determines the amount the density ratios should be trimmed.
#'  The default is 0.999, trimming the density ratios greater than the 0.999 percentile
#'  to the 0.999 percentile. A value of 1 indicates no trimming.
#' @param .learners_outcome_folds \[\code{integer(1)}\]\cr
#'  The number of cross-validation folds for \code{learners_outcome}.
#' @param .learners_trt_folds \[\code{integer(1)}\]\cr
#'  The number of cross-validation folds for \code{learners_trt}.
#' @param .return_full_fits \[\code{logical(1)}\]\cr
#'  Return full SuperLearner fits? Default is \code{FALSE}, return only SuperLearner weights.
#' @param ... Extra arguments. Exists for backwards compatibility.
#'
#' @return A list of class \code{imtp} containing the following components:
#'
#' \item{estimator}{The estimator used, in this case "TMLE".}
#' \item{theta}{The estimated population effect.}
#' \item{standard_error}{The estimated standard error of the estimand.}
#' \item{low}{Lower bound of the 95\% confidence interval of the estimand.}
#' \item{high}{Upper bound of the 95\% confidence interval of the estimand.}
#' \item{eif}{The estimated, un-centered, influence function of the estimate.}
#' \item{delta}{...}
#' \item{outcome_reg}{An n x Tau + 1 matrix of outcome regression predictions.
#'  The mean of the first column is used for calculating theta.}
#' \item{density_ratios}{An n x Tau matrix of the estimated, non-cumulative, density ratios.}
#' \item{fits_m}{A list the same length as \code{folds}, containing the fits at each time-point
#'  for each fold for the outcome regression.}
#' \item{fits_r}{A list the same length as \code{folds}, containing the fits at each time-point
#' for each fold of density ratio estimation.}
#' \item{outcome_type}{The outcome variable type.}
#'
#' @example inst/examples/tmle-ex.R
#' @export
imtp_tmle <- function(data, trt, outcome, baseline = NULL, time_vary = NULL,
                      cens = NULL, delta = 1, k = Inf,
                      mtp = FALSE, outcome_type = c("binomial", "continuous", "survival"),
                      id = NULL, bounds = NULL,
                      learners_outcome = "SL.glm",
                      learners_trt = "SL.glm",
                      folds = 10, weights = NULL, .bound = 1e-5, .trim = 0.999,
                      .learners_outcome_folds = 10, .learners_trt_folds = 10,
                      .return_full_fits = FALSE, ...) {

	assertNotDataTable(data)
	checkmate::assertCharacter(outcome, len = if (match.arg(outcome_type) != "survival") 1,
														 min.len = if (match.arg(outcome_type) == "survival") 2)
	checkmate::assertCharacter(baseline, null.ok = TRUE)

	tau <- determine_tau(outcome, trt)

	assertTrtCharacter(trt, tau)
	checkmate::assertCharacter(cens, len = tau, null.ok = !checkmate::anyMissing(data[, outcome, drop = FALSE]))
	checkmate::assertList(time_vary, types = c("NULL", "character"), len = tau, null.ok = TRUE)
	checkmate::assertCharacter(id, len = 1, null.ok = TRUE)
	checkmate::assertSubset(c(trt, outcome, baseline, unlist(time_vary), cens, id), names(data))
  checkmate::assertCharacter(id, len = 1, null.ok = TRUE)
  checkmate::assertSubset(c(trt, outcome, unique(unlist(baseline)), unique(unlist(time_vary)), cens, id), names(data))
  assertImtpData(data, trt, outcome, baseline, time_vary, cens, id)
  assertOutcomeTypes(data, outcome, match.arg(outcome_type))
  assertReservedNames(data)
  checkmate::assertNumber(delta, lower = 0)
  checkmate::assertNumeric(bounds, len = 2, finite = TRUE, any.missing = FALSE, sorted = TRUE, null.ok = TRUE)
  checkmate::assertNumeric(weights, len = nrow(data), finite = TRUE, any.missing = FALSE, null.ok = TRUE)
  checkmate::assertNumber(k, lower = 0, upper = Inf)
  checkmate::assertNumber(folds, lower = 1, upper = nrow(data) - 1)
  checkmate::assertNumber(.learners_outcome_folds, null.ok = TRUE)
  checkmate::assertNumber(.learners_trt_folds, null.ok = TRUE)
  checkmate::assertNumber(.bound)
  checkmate::assertNumber(.trim, upper = 1)
  checkmate::assertLogical(.return_full_fits, len = 1)


  Task <- imtp_Task$new(
    data = data,
    trt = trt,
    outcome = outcome,
    time_vary = time_vary,
    baseline = baseline,
    cens = cens,
    k = k,
    delta = delta,
    id = id,
    outcome_type = match.arg(outcome_type),
    V = folds,
    weights = weights,
    bounds = bounds,
    bound = .bound
  )

  pb <- progressr::progressor(Task$tau*folds*2)

  ratios <- cf_r(Task, learners_trt, mtp, .learners_trt_folds, .trim, .return_full_fits, pb)
  Task$add_d(ratios$d)
  estims <- cf_tmle(Task, "tmp_imtp_scaled_outcome", ratios$ratios, learners_outcome, .learners_outcome_folds, .return_full_fits, pb)

  theta_dr(
    list(
      estimator = "TMLE",
      m = list(natural = estims$natural, shifted = estims$shifted),
      r = ratios$ratios,
      tau = Task$tau,
      folds = Task$folds,
      id = Task$id,
      outcome_type = Task$outcome_type,
      bounds = Task$bounds,
      weights = Task$weights,
      delta = Task$delta,
      fits_m = estims$fits,
      fits_r = ratios$fits,
      outcome_type = Task$outcome_type
    ),
    FALSE
  )
}

#' Incremental Propensity Score Intervention Sequential Doubly Robust Estimator
#'
#' @param data \[\code{data.frame}\]\cr
#'  A \code{data.frame} in wide format containing all necessary variables
#'  for the estimation problem. Must not be a \code{data.table}.
#' @param trt \[\code{character}\]\cr
#'  A vector containing the column names of treatment variables ordered by time.
#' @param outcome \[\code{character}\]\cr
#'  The column name of the outcome variable. In the case of time-to-event
#'  analysis, a vector containing the columns names of intermediate outcome variables and the final
#'  outcome variable ordered by time. Only numeric values are allowed. If the outcome type
#'  is binary, data should be coded as 0 and 1.
#' @param baseline \[\code{character}\]\cr
#'  An optional vector containing the column names of baseline covariates to be
#'  included for adjustment at every time point.
#' @param time_vary \[\code{list}\]\cr
#'  A list the same length as the number of time points of observation with
#'  the column names for new time-varying covariates introduced at each time point. The list
#'  should be ordered following the time ordering of the model.
#' @param cens \[\code{character}\]\cr
#'  An optional vector of column names of censoring indicators the same
#'  length as the number of time points of observation. If missingness in the outcome is
#'  present or if time-to-event outcome, must be provided.
#' @param delta \[\code{numeric(1)}\]\cr
#' @param k \[\code{integer(1)}\]\cr
#'  An integer specifying how previous time points should be
#'  used for estimation at the given time point. Default is \code{Inf},
#'  all time points.
#' @param outcome_type \[\code{character(1)}\]\cr
#'  Outcome variable type (i.e., continuous, binomial, survival).
#' @param id \[\code{character(1)}\]\cr
#'  An optional column name containing cluster level identifiers.
#' @param bounds \[\code{numeric(2)}\]\cr
#'  An optional, ordered vector of the bounds for a continuous outcomes. If \code{NULL},
#'  the bounds will be taken as the minimum and maximum of the observed data.
#'  Should be left as \code{NULL} if the outcome type is binary.
#' @param learners_outcome \[\code{character}\]\cr A vector of \code{SuperLearner} algorithms for estimation
#'  of the outcome regression. Default is \code{"SL.glm"}, a main effects GLM.
#' @param learners_trt \[\code{character}\]\cr A vector of \code{SuperLearner} algorithms for estimation
#'  of the exposure mechanism. Default is \code{"SL.glm"}, a main effects GLM.
#' @param folds \[\code{integer(1)}\]\cr
#'  The number of folds to be used for cross-fitting.
#' @param weights \[\code{numeric(nrow(data))}\]\cr
#'  An optional vector containing sampling weights.
#' @param .bound \[\code{numeric(1)}\]\cr
#'  Determines that maximum and minimum values (scaled) predictions
#'  will be bounded by. The default is 1e-5, bounding predictions by 1e-5 and 0.9999.
#' @param .trim \[\code{numeric(1)}\]\cr
#'  Determines the amount the density ratios should be trimmed.
#'  The default is 0.999, trimming the density ratios greater than the 0.999 percentile
#'  to the 0.999 percentile. A value of 1 indicates no trimming.
#' @param .learners_outcome_folds \[\code{integer(1)}\]\cr
#'  The number of cross-validation folds for \code{learners_outcome}.
#' @param .learners_trt_folds \[\code{integer(1)}\]\cr
#'  The number of cross-validation folds for \code{learners_trt}.
#' @param .return_full_fits \[\code{logical(1)}\]\cr
#'  Return full SuperLearner fits? Default is \code{FALSE}, return only SuperLearner weights.
#' @param ... Extra arguments. Exists for backwards compatibility.
#'
#' @return A list of class \code{imtp} containing the following components:
#'
#' \item{estimator}{The estimator used, in this case "SDR".}
#' \item{theta}{The estimated population effect.}
#' \item{standard_error}{The estimated standard error of the estimand}
#' \item{low}{Lower bound of the 95\% confidence interval of the estimand.}
#' \item{high}{Upper bound of the 95\% confidence interval of the estimand.}
#' \item{eif}{The estimated, un-centered, influence function of the estimate.}
#' \item{delta}{...}
#' \item{outcome_reg}{An n x Tau + 1 matrix of outcome regression predictions.
#'  The mean of the first column is used for calculating theta.}
#' \item{density_ratios}{An n x Tau matrix of the estimated, non-cumulative, density ratios.}
#' \item{fits_m}{A list the same length as \code{folds}, containing the fits at each time-point
#'  for each fold for the outcome regression.}
#' \item{fits_r}{A list the same length as \code{folds}, containing the fits at each time-point
#' for each fold of density ratio estimation.}
#' \item{outcome_type}{The outcome variable type.}
#'
#' @example inst/examples/sdr-ex.R
#' @export
imtp_sdr <- function(data, trt, outcome, baseline = NULL, time_vary = NULL,
                     cens = NULL, delta = 1, k = Inf,
                     mtp = FALSE,
                     outcome_type = c("binomial", "continuous", "survival"),
                     id = NULL, bounds = NULL,
                     learners_outcome = "SL.glm",
                     learners_trt = "SL.glm",
                     folds = 10, weights = NULL, .bound = 1e-5, .trim = 0.999,
                     .learners_outcome_folds = 10, .learners_trt_folds = 10,
                     .return_full_fits = FALSE, ...) {

	assertNotDataTable(data)
	checkmate::assertCharacter(outcome, len = if (match.arg(outcome_type) != "survival") 1,
														 min.len = if (match.arg(outcome_type) == "survival") 2)
	checkmate::assertCharacter(baseline, null.ok = TRUE)

	tau <- determine_tau(outcome, trt)

	assertTrtCharacter(trt, tau)
	checkmate::assertCharacter(cens, len = tau, null.ok = !checkmate::anyMissing(data[, outcome, drop = FALSE]))
	checkmate::assertList(time_vary, types = c("NULL", "character"), len = tau, null.ok = TRUE)
	checkmate::assertCharacter(id, len = 1, null.ok = TRUE)
	checkmate::assertSubset(c(trt, outcome, baseline, unlist(time_vary), cens, id), names(data))
	checkmate::assertCharacter(id, len = 1, null.ok = TRUE)
	checkmate::assertSubset(c(trt, outcome, unique(unlist(baseline)), unique(unlist(time_vary)), cens, id), names(data))
	assertImtpData(data, trt, outcome, baseline, time_vary, cens, id)
	assertOutcomeTypes(data, outcome, match.arg(outcome_type))
	assertReservedNames(data)
	checkmate::assertNumber(delta, lower = 0)
	checkmate::assertNumeric(bounds, len = 2, finite = TRUE, any.missing = FALSE, sorted = TRUE, null.ok = TRUE)
	checkmate::assertNumeric(weights, len = nrow(data), finite = TRUE, any.missing = FALSE, null.ok = TRUE)
	checkmate::assertNumber(k, lower = 0, upper = Inf)
	checkmate::assertNumber(folds, lower = 1, upper = nrow(data) - 1)
	checkmate::assertNumber(.learners_outcome_folds, null.ok = TRUE)
	checkmate::assertNumber(.learners_trt_folds, null.ok = TRUE)
	checkmate::assertNumber(.bound)
	checkmate::assertNumber(.trim, upper = 1)
	checkmate::assertLogical(.return_full_fits, len = 1)

	Task <- imtp_Task$new(
		data = data,
		trt = trt,
		outcome = outcome,
		time_vary = time_vary,
		baseline = baseline,
		cens = cens,
		k = k,
		delta = delta,
		id = id,
		outcome_type = match.arg(outcome_type),
		V = folds,
		weights = weights,
		bounds = bounds,
		bound = .bound
	)

  pb <- progressr::progressor(Task$tau*folds*2)

  ratios <- cf_r(Task, learners_trt, mtp, .learners_trt_folds, .trim, .return_full_fits, pb)
  Task$add_d(ratios$d)
  estims <- cf_tmle(Task, "tmp_imtp_scaled_outcome", ratios$ratios, learners_outcome, .learners_outcome_folds, .return_full_fits, pb)

  theta_dr(
    list(
      estimator = "SDR",
      m = list(natural = estims$natural, shifted = estims$shifted),
      r = ratios$ratios,
      tau = Task$tau,
      folds = Task$folds,
      id = Task$id,
      outcome_type = Task$outcome_type,
      bounds = Task$bounds,
      weights = Task$weights,
      delta = Task$delta,
      fits_m = estims$fits,
      fits_r = ratios$fits,
      outcome_type = Task$outcome_type
    ),
    TRUE
  )
}
