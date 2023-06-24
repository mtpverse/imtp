cf_r <- function(Task, learners, mtp, lrnr_folds, trim, full_fits, pb) {
  fopts <- options("lmtp.bound", "lmtp.trt.length")
  out <- list()

  for (fold in seq_along(Task$folds)) {
    out[[fold]] <- future::future({
      options(fopts)

      estimate_r(
        get_folded_data(Task$natural, Task$folds, fold),
        Task$trt, Task$cens, Task$risk, Task$tau,
        Task$delta, Task$node_list,
        learners, pb, mtp, lrnr_folds, full_fits
      )
    },
    seed = TRUE)
  }

  trim_ratios(recombine_ratios(future::value(out), Task$folds), trim)
}

estimate_r <- function(natural, trt, cens, risk, tau, delta, node_list, learners, pb, mtp, lrnr_folds, full_fits) {
  trt_tau <- ifelse(length(trt) == 1 & tau > 1, 1, tau)

	densratios <- matrix(nrow = nrow(natural$valid), ncol = tau)
  d <- matrix(-999L, nrow = nrow(natural$valid), ncol = trt_tau)
  fits <- list()

  fit_propensity <- TRUE
  for (t in 1:tau) {
    jrt <- censored(natural$train, cens, t)$j
    drt <- at_risk(natural$train, risk, t)
    irv <- censored(natural$valid, cens, t)$i
    jrv <- censored(natural$valid, cens, t)$j
    drv <- at_risk(natural$valid, risk, t)

    trt_t <- ifelse(length(trt) > 1, trt[t], trt)

    vars_trt <- node_list$trt[[t]]
    vars_cens <- node_list$cens[[t]]

    if (fit_propensity) {
    	fit <- run_ensemble(
    		natural$train[jrt & drt, ][[trt_t]],
    		natural$train[jrt & drt, vars_trt],
    		learners,
    		"binomial",
    		natural$train[jrt & drt, ]$lmtp_id,
    		lrnr_folds
    	)

    	eps <- runif(nrow(natural$valid))
    	pi <- pi_d <- matrix(-999L, nrow = nrow(natural$valid), ncol = 1)
    	pi[jrv & drv, ] <- SL_predict(fit, natural$valid[jrv & drv, vars_trt])
    	pi_d[jrv & drv, ] <- ((delta*pi) / (delta*pi + 1 - pi))[jrv & drv, ]
    	eps <- runif(nrow(natural$valid))

    	if (trt_tau != tau) {
    		d[jrv & drv, 1] <- as.numeric(eps[jrv & drv] < pi_d[jrv & drv])
    		fit_propensity <- FALSE
    	} else {
    		d[jrv & drv, t] <- as.numeric(eps[jrv & drv] < pi_d[jrv & drv])
    	}
    }

    if (!is.null(cens)) {
      fit_cens <- run_ensemble(
        natural$train[censored(natural$train, cens, t)$j & at_risk(natural$train, risk, t), ][[cens[t]]],
        natural$train[censored(natural$train, cens, t)$j & at_risk(natural$train, risk, t), vars_cens],
        learners,
        "binomial",
        natural$train[censored(natural$train, cens, t)$j & at_risk(natural$train, risk, t), ]$lmtp_id,
        lrnr_folds
      )

      pred_cens <- matrix(-999L, nrow = nrow(natural$valid), ncol = 1)
      pred_cens[jrv & drv, ] <- bound(SL_predict(fit_cens, natural$valid[jrv & drv, vars_cens]), .Machine$double.eps)
    } else {
      fit_cens <- NULL
      pred_cens <- 1
    }

    fits[[t]] <- list()
    if (full_fits) {
      fits[[t]][["treatment"]] <- fit
      fits[[t]][["censoring"]] <- fit_cens
    } else {
      fits[[t]][["treatment"]] <- extract_sl_weights(fit)
      fits[[t]][["censoring"]] <- extract_sl_weights(fit_cens)
    }

    densratios[, t] <- (pi_d / pi)*(1 / pred_cens)*irv*drv

    pb()
  }

  list(ratios = densratios, d = d, fits = fits)
}
