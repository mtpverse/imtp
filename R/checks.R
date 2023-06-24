check_imtp_data <- function(x, trt, outcome, baseline, time_vary, cens, id) {
  for (t in 1:determine_tau(outcome, trt)) {
    ci <- censored(x, cens, t)$j
    di <- at_risk(x, risk_indicators(outcome), t, TRUE)
    trt_t <- ifelse(length(trt) == 1, trt, trt[t])

    data_t <- x[ci & di, c(trt_t, unique(unlist(baseline)), unique(unlist(lapply(time_vary, function(x) x[t])))), drop = FALSE]

    if (any(is.na(data_t))) {
      return("Missing data found in treatment and/or covariate nodes for uncensored observations")
    }
  }

  TRUE
}

assertImtpData <- checkmate::makeAssertionFunction(check_imtp_data)

check_trt_character <- function(trt, tau) {
  is_character <- checkmate::check_character(trt)
  if (!isTRUE(is_character)) {
    return(is_character)
  }

  if (length(trt) != 1 && length(trt) != tau) {
    return(paste0("'trt' should be of length 1 or ", tau))
  }

  TRUE
}

assertTrtCharacter <- checkmate::makeAssertionFunction(check_trt_character)

check_reserved_names <- function(x) {
  bad_names <- c("imtp_id", "tmp_imtp_stack_indicator", "tmp_imtp_scaled_outcome") %in% names(x)
  if (!any(bad_names)) {
    return(TRUE)
  }
  "'imtp_id', 'tmp_imtp_stack_indicator', and 'tmp_imtp_scaled_outcome' are reserved variable names"
}

assertReservedNames <- checkmate::makeAssertionFunction(check_reserved_names)

check_not_data_table <- function(x) {
  is_data_frame <- checkmate::checkDataFrame(x)
  if (!isTRUE(is_data_frame)) {
    return(is_data_frame)
  }

  is_data_table <- data.table::is.data.table(x)
  if (is_data_table) {
    return("Must be a 'data.frame', not a 'data.table'")
  }
  TRUE
}

assert_not_data_table <- assertNotDataTable <- checkmate::makeAssertionFunction(check_not_data_table)

check_outcome_types <- function(x, outcomes, outcome_type) {
  x <- x[, outcomes, drop = FALSE]
  all_numeric <- checkmate::testDataFrame(x, types = "numeric")
  if (!all_numeric) {
    return("Outcome variables must be of type numeric")
  }

  if (outcome_type %in% c("binomial", "survival")) {
    vals <- lapply(outcomes, function(var) as.character(unique(na.omit(x[[var]]))))
    all_binary <- all(unlist(vals) %in% c("0", "1"))

    if (!isTRUE(all_binary))
      return("Only 0 and 1 allowed in outcome variables if 'outcome_type' set to binomial or survival")
  }
  TRUE
}

assertOutcomeTypes <- checkmate::makeAssertionFunction(check_outcome_types)
