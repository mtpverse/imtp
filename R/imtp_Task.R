#' @importFrom R6 R6Class
imtp_Task <- R6::R6Class(
  "lmtp_Task",
  public = list(
    natural = NULL,
    shifted = NULL,
    delta = NULL,
    trt = NULL,
    cens = NULL,
    risk = NULL,
    node_list = NULL,
    n = NULL,
    tau = NULL,
    id = NULL,
    outcome_type = NULL,
    survival = NULL,
    bounds = NULL,
    folds = NULL,
    weights = NULL,
    initialize = function(data, trt, outcome, time_vary, baseline, cens, k, delta, id, outcome_type = NULL, V = 10, weights = NULL, bounds = NULL, bound = NULL) {
      self$tau <- determine_tau(outcome, trt)
      self$n <- nrow(data)
      self$trt <- trt
      self$risk <- risk_indicators(outcome)
      self$cens <- cens
      self$node_list <- create_node_list(trt, self$tau, time_vary, baseline, k)
      self$outcome_type <- ifelse(outcome_type %in% c("binomial", "survival"), "binomial", "continuous")
      self$survival <- outcome_type == "survival"
      self$bounds <- y_bounds(data[[final_outcome(outcome)]], self$outcome_type, bounds)
      data$lmtp_id <- create_ids(data, id)
      self$id <- data$lmtp_id
      self$folds <- setup_cv(data, data$lmtp_id, V)
      self$delta <- delta

      data <- data.table::copy(data)

      data <- fix_censoring_ind(data, cens)

      if (self$survival) {
        for (outcomes in outcome) {
          data.table::set(data, j = outcomes, value = convert_to_surv(data[[outcomes]]))
        }
      }

      data$tmp_imtp_scaled_outcome <- scale_y(data[[final_outcome(outcome)]], self$bounds)

      self$shifted <- self$natural <- data

      if (!is.null(weights)) {
        self$weights <- weights
      }
    },
    add_d = function(d) {
    	for (i in 1:ncol(d)) {
    		self$shifted[[self$trt[i]]] <- d[, i]
    	}
    	invisible(self)
    }
  )
)
