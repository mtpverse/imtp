#' Create a node list specification
#'
#' Creates a node list specification that is used by the provided estimators.
#' \code{create_node_list()} is not explicitly called by the analyst, rather
#' it is provided so the analyst can confirm how estimators will use variables
#' before actually performing the estimation procedure.
#'
#' @param trt A vector of column names of treatment variables.
#' @param tau The number of time points of observation, excluding the final outcome.
#' @param time_vary \[\code{list(tau)}\]\cr See examples
#' @param baseline \[\code{list(tau)}\]\cr See examples
#' @param k An integer specifying how previous time points should be
#'  used for estimation at the given time point. Default is \code{Inf},
#'  all time points.
#'
#' @return A list of lists. Each sub-list is the same length of the
#' \code{time_vary} parameter with the variables to be used for estimation at that given time point
#' for either the treatment mechanism or outcome regression.
#' @export
#' @examples
#' a <- c("A_1", "A_2", "A_3", "A_4")
#' bs <- c("W1", "W2")
#' time_vary <- list(c("L_1"), c("L_2"), c("L_3"), c("L_4"))
#'
#' # assuming no Markov property
#' create_node_list(a, 4, time_vary, bs, k = Inf)
#'
#' # assuming a Markov property
#' create_node_list(a, 4, time_vary, bs, k = 0)
create_node_list <- function(trt, tau, time_vary = NULL, baseline = NULL, k = Inf) {
  if (is.null(k)) {
    k <- Inf
  }

  list(
    trt = trt_node_list(trt, time_vary, baseline, k, tau),
    cens = outcome_node_list(trt, time_vary, baseline, k, tau),
    outcome = outcome_node_list(trt, time_vary, baseline, k, tau)
  )
}

trt_node_list <- function(trt, time_vary, baseline = NULL, k, tau) {
  out <- list()
  if (!is.null(baseline)) {
    for (i in 1:tau) {
      out[[i]] <- c(baseline)
    }
  }

  if (length(out) == 0) {
    if (length(trt) == tau) {
      for (i in 1:tau) {
        if (i > 1) {
          out[[i]] <- c(time_vary[[i]], trt[i - 1])
        } else {
          out[[i]] <- c(time_vary[[i]])
        }
      }
    }

    if (length(trt) != tau) {
      for (i in 1:tau) {
        out[[i]] <- c(time_vary[[i]])
      }
    }
  } else {
    if (length(trt) == tau) {
      for (i in 1:tau) {
        if (i > 1) {
          out[[i]] <- c(out[[i]], time_vary[[i]], trt[i - 1])
        } else {
          out[[i]] <- c(out[[i]], time_vary[[i]])
        }
      }
    }

    if (length(trt) != tau) {
      for (i in 1:tau) {
        out[[i]] <- c(out[[i]], time_vary[[i]])
      }
    }
  }

  slide(out, k)
}

outcome_node_list <- function(trt, time_vary, baseline = NULL, k, tau) {
  out <- list()

  if (length(trt) == tau) {
    for (i in 1:tau) {
      out[[i]] <- c(time_vary[[i]], trt[i])
    }
  }

  if (length(trt) != tau) {
    for (i in 1:tau) {
      out[[i]] <- c(time_vary[[i]], trt)
    }
  }

  out <- slide(out, k)
  if (is.null(baseline)) {
    return(out)
  }

  for (i in 1:tau) {
    out[[i]] <- c(baseline, out[[i]])
  }
  out
}

slide <- function(x, k) {
  if (k == 0) {
    return(x)
  }

  t <- length(x)
  if (k == Inf) {
    k <- t - 1
  }
  lapply(1:t, Lag, x = x, k = k)
}

Lag <- function(x, t, k) {
  if (t == 1) {
    return(x[[1]])
  }
  tk <- max(1, t - k)
  unique(do.call(c, x[tk:t]))
}
