random_xdata <- function(n_values = 30000, n_row = 500, seed = 1985) {
  set.seed(seed)
  xdata <- matrix(rnorm(n_values), nrow = n_row)
}

prepare_mae <- function() {
  tmp_env <- new.env()
  data("miniACC", package = "MultiAssayExperiment", envir = tmp_env)

  xdata <- tmp_env$miniACC

  event_ix <- which(!is.na(
    MultiAssayExperiment::colData(xdata)$days_to_death
  ))
  cens_ix <- which(!is.na(
    MultiAssayExperiment::colData(xdata)$days_to_last_followup
  ))

  xdata$surv_event_time <- array(
    NA_integer_, nrow(MultiAssayExperiment::colData(xdata))
  )
  xdata$surv_event_time[event_ix] <- xdata$days_to_death[event_ix]
  xdata$surv_event_time[cens_ix] <- xdata$days_to_last_followup[cens_ix]

  # Keep only valid individuals
  valid_ix <- as.vector(
    !is.na(xdata$surv_event_time) &
      !is.na(xdata$vital_status) &
      xdata$surv_event_time > 0
  )
  xdata_valid <- xdata[
    , rownames(MultiAssayExperiment::colData(xdata))[valid_ix]
  ]
  ydata_valid <- MultiAssayExperiment::colData(
    xdata_valid
  )[, c("surv_event_time", "vital_status")]
  colnames(ydata_valid) <- c("time", "status")

  list(xdata = xdata_valid, ydata = ydata_valid)
}
