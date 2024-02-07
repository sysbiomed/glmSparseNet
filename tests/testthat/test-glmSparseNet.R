test_that("glmSparseNet: simple call", {
  xdata <- matrix(rnorm(100), ncol = 20)
  glmSparseNet(xdata, rnorm(nrow(xdata)), "correlation", family = "gaussian") |>
    expect_s3_class("glmnet")
})

test_that("glmSparseNet: simple call with MultiAssayExperiment", {
  data("miniACC", package = "MultiAssayExperiment")

  xdata <- miniACC

  event_ix <- which(!is.na(xdata$days_to_death))
  cens_ix <- which(!is.na(xdata$days_to_last_followup))

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

  glmSparseNet(
    xdata_valid,
    ydata_valid,
    family = "cox",
    network = "correlation",
    experiment = "RNASeq2GeneNorm"
  ) |>
    expect_warning("'experiments' dropped;") |>
    expect_s3_class("glmnet")
})

test_that("cv.glmSparseNet: simple call", {
  xdata <- matrix(rnorm(500), ncol = 5)
  cv.glmSparseNet(
    xdata,
    rnorm(nrow(xdata)), "correlation",
    family = "gaussian"
  ) |>
    expect_s3_class("cv.glmnet")
})

test_that("cv.glmSparseNet: simple call", {
  #
  # load data
  data("miniACC", package = "MultiAssayExperiment")
  xdata <- miniACC

  #
  # build valid data with days of last follow up or to event
  event.ix <- which(!is.na(xdata$days_to_death))
  cens.ix <- which(!is.na(xdata$days_to_last_followup))
  xdata$surv_event_time <- array(
    NA_integer_, nrow(MultiAssayExperiment::colData(xdata))
  )
  xdata$surv_event_time[event.ix] <- xdata$days_to_death[event.ix]
  xdata$surv_event_time[cens.ix] <- xdata$days_to_last_followup[cens.ix]

  #
  # Keep only valid individuals
  valid.ix <- as.vector(!is.na(xdata$surv_event_time) &
    !is.na(xdata$vital_status) &
    xdata$surv_event_time > 0)
  xdata.valid <- xdata[
    , rownames(MultiAssayExperiment::colData(xdata))[valid.ix]
  ]
  ydata.valid <- MultiAssayExperiment::colData(
    xdata.valid
  )[, c("surv_event_time", "vital_status")]
  colnames(ydata.valid) <- c("time", "status")

  #
  cv.glmSparseNet(xdata.valid,
    ydata.valid,
    nfolds = 5,
    family = "cox",
    network = "correlation",
    experiment = "RNASeq2GeneNorm"
  ) |>
    expect_warning("'experiments' dropped;") |>
    expect_s3_class("cv.glmnet")
})
