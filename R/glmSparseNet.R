#' Calculate GLM model with network-based regularization
#'
#' network parameter accepts:
#' * string to calculate network based on data (correlation, covariance)
#' * matrix representing the network
#' * vector with already calculated penalty weights (can also be used directly
#' with glmnet)
#'
#' @order 0
#' @param xdata input data, can be a matrix or MultiAssayExperiment.
#' @param ydata response data compatible with glmnet.
#' @param network type of network, see below.
#' @param options options to calculate network.
#' @param experiment name of experiment to use as input in
#' MultiAssayExperiment object (only if xdata is an object of this class).
#' @param network.options `r lifecycle::badge("deprecated")`
#' @param experiment.name `r lifecycle::badge("deprecated")`
#' @param ... parameters that [glmnet::glmnet()] accepts.
#'
#' @return an object just as glmnet
#' @export
#'
#' @seealso Other model functions with pre-defined penalization:
#' [glmDegree()], [glmHub()] and [glmOrphan()].
#' Cross-validation with the same penalization: [cv.glmSparseNet()].
#'
#' @examples
#' xdata <- matrix(rnorm(100), ncol = 20)
#' glmSparseNet(xdata, rnorm(nrow(xdata)), "correlation", family = "gaussian")
#' glmSparseNet(xdata, rnorm(nrow(xdata)), "covariance", family = "gaussian")
#'
#' #
#' #
#' # Using MultiAssayExperiment
#' # load data
#' data("miniACC", package = "MultiAssayExperiment")
#' xdata <- miniACC
#' # TODO aking out x individuals missing values
#' # build valid data with days of last follow up or to event
#' event.ix <- which(!is.na(xdata$days_to_death))
#' cens.ix <- which(!is.na(xdata$days_to_last_followup))
#' xdata$surv_event_time <- array(NA, nrow(colData(xdata)))
#' xdata$surv_event_time[event.ix] <- xdata$days_to_death[event.ix]
#' xdata$surv_event_time[cens.ix] <- xdata$days_to_last_followup[cens.ix]
#' # Keep only valid individuals
#' valid.ix <- as.vector(!is.na(xdata$surv_event_time) &
#'   !is.na(xdata$vital_status) &
#'   xdata$surv_event_time > 0)
#' xdata.valid <- xdata[, rownames(colData(xdata))[valid.ix]]
#' ydata.valid <- colData(xdata.valid)[, c("surv_event_time", "vital_status")]
#' colnames(ydata.valid) <- c("time", "status")
#' glmSparseNet(
#'   xdata.valid,
#'   ydata.valid,
#'   family          = "cox",
#'   network         = "correlation",
#'   experiment.name = "RNASeq2GeneNorm"
#' )
glmSparseNet <- function(
    xdata,
    ydata,
    network,
    options = networkOptions(),
    experiment = NULL,
    network.options = deprecated(),
    experiment.name = deprecated(),
    ...) {
  # Lifecycle management: to remove after 1.23.0
  if (lifecycle::is_present(network.options)) {
    .deprecatedDotParam("cv.glmSparseNet", "network.options")
    options <- network.options
  }
  if (lifecycle::is_present(experiment.name)) {
    .deprecatedDotParam("cv.glmSparseNet", "experiment.name")
    experiment <- experiment.name
  }
  # Lifecycle management: end

  .glmSparseNetPrivate(
    glmnet::glmnet,
    xdata,
    ydata,
    network = network,
    options = options,
    experiment = experiment,
    ...
  )
}

#' Calculate cross validating GLM model with network-based regularization
#'
#' network parameter accepts:
#'
#' * string to calculate network based on data (correlation, covariance)
#' * matrix representing the network
#' * vector with already calculated penalty weights (can also be used directly
#' glmnet)
#'
#' @inheritParams glmSparseNet
#' @param ... parameters that [glmnet::cv.glmnet()] accepts.
#'
#' @return an object just as `cv.glmnet`
#' @export
#'
#' @seealso Other cross-validation functions: [cv.glmDegree()], [cv.glmHub()] and [cv.glmOrphan()].
#' Model with the same penalization: [glmSparseNet()].
#'
#' @examples
#' \donttest{
#' # Gaussian model
#' xdata <- matrix(rnorm(500), ncol = 5)
#' cv.glmSparseNet(xdata, rnorm(nrow(xdata)), "correlation",
#'   family = "gaussian"
#' )
#' cv.glmSparseNet(xdata, rnorm(nrow(xdata)), "covariance",
#'   family = "gaussian"
#' )
#' }
#'
#' #
#' #
#' # Using MultiAssayExperiment with survival model
#'
#'
#' #
#' # load data
#' data("miniACC", package = "MultiAssayExperiment")
#' xdata <- miniACC
#'
#' #
#' # build valid data with days of last follow up or to event
#' event.ix <- which(!is.na(xdata$days_to_death))
#' cens.ix <- which(!is.na(xdata$days_to_last_followup))
#' xdata$surv_event_time <- array(NA, nrow(colData(xdata)))
#' xdata$surv_event_time[event.ix] <- xdata$days_to_death[event.ix]
#' xdata$surv_event_time[cens.ix] <- xdata$days_to_last_followup[cens.ix]
#'
#' #
#' # Keep only valid individuals
#' valid.ix <- as.vector(!is.na(xdata$surv_event_time) &
#'   !is.na(xdata$vital_status) &
#'   xdata$surv_event_time > 0)
#' xdata.valid <- xdata[, rownames(colData(xdata))[valid.ix]]
#' ydata.valid <- colData(xdata.valid)[, c("surv_event_time", "vital_status")]
#' colnames(ydata.valid) <- c("time", "status")
#'
#' #
#' cv.glmSparseNet(
#'   xdata.valid,
#'   ydata.valid,
#'   nfolds          = 5,
#'   family          = "cox",
#'   network         = "correlation",
#'   experiment.name = "RNASeq2GeneNorm"
#' )
cv.glmSparseNet <- function(
    xdata,
    ydata,
    network,
    options = networkOptions(),
    experiment = NULL,
    network.options = deprecated(),
    experiment.name = deprecated(),
    ...) {
  # Lifecycle management: to remove after 1.23.0
  if (lifecycle::is_present(network.options)) {
    .deprecatedDotParam("cv.glmSparseNet", "network.options")
    options <- network.options
  }
  if (lifecycle::is_present(experiment.name)) {
    .deprecatedDotParam("cv.glmSparseNet", "experiment.name")
    experiment <- experiment.name
  }
  # Lifecycle management: end

  .glmSparseNetPrivate(
    glmnet::cv.glmnet,
    xdata,
    ydata,
    network,
    experiment = experiment,
    options = options,
    ...
  )
}
