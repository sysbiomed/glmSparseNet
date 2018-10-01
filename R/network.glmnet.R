#' Calculate GLM model with network-based regularization
#'
#' network parameter accepts:
#'
#'  * string to calculate network based on data (correlation, covariance)
#'  * matrix representing the network
#'  * vector with already calculated penalty weights (can also be used directly
#'    with glmnet)
#'
#' @param xdata input data, can be a matrix or MultiAssayExperiment
#' @param ydata response data compatible with glmnet
#' @param network type of network, see below
#' @param network.options options to calculate network
#' @param experiment.name name of experiment to use as input in
#' MultiAssayExperiment object (only if xdata is an object of this class)
#' @param ... parameters that glmnet accepts
#'
#' @return an object just as glmnet
#' @export
#'
#' @examples
#' xdata <- matrix(rnorm(100), ncol = 20)
#' glmSparseNet(xdata, rnorm(nrow(xdata)), 'correlation', family = 'gaussian')
#' glmSparseNet(xdata, rnorm(nrow(xdata)), 'covariance', family = 'gaussian')
#'
#' #
#' #
#' # Using MultiAssayExperiment
#' # load data
#' xdata <- MultiAssayExperiment::miniACC
#' # TODO aking out x indivudals missing values
#' # build valid data with days of last follow up or to event
#' event.ix <- which(!is.na(xdata$days_to_death))
#' cens.ix <- which(!is.na(xdata$days_to_last_followup))
#' xdata$surv_event_time <- array(NA, nrow(colData(xdata)))
#' xdata$surv_event_time[event.ix] <- xdata$days_to_death[event.ix]
#' xdata$surv_event_time[cens.ix] <- xdata$days_to_last_followup[cens.ix]
#' # Keep only valid individuals
#' valid.ix <- as.vector(!is.na(xdata$surv_event_time) &
#'                       !is.na(xdata$vital_status) &
#'                       xdata$surv_event_time > 0)
#' xdata.valid <- xdata[, rownames(colData(xdata))[valid.ix]]
#' ydata.valid <- colData(xdata.valid)[,c('surv_event_time', 'vital_status')]
#' colnames(ydata.valid) <- c('time', 'status')
#' glmSparseNet(xdata.valid,
#'              ydata.valid,
#'              family          = 'cox',
#'              network         = 'correlation',
#'              experiment.name = 'RNASeq2GeneNorm')
glmSparseNet <- function(xdata, ydata, network,
                         network.options = network.options.default(),
                         experiment.name = NULL, ...) {
    .glmSparseNetPrivate(glmnet::glmnet, xdata, ydata,
                         network = network,
                         network.options = network.options,
                         experiment.name = experiment.name,
                         ...)
}
