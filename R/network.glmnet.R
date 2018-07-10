#' Calculate GLM model with network-based regularization
#'
#' network parameter accepts:
#'
#'  * string to calculate network based on data (correlation, covariance)
#'  * matrix representing the network
#'  * vector with already calculated penalty weights (can also be used directly with glmnet)
#'
#' @param xdata input data, can be a matrix or MultiAssayExperiment
#' @param ydata response data compatible with glmnet
#' @param network type of network, see below
#' @param network.options options to calculate network
#' @param ... parameters that glmnet accepts
#'
#' @return an object just as glmnet
#' @export
#'
#' @examples
#' xdata <- matrix(rnorm(100), ncol = 20)
#' network.glmnet(xdata, rnorm(nrow(xdata)), 'correlation', family = 'gaussian')
#' network.glmnet(xdata, rnorm(nrow(xdata)), 'covariance', family = 'gaussian')
#'
#' #
#' #
#' # Using MultiAssayExperiment
#'
#' # load data
#' xdata <- MultiAssayExperiment::miniACC
#' # TODO aking out x indivudals missing values
#' # build valid data with days of last follow up or to event
#' event.ix <- which(!is.na(xdata$days_to_death))
#' cens.ix <- which(!is.na(xdata$days_to_last_followup))
#' xdata$surv_event_time <- array(NA, nrow(xdata@colData))
#' xdata$surv_event_time[event.ix] <- xdata$days_to_death[event.ix]
#' xdata$surv_event_time[cens.ix] <- xdata$days_to_last_followup[cens.ix]
#' # Keep only valid individuals
#' valid.ix <- as.vector(!is.na(xdata$surv_event_time) &
#'                       !is.na(xdata$vital_status) &
#'                       xdata$surv_event_time > 0)
#' xdata.valid <- xdata[, rownames(xdata@colData)[valid.ix]]
#' ydata.valid <- xdata.valid@colData[,c('surv_event_time', 'vital_status')]
#' colnames(ydata.valid) <- c('time', 'status')
#' network.glmnet(xdata.valid,
#'                ydata.valid,
#'                family          = 'cox',
#'                network         = 'correlation',
#'                experiment.name = 'RNASeq2GeneNorm')
setGeneric('network.glmnet', function(xdata, ydata, network, network.options = network.options.default(), ...) {
  stop('wrong arguments, see help for network.glmnet')
})

#' Calculate GLM model with network-based regularization
#'
#' @param xdata input data as a Matrix
#' @param ydata response data compatible with glmnet
#' @param network type of network, see below
#' @param network.options options to calculate network
#' @param ... parameters that glmnet accepts
#'
#' @return an object just as glmnet
#' @export
#' @import Matrix
setMethod('network.glmnet', signature(xdata = 'matrix'), function(xdata, ydata, network,
                                                                  network.options = network.options.default(), ...) {
  return(network.glmnet.private(glmnet::glmnet, xdata, ydata, network = network, network.options = network.options, ...))
})

#' Calculate GLM model with network-based regularization
#'
#' @param xdata input data as a MultiAssayExperiment
#' @param ydata response data compatible with glmnet
#' @param experiment.name name of experiment to use as input in MultiAssayExperiment object
#' @param network type of network, see below
#' @param network.options options to calculate network
#' @param ... parameters that glmnet accepts
#'
#' @return an object just as glmnet
#' @export
#' @import MultiAssayExperiment
setMethod('network.glmnet', signature(xdata = 'MultiAssayExperiment'), function(xdata, ydata, network,
                                                                                experiment.name = NULL,
                                                                                network.options = network.options.default(), ...) {
  return(network.glmnet.private(glmnet::glmnet, xdata, ydata, network, experiment.name = experiment.name, network.options = network.options, ...))
})


#' Calculate GLM model with network-based regularization
#'
#' @param xdata input data as a SummarizedExperiment
#' @param ydata response data compatible with glmnet
#' @param network type of network, see below
#' @param network.options options to calculate network
#' @param ... parameters that glmnet accepts
#'
#' @return an object just as glmnet
#' @export
#' @import SummarizedExperiment
setMethod('network.glmnet', signature(xdata = 'SummarizedExperiment'), function(xdata, ydata, network,
                                                                                network.options = network.options.default(), ...) {
  return(network.glmnet.private(glmnet::glmnet, t(MultiAssayExperiment::assay(xdata)), ydata, network, network.options = network.options, ...))
})



