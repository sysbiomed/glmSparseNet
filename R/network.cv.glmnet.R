#' Calculate cross validating GLM model with network-based regularization
#'
#' @param xdata input data, can be a matrix or MultiAssayExperiment
#' @param ydata response data compatible with glmnet
#' @param network type of network, see below
#' @param network.options options to calculate network
#' @param ... parameters that cv.glmnet accepts
#'
#' @return an object just as cv.glmnet
#' @export
#'
#' network parameter accepts:
#'
#'  * string to calculate network based on data (correlation, covariance)
#'  * matrix representing the network
#'  * vector with already calculated penalty weights (can also be used directly with glmnet)
#'
#' @examples
#' xdata <- matrix(rnorm(100), ncol = 20)
#' network.cv.glmnet(xdata, rnorm(nrow(xdata)), 'correlation', family = 'gaussian')
#' network.cv.glmnet(xdata, rnorm(nrow(xdata)), 'covariance', family = 'gaussian')
#'
#' #
#' #
#' # Using MultiAssayExperiment
#'
#' # load data
#' xdata <- MultiAssayExperiment::miniACC
#' # build valid data with days of last follow up or to event
#' event.ix <- which(!is.na(xdata$days_to_death))
#' cens.ix <- which(!is.na(xdata$days_to_last_followup))
#' xdata$surv_event_time <- array(NA, nrow(xdata@colData))
#' xdata$surv_event_time[event.ix] <- xdata$days_to_death[event.ix]
#' xdata$surv_event_time[cens.ix] <- xdata$days_to_last_followup[cens.ix]
#' # Keep only valid individuals
#' valid.ix <- as.vector(!is.na(xdata$surv_event_time) & !is.na(xdata$vital_status) & xdata$surv_event_time > 0)
#' xdata.valid <- xdata[, rownames(xdata@colData)[valid.ix]]
#' ydata.valid <- xdata.valid@colData[,c('surv_event_time', 'vital_status')]
#' colnames(ydata.valid) <- c('time', 'status')
#' network.cv.glmnet(xdata.valid, ydata.valid, family = 'cox', network = 'correlation', experiment.name = 'RNASeq2GeneNorm')
setGeneric('network.cv.glmnet', function(xdata, ydata, network, network.options = network.options.default(), ...) {
  stop('wrong arguments, see help for network.cv.glmnet and cv.glmnet')
})


#' Calculate GLM model with network-based regularization
#'
#' @param xdata matrix.
#'
#' @return an object just as glmnet
#' @export
#'
setMethod('network.cv.glmnet', signature(xdata = 'matrix'), function(xdata, ydata, network,
                                                                     network.options = network.options.default(), ...) {
  return(network.glmnet.private(glmnet::cv.glmnet, xdata, ydata, network, network.options = network.options, ...))
})

#' Calculate GLM model with network-based regularization
#'
#' @param xdata MultiAssayExperiment.
#'
#' @return an object just as glmnet
#' @export
#'
setMethod('network.cv.glmnet', signature(xdata = 'MultiAssayExperiment'), function(xdata, ydata, network,
                                                                                   experiment.name,
                                                                                   network.options = network.options.default(), ...) {
  return(network.glmnet.private(glmnet::cv.glmnet, xdata, ydata, network, experiment.name = experiment.name , network.options, ...))
})


#' Calculate GLM model with network-based regularization
#'
#' @param xdata SummarizedExperiment.
#'
#' @return an object just as glmnet
#' @export
#'
setMethod('network.cv.glmnet', signature(xdata = 'SummarizedExperiment'), function(xdata, ydata, network,
                                                                                   network.options = network.options.default(), ...) {
  return(network.glmnet.private(glmnet::cv.glmnet, xdata, ydata, network, network.options, ...))
})



