#' Calculate GLM model with network-based regularization
#'
#' @param fun function to be called (glmnet or cv.glmnet)
#' @param xdata input data, can be a matrix or MultiAssayExperiment
#' @param ydata response data compatible with glmnet
#' @param network type of network, see below
#' @param network.options options to calculate network
#' @param ... parameters that glmnet accepts
#'
#' @return an object just as glmnet
#'
#' network parameter accepts:
#'
#'  * string to calculate network based on data (correlation, covariance)
#'  * matrix representing the network
#'  * vector with already calculated penalty weights (can also be used directly with glmnet)
#'
setGeneric('network.glmnet.private', function(fun, xdata, ydata, network, network.options = network.options.default(), ...) {
  stop('wrong arguments, see help for network.glmnet')
})


#' Calculate GLM model with network-based regularization
#'
#' @param xdata matrix.
#'
#' @return an object just as glmnet
#'
setMethod('network.glmnet.private', signature(xdata = 'matrix'), function(fun, xdata, ydata, network, network.options = network.options.default(), ...) {
  if (is.character(network)) {
    penalty.factor <- calc.penalty(xdata, network, network.options)
  } else if (is.matrix(network)) {
    penalty.factor <- (colSums(network) + rowSums(network)) / 2
  } else if (is.vector(network)) {
    if (lenght(network) != ncol(xdata)) {
      stop('Network vector size does not match xdata input')
    }
    penalty.factor <- network
  } else {
    stop('There was an error with network argumnet')
  }
  return(fun(xdata, ydata, penalty.factor = penalty.factor, ...))
})

#' Calculate GLM model with network-based regularization
#'
#' @param xdata MultiAssayExperiment.
#'
#' @return an object just as glmnet
#'
setMethod('network.glmnet.private', signature(xdata = 'MultiAssayExperiment'), function(fun, xdata, ydata, network,
                                                                                        experiment.name = NULL,
                                                                                        network.options = network.options.default(), ...) {
  if (is.null(experiment.name)) {
    stop('Experiment name must be passed in network.options, see documentation.')
  }
  valid.ydata.id <- xdata@sampleMap[xdata@sampleMap$assay == experiment.name, 'primary']
  if (is.matrix(ydata) || is.data.frame(ydata) || inherits(ydata, 'DataFrame')) {
    if (!is.null(rownames(ydata))) {
      ydata <- as.matrix(ydata[valid.ydata.id,])
    }
  } else if (is.array(ydata) && !is.null(names(ydata))) {
    ydata <- ydata[valid.ydata.id]
  }
  return(network.glmnet.private(fun, xdata[[experiment.name]], ydata, network, network.options, ...))
})


#' Calculate GLM model with network-based regularization
#'
#' @param xdata SummarizedExperiment.
#'
#' @return an object just as glmnet
#'
setMethod('network.glmnet.private', signature(xdata = 'SummarizedExperiment'), function(fun, xdata, ydata, network,
                                                                                        network.options = network.options.default(), ...) {
  return(network.glmnet.private(fun, t(assay(xdata)), ydata, network, network.options, ...))
})



