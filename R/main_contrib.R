#' GLMNET model penalizing nodes with high degree
#'
#' This function overrides the `trans.fun` options in `network.options` with
#' an heuristic described in Veríssimo et al. that penalizes nodes with high degree.
#'
#' @param xdata input data, can be a matrix or MultiAssayExperiment
#' @param ydata response data compatible with glmnet
#' @param network type of network, see below
#' @param network.options options to calculate network
#' @param ... parameters that glmnet accepts
#'
#' @return see network.glmnet
#' @export
#'
#' @seealso network.glmnet
glmOrphan <- function(xdata, ydata, network, network.options = network.options.default(), ...) {
  network.options$trans.fun <- orphan.heuristic
  network.glmnet(xdata, ydata, network, network.options = network.options.default(), ...)
}

#' GLMNET model penalizing nodes with small degree
#'
#' This function overrides the `trans.fun` options in `network.options` with
#' an heuristic described in Veríssimo et al. that penalizes nodes with small degree.
#'
#' @param xdata input data, can be a matrix or MultiAssayExperiment
#' @param ydata response data compatible with glmnet
#' @param network type of network, see below
#' @param network.options options to calculate network
#' @param ... parameters that glmnet accepts
#'
#' @return see network.glmnet
#' @export
#'
#' @seealso network.glmnet
glmDegree <- function(xdata, ydata, network, network.options = network.options.default(), ...) {
  network.options$trans.fun <- degree.heuristic
  network.glmnet(xdata, ydata, network, network.options = network.options, ...)
}

#' GLMNET cross-validation model penalizing nodes with high degree
#'
#' This function overrides the `trans.fun` options in `network.options` with
#' an heuristic described in Veríssimo et al. that penalizes nodes with high degree.
#'
#' @param xdata input data, can be a matrix or MultiAssayExperiment
#' @param ydata response data compatible with glmnet
#' @param network type of network, see below
#' @param network.options options to calculate network
#' @param ... parameters that glmnet accepts
#'
#' @return see network.cv.glmnet
#' @export
#'
#' @seealso network.glmnet
cv.glmOrphan <- function(xdata, ydata, network, network.options = network.options.default(), ...) {
  network.options$trans.fun <- orphan.heuristic
  network.cv.glmnet(xdata, ydata, network, network.options = network.options, ...)
}

#' GLMNET cross-validation model penalizing nodes with small degree
#'
#' This function overrides the `trans.fun` options in `network.options` with
#' an heuristic described in Veríssimo et al. that penalizes nodes with small degree.
#'
#' @param xdata input data, can be a matrix or MultiAssayExperiment
#' @param ydata response data compatible with glmnet
#' @param network type of network, see below
#' @param network.options options to calculate network
#' @param ... parameters that glmnet accepts
#'
#' @return see network.cv.glmnet
#' @export
#'
#' @seealso network.glmnet
cv.glmDegree <- function(xdata, ydata, network, network.options = network.options.default(), ...) {
  network.options$trans.fun <- degree.heuristic
  network.cv.glmnet(xdata, ydata, network, network.options = network.options, ...)
}
