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
#' @return see glmNetSparse
#' @export
#'
#' @seealso glmNetSparse
glmOrphan <- function(xdata, ydata, network, network.options = network.options.default(), ...) {
  network.options$trans.fun <- orphan.heuristic
  glmSparseNet(xdata, ydata, network, network.options = network.options.default(), ...)
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
#' @return see glmNetSparse
#' @export
#'
#' @seealso glmNetSparse
glmHub <- function(xdata, ydata, network, network.options = network.options.default(), ...) {
  network.options$trans.fun <- hub.heuristic
  glmSparseNet(xdata, ydata, network, network.options = network.options, ...)
}

#' GLMNET model penalizing nodes with small degree
#'
#' This function overrides the `trans.fun` options in `network.options` with the
#' inverse of a degree described in Veríssimo et al. (2015) that penalizes
#' nodes with small degree.
#'
#' @param xdata input data, can be a matrix or MultiAssayExperiment
#' @param ydata response data compatible with glmnet
#' @param network type of network, see below
#' @param network.options options to calculate network
#' @param ... parameters that glmnet accepts
#'
#' @return see glmNetSparse
#' @export
#'
#' @seealso glmNetSparse
glmDegree <- function(xdata, ydata, network, network.options = network.options.default(), ...) {
  network.options$trans.fun <- function(x) { return(1 / x)}
  glmSparseNet(xdata, ydata, network, network.options = network.options, ...)
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
#' @return see cv.glmSparseNet
#' @export
#'
#' @seealso glmNetSparse
cv.glmOrphan <- function(xdata, ydata, network, network.options = network.options.default(), ...) {
  network.options$trans.fun <- orphan.heuristic
  cv.glmSparseNet(xdata, ydata, network, network.options = network.options, ...)
}

#' GLMNET cross-validation model penalizing nodes with small degree
#'
#' This function overrides the `trans.fun` options in `network.options` with the
#' inverse of a degree described in Veríssimo et al. (2015) that penalizes
#' nodes with small degree.
#'
#' @param xdata input data, can be a matrix or MultiAssayExperiment
#' @param ydata response data compatible with glmnet
#' @param network type of network, see below
#' @param network.options options to calculate network
#' @param ... parameters that glmnet accepts
#'
#' @return see cv.glmSparseNet
#' @export
#'
#' @seealso glmNetSparse
cv.glmDegree <- function(xdata, ydata, network, network.options = network.options.default(), ...) {
  network.options$trans.fun <- function(x) { 1 / x}
  cv.glmSparseNet(xdata, ydata, network, network.options = network.options, ...)
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
#' @return see cv.glmSparseNet
#' @export
#'
#' @seealso glmNetSparse
cv.glmHub <- function(xdata, ydata, network, network.options = network.options.default(), ...) {
  network.options$trans.fun <- hub.heuristic
  cv.glmSparseNet(xdata, ydata, network, network.options = network.options, ...)
}
