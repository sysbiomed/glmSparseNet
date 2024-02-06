#' GLMNET model penalizing nodes with high degree
#'
#' This function overrides the `trans.fun` options in `network.options` with
#' an heuristic described in Veríssimo et al. that penalizes nodes with high
#' degree.
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
#' @examples
#' xdata <- matrix(rnorm(100), ncol = 5)
#' glmOrphan(xdata, rnorm(nrow(xdata)), "correlation",
#'   family = "gaussian",
#'   network.options = networkOptions(min.degree = .2)
#' )
glmOrphan <- function(xdata, ydata, network,
                      network.options = networkOptions(), ...) {
  network.options$trans.fun <- orphanHeuristic
  glmSparseNet(xdata, ydata, network,
               network.options = networkOptions(), ...
  )
}

#' GLMNET cross-validation model penalizing nodes with high degree
#'
#' This function overrides the `trans.fun` options in `network.options` with
#' an heuristic described in Veríssimo et al. that penalizes nodes with high
#' degree.
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
#' @examples
#' xdata <- matrix(rnorm(100), ncol = 5)
#' cv.glmOrphan(xdata, rnorm(nrow(xdata)), "correlation",
#'   family = "gaussian",
#'   nfolds = 5,
#'   network.options = networkOptions(min.degree = .2)
#' )
cv.glmOrphan <- function(xdata, ydata, network,
                         network.options = networkOptions(), ...) {
  network.options$trans.fun <- orphanHeuristic
  cv.glmSparseNet(xdata, ydata, network,
                  network.options = network.options, ...
  )
}
