#' GLMNET model penalizing nodes with high degree
#'
#' This function overrides the `trans.fun` options in `network.options` with
#' an heuristic described in Veríssimo et al. that penalizes nodes with high
#' degree.
#'
#' @inheritParams glmSparseNet
#' @inherit glmSparseNet return
#' @export
#'
#' @seealso Generic function without pre-defined penalization: [glmNetSparse()].
#' Other penalizations: [glmDegree()] and [glmHub()].
#' Cross-validation with the same penalization: [cv.glmOrphan()].
#' @examples
#' xdata <- matrix(rnorm(100), ncol = 5)
#' glmOrphan(xdata, rnorm(nrow(xdata)), "correlation",
#'   family = "gaussian",
#'   network.options = networkOptions(min.degree = .2)
#' )
glmOrphan <- function(
    xdata,
    ydata,
    network,
    network.options = networkOptions(),
    ...) {
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
#' @inheritParams cv.glmSparseNet
#' @inherit cv.glmSparseNet return
#' @export
#'
#' @seealso Generic function without pre-defined penalization: [cv.glmNetSparse()].
#' Other penalizations: [cv.glmDegree()] and [cv.glmHub()].
#' Model with the same penalization: [glmOrphan()].
#' @examples
#' xdata <- matrix(rnorm(100), ncol = 5)
#' cv.glmOrphan(xdata, rnorm(nrow(xdata)), "correlation",
#'   family = "gaussian",
#'   nfolds = 5,
#'   network.options = networkOptions(min.degree = .2)
#' )
cv.glmOrphan <- function(
    xdata,
    ydata,
    network,
    network.options = networkOptions(),
    ...) {
  network.options$trans.fun <- orphanHeuristic
  cv.glmSparseNet(xdata, ydata, network,
    network.options = network.options, ...
  )
}
