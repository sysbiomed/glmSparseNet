#' GLMNET model penalizing nodes with small degree
#'
#' This function overrides the `trans.fun` options in `network.options` with
#' an heuristic described in Veríssimo et al. that penalizes nodes with small
#' degree.
#'
#' @inheritParams glmSparseNet
#' @inherit glmSparseNet return
#' @export
#'
#' @seealso Generic function without pre-defined penalization: [glmNetSparse()].
#' Other penalizations: [glmDegree()] and [glmOrphan()].
#' Cross-validation with the same penalization: [cv.glmHub()].
#' @examples
#' xdata <- matrix(rnorm(100), ncol = 5)
#' glmHub(xdata, rnorm(nrow(xdata)), "correlation",
#'   family = "gaussian",
#'   network.options = networkOptions(min.degree = .2)
#' )
glmHub <- function(
    xdata,
    ydata,
    network,
    network.options = networkOptions(),
    ...) {
  network.options$trans.fun <- hubHeuristic
  glmSparseNet(xdata, ydata, network,
    network.options = network.options, ...
  )
}

#' GLMNET cross-validation model penalizing nodes with small degree
#'
#' This function overrides the `trans.fun` options in `network.options` with
#' an heuristic described in Veríssimo et al. that penalizes nodes with small
#' degree.
#'
#' @inheritParams cv.glmSparseNet
#' @inherit cv.glmSparseNet return
#' @export
#'
#' @seealso Generic function without pre-defined penalization: [cv.glmNetSparse()].
#' Other penalizations: [cv.glmDegree()] and [cv.glmOrphan()].
#' Model with the same penalization: [glmHub()].
#' @examples
#' xdata <- matrix(rnorm(100), ncol = 5)
#' cv.glmHub(xdata, rnorm(nrow(xdata)), "correlation",
#'   family = "gaussian",
#'   nfolds = 5,
#'   network.options = networkOptions(min.degree = .2)
#' )
cv.glmHub <- function(
    xdata,
    ydata,
    network,
    network.options = networkOptions(),
    ...) {
  network.options$trans.fun <- hubHeuristic
  cv.glmSparseNet(xdata, ydata, network,
    network.options = network.options, ...
  )
}
