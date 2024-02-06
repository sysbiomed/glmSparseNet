#' GLMNET model penalizing nodes with small degree
#'
#' This function overrides the `trans.fun` options in `network.options` with the
#' inverse of a degree described in Veríssimo et al. (2015) that penalizes
#' nodes with small degree.
#'
#' @inheritParams glmSparseNet
#' @inherit glmSparseNet return
#' @export
#'
#' @seealso Generic function without pre-defined penalization: [glmNetSparse()].
#' Other penalizations: [glmHub()] and [glmOrphan()].
#' Cross-validation with the same penalization: [cv.glmDegree()].
#' @examples
#' xdata <- matrix(rnorm(100), ncol = 5)
#' glmDegree(xdata, rnorm(nrow(xdata)), "correlation",
#'   family = "gaussian",
#'   network.options = networkOptions(min.degree = .2)
#' )
glmDegree <- function(
    xdata,
    ydata,
    network,
    network.options = networkOptions(),
    ...) {
  network.options$trans.fun <- function(x) {
    return(1 / x)
  }
  glmSparseNet(xdata, ydata, network,
    network.options = network.options, ...
  )
}

#' GLMNET cross-validation model penalizing nodes with small degree
#'
#' This function overrides the `trans.fun` options in `network.options` with the
#' inverse of a degree described in Veríssimo et al. (2015) that penalizes
#' nodes with small degree.
#'
#' @inheritParams cv.glmSparseNet
#' @inherit cv.glmSparseNet return
#' @export
#'
#' @seealso Generic function without pre-defined penalization: [cv.glmNetSparse()].
#' Other penalizations: [cv.glmHub()] and [cv.glmOrphan()].
#' Model with the same penalization: [glmDegree()].
#' @examples
#' xdata <- matrix(rnorm(100), ncol = 5)
#' cv.glmDegree(xdata, rnorm(nrow(xdata)), "correlation",
#'   family = "gaussian",
#'   nfolds = 5,
#'   network.options = networkOptions(min.degree = .2)
#' )
cv.glmDegree <- function(
    xdata,
    ydata,
    network,
    network.options = networkOptions(),
    ...) {
  network.options$trans.fun <- function(x) {
    1 / x
  }
  cv.glmSparseNet(xdata, ydata, network,
    network.options = network.options, ...
  )
}
