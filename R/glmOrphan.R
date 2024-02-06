#' @describeIn glmSparseNet Penalizes nodes with high degree
#' _(normalized heuristic that promotes nodes with few edges)_.
#'
#' @export
#' @seealso [orphanHeuristic()]
#' @examples
#' # Orphan penalization
#'
#' xdata <- matrix(rnorm(100), ncol = 5)
#' glmOrphan(
#'   xdata,
#'   rnorm(nrow(xdata)),
#'   "correlation",
#'   family = "gaussian",
#'   options = networkOptions(min.degree = .2)
#' )
glmOrphan <- function(
    xdata,
    ydata,
    network,
    options = networkOptions(),
    experiment = NULL,
    network.options = deprecated(),
    experiment.name = deprecated(),
    ...) {
  # Lifecycle management: to remove after 1.23.0
  if (lifecycle::is_present(network.options)) {
    .deprecatedDotParam("cv.glmSparseNet", "network.options")
    options <- network.options
  }
  if (lifecycle::is_present(experiment.name)) {
    .deprecatedDotParam("cv.glmSparseNet", "experiment.name")
    experiment <- experiment.name
  }
  # Lifecycle management: end

  options$trans.fun <- orphanHeuristic
  glmSparseNet(
    xdata,
    ydata,
    network,
    options = options,
    experiment = experiment,
    ...
  )
}

#' GLMNET cross-validation model penalizing nodes with high degree
#'
#' This function overrides the `trans.fun` options in `options` with
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
#' cv.glmOrphan(
#'   xdata,
#'   rnorm(nrow(xdata)),
#'   "correlation",
#'   family = "gaussian",
#'   nfolds = 5,
#'   options = networkOptions(min.degree = .2)
#' )
cv.glmOrphan <- function(
    xdata,
    ydata,
    network,
    options = networkOptions(),
    experiment = NULL,
    network.options = deprecated(),
    experiment.name = deprecated(),
    ...) {
  # Lifecycle management: to remove after 1.23.0
  if (lifecycle::is_present(network.options)) {
    .deprecatedDotParam("cv.glmSparseNet", "network.options")
    options <- network.options
  }
  if (lifecycle::is_present(experiment.name)) {
    .deprecatedDotParam("cv.glmSparseNet", "experiment.name")
    experiment <- experiment.name
  }
  # Lifecycle management: end

  options$trans.fun <- orphanHeuristic
  cv.glmSparseNet(
    xdata,
    ydata,
    network,
    options = options,
    experiment = experiment,
    ...
  )
}
