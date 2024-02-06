#' @describeIn glmSparseNet Penalizes nodes with small degree
#'
#' @export
#'
#' @seealso Generic function without pre-defined penalization: [glmNetSparse()].
#' Other penalizations: [glmDegree()] and [glmOrphan()].
#' Cross-validation with the same penalization: [cv.glmHub()].
#' @examples
#' xdata <- matrix(rnorm(100), ncol = 5)
#' glmHub(xdata, rnorm(nrow(xdata)), "correlation",
#'   family = "gaussian",
#'   options = networkOptions(min.degree = .2)
#' )
glmHub <- function(
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
    .deprecated_dot_param("cv.glmSparseNet", "network.options")
    options <- network.options
  }
  if (lifecycle::is_present(experiment.name)) {
    .deprecated_dot_param("cv.glmSparseNet", "experiment.name")
    experiment <- experiment.name
  }
  # Lifecycle management: end

  options$trans.fun <- hubHeuristic
  glmSparseNet(
    xdata,
    ydata,
    network,
    options = options,
    experiment = experiment,
    ...
  )
}

#' GLMNET cross-validation model penalizing nodes with small degree
#'
#' This function overrides the `trans.fun` options in `options` with
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
#'   options = networkOptions(min.degree = .2)
#' )
cv.glmHub <- function(
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
    .deprecated_dot_param("cv.glmSparseNet", "network.options")
    options <- network.options
  }
  if (lifecycle::is_present(experiment.name)) {
    .deprecated_dot_param("cv.glmSparseNet", "experiment.name")
    experiment <- experiment.name
  }
  # Lifecycle management: end

  options$trans.fun <- hubHeuristic
  cv.glmSparseNet(
    xdata,
    ydata,
    network,
    options = options,
    experiment = experiment,
    ...
  )
}
