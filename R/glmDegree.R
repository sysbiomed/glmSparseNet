#' @describeIn glmSparseNet penalizes nodes with small degree
#' _(inversion penalization `h(x) = 1 / x`)_.
#'
#' @export
#'
#' @examples
#' # Degree penalization
#'
#' xdata <- matrix(rnorm(100), ncol = 5)
#' glmDegree(
#'   xdata,
#'   rnorm(nrow(xdata)),
#'   "correlation",
#'   family = "gaussian",
#'   options = networkOptions(min.degree = .2)
#' )
glmDegree <- function(
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

  options$trans.fun <- function(x) 1 / x
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
#' This function overrides the `trans.fun` options in `networkOptions` with the
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
#' cv.glmDegree(
#'   xdata,
#'   rnorm(nrow(xdata)),
#'   "correlation",
#'   family = "gaussian",
#'   nfolds = 5,
#'   network_options = networkOptions(min.degree = .2)
#' )
cv.glmDegree <- function(
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
  if (lifecycle::is_present(experiment)) {
    .deprecatedDotParam("cv.glmSparseNet", "experiment.name")
    experiment <- experiment.name
  }
  # Lifecycle management: end

  options$trans.fun <- function(x) 1 / x
  cv.glmSparseNet(
    xdata,
    ydata,
    network,
    options = options,
    experiment = experiment,
    ...
  )
}
