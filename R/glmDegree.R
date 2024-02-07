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

#' @describeIn cv.glmSparseNet penalizes nodes with small degree
#' _(inversion penalization `h(x) = 1 / x`)_.
#'
#' @export
#'
#' @examples
#' # Degree penalization
#'
#' xdata <- matrix(rnorm(100), ncol = 5)
#' cv.glmDegree(
#'   xdata,
#'   rnorm(nrow(xdata)),
#'   "correlation",
#'   family = "gaussian",
#'   nfolds = 5,
#'   options = networkOptions(min.degree = .2)
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
