#' @describeIn glmSparseNet Penalizes nodes with small degree
#' _(normalized heuristic that promotes nodes with many edges)_.
#'
#' @export
#'
#' @examples
#' xdata <- matrix(rnorm(100), ncol = 5)
#' glmHub(
#'   xdata,
#'   rnorm(nrow(xdata)),
#'   "correlation",
#'   family = "gaussian",
#'   options = networkOptions(min.degree = .2)
#' )
glmHub <- function(
    xdata,
    ydata,
    network,
    options = networkOptions(),
    experiment = NULL,
    network.options = deprecated(), # nolint: object_name_linter.
    experiment.name = deprecated(), # nolint: object_name_linter.
    ...) {
  # Lifecycle management: to remove after 1.23.0
  if (lifecycle::is_present(network.options)) {
    .deprecatedDotParam("cv.glmSparseNet", "network.options")
    options <- network.options
  }
  if (lifecycle::is_present(experiment.name)) {
    .deprecatedDotParam("cv.glmSparseNet", "experiment")
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

#' @describeIn cv.glmSparseNet penalizes nodes with small degree
#' _(normalized heuristic that promotes nodes with many edges)_.
#'
#' @export
#'
#' @examples
#' # Hub penalization
#'
#' xdata <- matrix(rnorm(100), ncol = 5)
#' cv.glmHub(
#'   xdata,
#'   rnorm(nrow(xdata)),
#'   "correlation",
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
    network.options = deprecated(), # nolint: object_name_linter.
    experiment.name = deprecated(), # nolint: object_name_linter.
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
