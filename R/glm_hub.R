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
#'   options = networkOptions(minDegree = .2)
#' )
glmHub <- function(
    xdata,
    ydata,
    network,
    options = networkOptions(),
    experiment = NULL,
    # Deprecated arguments with dots in name
    network.options = deprecated(), # nolint: object_name_linter.
    experiment.name = deprecated(), # nolint: object_name_linter.
    ...) {
  # Lifecycle management: to remove after 1.23.0
  if (lifecycle::is_present(network.options)) {
    .deprecatedDotParam("glmHub", "network.options", "options")
    options <- network.options
  }
  if (lifecycle::is_present(experiment.name)) {
    .deprecatedDotParam("glmHub", "experiment.name", "experiment")
    experiment <- experiment.name
  }
  # Lifecycle management: end

  options$transFun <- hubHeuristic
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
#'   options = networkOptions(minDegree = .2)
#' )
cv.glmHub <- function(
    xdata,
    ydata,
    network,
    options = networkOptions(),
    experiment = NULL,
    # Deprecated arguments with dots in name
    network.options = deprecated(), # nolint: object_name_linter.
    experiment.name = deprecated(), # nolint: object_name_linter.
    ...) {
  # Lifecycle management: to remove after 1.23.0
  if (lifecycle::is_present(network.options)) {
    .deprecatedDotParam("cv.glmHub", "network.options", "options")
    options <- network.options
  }
  if (lifecycle::is_present(experiment.name)) {
    .deprecatedDotParam("cv.glmHub", "experiment.name", "experiment")
    experiment <- experiment.name
  }
  # Lifecycle management: end

  options$transFun <- hubHeuristic
  cv.glmSparseNet(
    xdata,
    ydata,
    network,
    options = options,
    experiment = experiment,
    ...
  )
}
