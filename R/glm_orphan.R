#' @describeIn glmSparseNet Penalizes nodes with high degree
#' _(normalized heuristic that promotes nodes with few edges)_.
#'
#' @export
#' @examples
#' # Orphan penalization
#'
#' xdata <- matrix(rnorm(100), ncol = 5)
#' glmOrphan(
#'     xdata,
#'     rnorm(nrow(xdata)),
#'     "correlation",
#'     family = "gaussian",
#'     options = networkOptions(minDegree = .2)
#' )
glmOrphan <- function(
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
        .deprecatedDotParam("glmOrphan", "network.options")
        options <- network.options
    }
    if (lifecycle::is_present(experiment.name)) {
        .deprecatedDotParam("glmOrphan", "experiment.name")
        experiment <- experiment.name
    }
    # Lifecycle management: end

    options$transFun <- orphanHeuristic
    glmSparseNet(
        xdata,
        ydata,
        network,
        options = options,
        experiment = experiment,
        ...
    )
}

#' @describeIn cv.glmSparseNet penalizes nodes with high degree
#' _(normalized heuristic that promotes nodes with few edges)_.
#'
#' @export
#'
#' @examples
#' # Orphan penalization
#'
#' xdata <- matrix(rnorm(100), ncol = 5)
#' cv.glmOrphan(
#'     xdata,
#'     rnorm(nrow(xdata)),
#'     "correlation",
#'     family = "gaussian",
#'     nfolds = 5,
#'     options = networkOptions(minDegree = .2)
#' )
cv.glmOrphan <- function(
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
        .deprecatedDotParam("cv.glmOrphan", "network.options")
        options <- network.options
    }
    if (lifecycle::is_present(experiment.name)) {
        .deprecatedDotParam("cv.glmOrphan", "experiment.name")
        experiment <- experiment.name
    }
    # Lifecycle management: end

    options$transFun <- orphanHeuristic
    cv.glmSparseNet(
        xdata,
        ydata,
        network,
        options = options,
        experiment = experiment,
        ...
    )
}
