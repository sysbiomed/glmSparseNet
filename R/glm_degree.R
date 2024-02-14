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
#'     xdata,
#'     rnorm(nrow(xdata)),
#'     "correlation",
#'     family = "gaussian",
#'     options = networkOptions(minDegree = .2)
#' )
glmDegree <- function(xdata,
                      ydata,
                      network,
                      options = networkOptions(),
                      experiment = NULL,
                      # Deprecated arguments with dots in name
                      # nolint start: object_name_linter.
                      network.options = deprecated(),
                      experiment.name = deprecated(),
                      ...) {
    # nolint end: object_name_linter.
    # Lifecycle management: to remove after 1.23.0
    if (lifecycle::is_present(network.options)) {
        .deprecatedDotParam("glmDegree", "network.options", "options")
        options <- network.options
    }
    if (lifecycle::is_present(experiment.name)) {
        .deprecatedDotParam("glmDegree", "experiment.name", "experiment")
        experiment <- experiment.name
    }
    # Lifecycle management: end

    options$transFun <- function(x) 1 / x
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
#'     xdata,
#'     rnorm(nrow(xdata)),
#'     "correlation",
#'     family = "gaussian",
#'     nfolds = 5,
#'     options = networkOptions(minDegree = .2)
#' )
cv.glmDegree <- function(xdata,
                         ydata,
                         network,
                         options = networkOptions(),
                         experiment = NULL,
                         # Deprecated arguments with dots in name
                         # nolint start: object_name_linter.
                         network.options = deprecated(),
                         experiment.name = deprecated(),
                         ...) {
    # nolint end: object_name_linter.
    # Lifecycle management: to remove after 1.23.0
    if (lifecycle::is_present(network.options)) {
        .deprecatedDotParam("cv.glmDegree", "network.options", "options")
        options <- network.options
    }
    if (lifecycle::is_present(experiment.name)) {
        .deprecatedDotParam("cv.glmDegree", "experiment.name", "experiment")
        experiment <- experiment.name
    }
    # Lifecycle management: end

    options$transFun <- function(x) 1 / x
    cv.glmSparseNet(
        xdata,
        ydata,
        network,
        options = options,
        experiment = experiment,
        ...
    )
}
