#' Calculates the correlation network
#'
#' @param xdata base data to calculate network
#' @param buildOutput if output returns a 'matrix', 'vector' of the upper triu
#' without the diagonal or NULL with any other argument
#' @param nCores number of cores to be used
#' @param forceRecalcNetwork force recalculation, instead of going to cache
#' @param showMessage shows cache operation messages
#' @param ... extra parameters for fun
#' @param build.output lifecycle::badge("deprecated")
#' without the diagonal or NULL with any other argument
#' @param n.cores lifecycle::badge("deprecated")
#' @param force.recalc.network lifecycle::badge("deprecated")
#' @param show.message lifecycle::badge("deprecated")
#'
#' @return depends on build.output parameter
#' @export
#'
#' @examples
#' n_col <- 6
#' xdata <- matrix(rnorm(n_col * 4), ncol = n_col)
#' networkCorParallel(xdata)
networkCorParallel <- function(xdata,
                               buildOutput = "matrix",
                               nCores = 1,
                               forceRecalcNetwork = FALSE,
                               showMessage = FALSE,
                               ...,
                               # Deprecated arguments with dots in name
                               # nolint start: object_name_linter.
                               build.output = deprecated(),
                               n.cores = deprecated(),
                               force.recalc.network = deprecated(),
                               show.message = deprecated()) {
    # nolint end: object_name_linter.
    # Lifecycle management: to remove after 1.23.0
    if (lifecycle::is_present(build.output)) {
        .deprecatedDotParam("networkCorParallel", "build.output")
        buildOutput <- build.output
    }
    if (lifecycle::is_present(n.cores)) {
        .deprecatedDotParam("networkCorParallel", "n.cores")
        nCores <- n.cores
    }
    if (lifecycle::is_present(force.recalc.network)) {
        .deprecatedDotParam("networkCorParallel", "force.recalc.network")
        forceRecalcNetwork <- force.recalc.network
    }
    if (lifecycle::is_present(show.message)) {
        .deprecatedDotParam("networkCorParallel", "show.message")
        showMessage <- show.message
    }
    # Lifecycle management: end

    .networkGenericParallel(
        stats::cor,
        "correlation",
        xdata,
        buildOutput = buildOutput,
        nCores = nCores,
        forceRecalcNetwork = forceRecalcNetwork,
        showMessage = showMessage,
        ...
    )
}
