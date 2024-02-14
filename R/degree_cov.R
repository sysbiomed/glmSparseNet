#' Calculate the degree of the covariance network based on xdata
#'
#' @inheritParams degreeCor
#' @param ... extra parameters for cov function.
#'
#' @return a vector of the degrees
#' @export
#'
#' @examples
#' n.col <- 6
#' xdata <- matrix(rnorm(n.col * 4), ncol = n.col)
#' degreeCov(xdata)
#' degreeCov(xdata, cutoff = .5)
#' degreeCov(xdata, cutoff = .5, considerUnweighted = TRUE)
degreeCov <- function(
    xdata,
    cutoff = 0,
    considerUnweighted = FALSE,
    forceRecalcDegree = FALSE,
    forceRecalcNetwork = FALSE,
    nCores = 1,
    ...,
    # Deprecated arguments with dots in name
    consider.unweighted = deprecated(), # nolint: object_name_linter.
    force.recalc.degree = deprecated(), # nolint: object_name_linter.
    force.recalc.network = deprecated(), # nolint: object_name_linter.
    n.cores = deprecated()) { # nolint: object_name_linter.
    # Lifecycle management: to remove after 1.23.0
    if (lifecycle::is_present(consider.unweighted)) {
        .deprecatedDotParam("separate2GroupsCox", "consider.unweighted")
        considerUnweighted <- consider.unweighted
    }
    if (lifecycle::is_present(force.recalc.degree)) {
        .deprecatedDotParam("separate2GroupsCox", "force.recalc.degree")
        forceRecalcDegree <- force.recalc.degree
    }
    if (lifecycle::is_present(force.recalc.network)) {
        .deprecatedDotParam("separate2GroupsCox", "force.recalc.network")
        forceRecalcNetwork <- force.recalc.network
    }
    if (lifecycle::is_present(n.cores)) {
        .deprecatedDotParam("separate2GroupsCox", "n.cores")
        nCores <- n.cores
    }
    # Lifecycle management: end

    checkmate::assert_matrix(xdata)
    checkmate::assert_double(cutoff, len = 1)
    checkmate::assert_flag(considerUnweighted)
    checkmate::assert_flag(forceRecalcDegree)
    checkmate::assert_flag(forceRecalcNetwork)
    checkmate::assert_integerish(nCores, lower = 1)

    .degreeGeneric(
        stats::cov, "correlation",
        xdata,
        cutoff = cutoff,
        considerUnweighted = considerUnweighted,
        forceRecalcDegree = forceRecalcDegree,
        forceRecalcNetwork = forceRecalcNetwork,
        nCores = nCores,
        ...
    )
}
