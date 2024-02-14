#' Calculate the degree of the correlation network based on xdata
#'
#' @param xdata calculate correlation matrix on each column.
#' @param cutoff positive value that determines a cutoff value.
#' @param considerUnweighted consider all edges as 1 if they are greater than
#' 0.
#' @param forceRecalcDegree force recalculation of penalty weights (but not
#' the network), instead of going to cache.
#' @param forceRecalcNetwork force recalculation of network and penalty
#' weights, instead of going to cache.
#' @param nCores number of cores to be used.
#' @param ... extra parameters for cor function.
#' @param consider.unweighted `r lifecycle::badge("deprecated")`
#' @param force.recalc.degree `r lifecycle::badge("deprecated")`
#' @param force.recalc.network `r lifecycle::badge("deprecated")`
#' @param n.cores `r lifecycle::badge("deprecated")`
#'
#' @return a vector of the degrees.
#' @export
#'
#' @examples
#' n.col <- 6
#' xdata <- matrix(rnorm(n.col * 4), ncol = n.col)
#' degreeCor(xdata)
#' degreeCor(xdata, cutoff = .5)
#' degreeCor(xdata, cutoff = .5, considerUnweighted = TRUE)
degreeCor <- function(xdata,
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
        stats::cor,
        "correlation",
        xdata,
        cutoff = cutoff,
        considerUnweighted = considerUnweighted,
        forceRecalcDegree = forceRecalcDegree,
        forceRecalcNetwork = forceRecalcNetwork,
        nCores = nCores,
        ...
    )
}
