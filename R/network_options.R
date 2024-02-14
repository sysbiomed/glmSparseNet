#' Setup network options
#'
#' Setup network options, such as using weighted or unweighted degree,
#'  which centrality measure to use
#'
#' @param method in case of correlation and covariance, which method to use.
#' @param unweighted calculate degree using unweighted network.
#' @param cutoff cuttoff value in network edges to trim the network.
#' @param centrality centrality measure to use, currently only supports degree.
#' @param minDegree minimum value that individual penalty weight can take.
#' @param nCores number of cores to use, default to 1.
#' @param transFun See details below.
#' @param min.degree `r lifecycle::badge("deprecated")`
#' @param n.cores `r lifecycle::badge("deprecated")`
#' @param trans.fun `r lifecycle::badge("deprecated")`
#'
#' The `transFun` argument takes a function definition that will apply a
#' transformation to the penalty vector calculated from the degree. This
#' transformation allows to change how the penalty is applied.
#'
#' @seealso [glmOrphan()] and [glmDegree()]
#'
#' @return a list of options
#' @export
#'
#' @examples
#' networkOptions(unweighted = FALSE)
networkOptions <- function(method = "pearson",
                           unweighted = TRUE,
                           cutoff = 0,
                           centrality = "degree",
                           minDegree = 0,
                           nCores = 1,
                           transFun = function(x) x,
                           # Deprecated arguments with dots in name
                           min.degree = deprecated(), # nolint: object_name_linter.
                           n.cores = deprecated(), # nolint: object_name_linter.
                           trans.fun = deprecated()) { # nolint: object_name_linter.
    # Lifecycle management: to remove after 1.23.0
    if (lifecycle::is_present(min.degree)) {
        .deprecatedDotParam("networkOptions", "min.degree")
        minDegree <- min.degree
    }
    if (lifecycle::is_present(n.cores)) {
        .deprecatedDotParam("networkOptions", "n.cores")
        nCores <- n.cores
    }
    if (lifecycle::is_present(trans.fun)) {
        .deprecatedDotParam("networkOptions", "trans.fun")
        transFun <- trans.fun
    }
    # Lifecycle management: end

    list(
        method = method,
        unweighted = unweighted,
        cutoff = cutoff,
        centrality = centrality,
        nCores = nCores,
        minDegree = minDegree,
        transFun = transFun
    )
}

#' Calculate penalty based on data
#'
#' Internal method to calculate the network using data-dependant methods
#'
#' @param xdata input data
#' @param penaltyType which method to use
#' @param options options to be used
#'
#' @return vector with penalty weights
#'
#' @examples
#' xdata <- matrix(rnorm(1000), ncol = 200)
#' glmSparseNet:::.calcPenalty(xdata, "none")
#' glmSparseNet:::.calcPenalty(
#'     xdata, "correlation",
#'     networkOptions(cutoff = .6)
#' )
#' glmSparseNet:::.calcPenalty(xdata, "correlation")
#' glmSparseNet:::.calcPenalty(
#'     xdata, "covariance",
#'     networkOptions(cutoff = .6)
#' )
#' glmSparseNet:::.calcPenalty(xdata, "covariance")
.calcPenalty <- function(xdata, penaltyType, options = networkOptions()) {
    if (options$centrality == "degree") {
        degreeArgs <- list(
            xdata = xdata,
            method = options$method,
            considerUnweighted = options$unweighted,
            cutoff = options$cutoff,
            nCores = options$nCores
        )
        penaltyFactor <- switch(penaltyType,
            correlation = do.call(degreeCor, degreeArgs),
            covariance = do.call(degreeCov, degreeArgs),
            none = rep(1L, ncol(xdata)),
            rlang::abort(
                "Unkown network type, see documentation of glmSparseNet"
            )
        )
    } else {
        rlang::abort(
            sprintf("Centrality method not recognised: %s", options$centrality)
        )
    }
    options$transFun(penaltyFactor)
}

#' Heuristic function to penalize nodes with low degree
#'
#' @param x single value of vector
#'
#' @return transformed
#' @export
#'
#' @examples
#' hubHeuristic(rnorm(1:10))
hubHeuristic <- function(x) {
    x <- x / max(x)
    heuristicScale(1 - x)
}

#' Heuristic function to penalize nodes with high degree
#'
#' @param x single value of vector
#'
#' @return transformed
#' @export
#'
#' @examples
#' orphanHeuristic(rnorm(1:10))
orphanHeuristic <- function(x) {
    x <- x / max(x)
    heuristicScale(x)
}

#' Heuristic function to use in high dimensions
#'
#' @param x vector of values to scale
#' @param subExp10 value to subtract to base 10 exponential, for example:
#' `10^0 - subExp10 = 1 - subExp10`
#' @param expMult parameter to multiply exponential, i.e. to have a negative
#' exponential or positive
#' @param subExp value to subtract for exponentional, for example if x = 0,
#' `exp(0) - sub.exp = 1 - sub.exp`
#' @param sub.exp10 `r lifecycle::badge("deprecated")`
#' @param exp.mult `r lifecycle::badge("deprecated")`
#' @param sub.exp `r lifecycle::badge("deprecated")`
#'
#' @return a vector of scaled values
#' @export
#'
#' @examples
#' heuristicScale(rnorm(1:10))
heuristicScale <- function(x,
                           subExp10 = -1,
                           expMult = -1,
                           subExp = -1,
                           # Deprecated arguments with dots in name
                           sub.exp10 = deprecated(), # nolint: object_name_linter.
                           exp.mult = deprecated(), # nolint: object_name_linter.
                           sub.exp = deprecated()) { # nolint: object_name_linter.
    # Lifecycle management: to remove after 1.23.0
    if (lifecycle::is_present(sub.exp10)) {
        .deprecatedDotParam("heuristicScale", "sub.exp10")
        subExp10 <- sub.exp10
    }
    if (lifecycle::is_present(exp.mult)) {
        .deprecatedDotParam("heuristicScale", "exp.mult")
        expMult <- exp.mult
    }
    if (lifecycle::is_present(sub.exp)) {
        .deprecatedDotParam("heuristicScale", "sub.exp")
        subExp <- sub.exp
    }
    # Lifecycle management: end

    subExp10 + 10^(-expMult * (exp(x) + subExp))
}
