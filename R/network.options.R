
#' Setup network options
#'
#' Setup network options, such as using weighted or unweighted degree,
#'  which centrality measure to use
#'
#' @param trans.fun see below
#' @param min.degree minimum value that individual penalty weight can take
#' @param method in case of correlation and covariance, which method to use
#' @param unweighted calculate degree using unweighted network
#' @param cutoff cuttoff value in network edges to trim the network
#' @param centrality centrality measure to use, currently only supports degree
#' @param n.cores number of cores to use, default to 1
#'
#' The trans.fun argument takes a function definition that will apply a
#' transformation to the penalty vector calculated from the degree. This
#' transformation allows to change how the penalty is applied.
#'
#' @seealso glmOrphan glmDegree
#'
#' @return a list of options
#' @export
#'
#' @examples
#' networkOptions(unweighted = FALSE)
networkOptions <- function(method     = 'pearson',
                           unweighted = TRUE,
                           cutoff     = 0,
                           centrality = 'degree',
                           min.degree = 0,
                           n.cores    = 1,
                           trans.fun  = function(x) { x }) {
    return(list(method = method,
                unweighted = unweighted,
                cutoff = cutoff,
                centrality = centrality,
                n.cores = n.cores,
                min.degree = min.degree,
                trans.fun = trans.fun))
}


#' Calculate penalty based on data
#'
#' Internal method to calculate the network using data-dependant methods
#'
#' @param xdata input data
#' @param penalty.type which method to use
#' @param network.options options to be used
#'
#' @return vector with penalty weights
#'
#' @examples
#' xdata <- matrix(rnorm(1000), ncol = 200)
#' glmSparseNet:::.calcPenalty(xdata, 'none')
#' glmSparseNet:::.calcPenalty(xdata, 'correlation',
#'                             networkOptions(cutoff = .6))
#' glmSparseNet:::.calcPenalty(xdata, 'correlation')
#' glmSparseNet:::.calcPenalty(xdata, 'covariance',
#'                             networkOptions(cutoff = .6))
#' glmSparseNet:::.calcPenalty(xdata, 'covariance')
#' glmSparseNet:::.calcPenalty(xdata, 'sparsebn')
.calcPenalty <- function(xdata, penalty.type,
                        network.options = networkOptions()) {
    if (network.options$centrality == 'degree') {
        penalty.factor <- switch (penalty.type,
            correlation = degreeCor(
                xdata,
                method              = network.options$method,
                consider.unweighted = network.options$unweighted,
                cutoff              = network.options$cutoff,
                #
                n.cores = network.options$n.cores),
            covariance = degreeCov(
                xdata,
                method              = network.options$method,
                consider.unweighted = network.options$unweighted,
                cutoff              = network.options$cutoff,
                #
                n.cores = network.options$n.cores),
            sparsebn = degreeSparsebn(
                xdata,
                consider.unweighted = network.options$unweighted,
                cutoff              = network.options$cutoff,
                #
                n.cores = network.options$n.cores),
            none = rep(1, ncol(xdata)),
            stop('Unkown network type, see documentation of glmSparseNet')
        )
    } else {
        stop(sprintf('Centrality method not recognised: %d',
                     network.options$centrality))
    }
    return(network.options$trans.fun(penalty.factor))
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
    return(heuristicScale(1 - x))
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
    return(heuristicScale(x))
}

#' Heuristic function to use in high dimensions
#'
#' @param x vector of values to scale
#' @param sub.exp10 value to subtract to base 10 exponential, for example:
#' `10^0 - sub.exp10 = 1 - sub.exp10`
#' @param exp.mult parameter to multiply exponential, i.e. to have a negative
#' exponential or positive
#' @param sub.exp value to subtract for exponentional, for example if x = 0,
#' `exp(0) - sub.exp = 1 - sub.exp`
#'
#' @return a vector of scaled values
#' @export
#'
#' @examples
#' heuristicScale(rnorm(1:10))
heuristicScale <- function(x, sub.exp10 = - 1, exp.mult = -1, sub.exp = -1) {
    return(sub.exp10 + 10^(-exp.mult * (exp(x) + sub.exp)))
}
