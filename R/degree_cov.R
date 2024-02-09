#' Calculate the degree of the covariance network based on xdata
#'
#' @param xdata calculate correlation matrix on each column
#' @param cutoff positive value that determines a cutoff value
#' @param consider.unweighted consider all edges as 1 if they are greater than 0
#' @param n.cores number of cores to be used
#' @param force.recalc.degree force recalculation of penalty weights (but not
#' the network), instead of going to cache
#' @param force.recalc.network force recalculation of network and penalty
#' weights, instead of going to cache
#' @param ... extra parameters for cov function
#'
#' @return a vector of the degrees
#' @export
#'
#' @examples
#' n.col <- 6
#' xdata <- matrix(rnorm(n.col * 4), ncol = n.col)
#' degreeCov(xdata)
#' degreeCov(xdata, cutoff = .5)
#' degreeCov(xdata, cutoff = .5, consider.unweighted = TRUE)
degreeCov <- function(xdata, cutoff = 0, consider.unweighted = FALSE,
                      force.recalc.degree = FALSE, force.recalc.network = FALSE,
                      n.cores = 1, ...) {
  return(.degreeGeneric(stats::cov, "correlation", xdata,
                        cutoff = cutoff,
                        consider.unweighted = consider.unweighted,
                        force.recalc.degree = force.recalc.degree,
                        force.recalc.network = force.recalc.network,
                        n.cores = n.cores, ...
  ))
}
