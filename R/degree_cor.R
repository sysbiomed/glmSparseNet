#' Calculate the degree of the correlation network based on xdata
#'
#' @param xdata calculate correlation matrix on each column
#' @param cutoff positive value that determines a cutoff value
#' @param consider.unweighted consider all edges as 1 if they are greater than 0
#' @param n.cores number of cores to be used
#' @param force.recalc.degree force recalculation of penalty weights (but not
#' the network), instead of going to cache
#' @param force.recalc.network force recalculation of network and penalty
#' weights, instead of going to cache
#' @param ... extra parameters for cor function
#'
#' @return a vector of the degrees
#' @export
#'
#' @examples
#' n.col <- 6
#' xdata <- matrix(rnorm(n.col * 4), ncol = n.col)
#' degreeCor(xdata)
#' degreeCor(xdata, cutoff = .5)
#' degreeCor(xdata, cutoff = .5, consider.unweighted = TRUE)
degreeCor <- function(
    xdata,
    cutoff = 0,
    consider.unweighted = FALSE,
    force.recalc.degree = FALSE,
    force.recalc.network = FALSE,
    n.cores = 1,
    ...) {
  checkmate::assert_matrix(xdata)
  checkmate::assert_double(cutoff, len = 1)
  checkmate::assert_integerish(n.cores, lower = 1)
  checkmate::assert_flag(consider.unweighted)
  checkmate::assert_flag(force.recalc.degree)
  checkmate::assert_flag(force.recalc.network)

  .degreeGeneric(
    stats::cor,
    "correlation",
    xdata,
    cutoff = cutoff,
    consider.unweighted = consider.unweighted,
    force.recalc.degree = force.recalc.degree,
    force.recalc.network = force.recalc.network,
    n.cores = n.cores,
    ...
  )
}
