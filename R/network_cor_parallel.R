#' Calculates the correlation network
#'
#' @param xdata base data to calculate network
#' @param build.output if output returns a 'matrix', 'vector' of the upper triu
#' without the diagonal or NULL with any other argument
#' @param n.cores number of cores to be used
#' @param force.recalc.network force recalculation, instead of going to cache
#' @param show.message shows cache operation messages
#' @param ... extra parameters for fun
#'
#' @return depends on build.output parameter
#' @export
#'
#' @examples
#' n.col <- 6
#' xdata <- matrix(rnorm(n.col * 4), ncol = n.col)
#' networkCorParallel(xdata)
networkCorParallel <- function(
    xdata,
    build.output = "matrix",
    n.cores = 1,
    force.recalc.network = FALSE,
    show.message = FALSE,
    ...) {
  .networkGenericParallel(
    stats::cor,
    "correlation",
    xdata,
    build.output = build.output,
    n.cores = n.cores,
    force.recalc.network = force.recalc.network,
    show.message = show.message,
    ...
  )
}
