#' Calculates the covariance network
#'
#' @inheritParams networkCorParallel
#'
#' @return depends on build.output parameter
#' @export
#'
#' @examples
#' n.col <- 6
#' xdata <- matrix(rnorm(n.col * 4), ncol = n.col)
#' networkCovParallel(xdata)
networkCovParallel <- function(
    xdata,
    buildOutput = "matrix",
    nCores = 1,
    forceRecalcNetwork = FALSE,
    showMessage = FALSE,
    ...,
    build.output = deprecated(),
    n.cores = deprecated(),
    force.recalc.network = deprecated(),
    show.message = deprecated()) {
  # Lifecycle management: to remove after 1.23.0
  if (lifecycle::is_present(build.output)) {
    .deprecatedDotParam("buildLambda", "build.output")
    buildOutput <- build.output
  }
  if (lifecycle::is_present(n.cores)) {
    .deprecatedDotParam("buildLambda", "n.cores")
    nCores <- n.cores
  }
  if (lifecycle::is_present(force.recalc.network)) {
    .deprecatedDotParam("buildLambda", "force.recalc.network")
    forceRecalcNetwork <- force.recalc.network
  }
  if (lifecycle::is_present(show.message)) {
    .deprecatedDotParam("buildLambda", "show.message")
    showMessage <- show.message
  }
  # Lifecycle management: end

  .networkGenericParallel(
    stats::cov,
    "covariance",
    xdata,
    buildOutput = buildOutput,
    nCores = nCores,
    forceRecalcNetwork = forceRecalcNetwork,
    showMessage = showMessage,
    ...
  )
}
