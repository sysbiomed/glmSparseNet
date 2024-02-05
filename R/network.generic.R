#' Calculate GLM model with network-based regularization
#'
#' @param fun function to be called (glmnet or cv.glmnet)
#' @param xdata input data, can be a matrix or MultiAssayExperiment
#' @param ydata response data compatible with glmnet
#' @param network type of network, see below
#' @param experiment.name when xdata is a MultiAssayExperiment object this
#' parameter is required
#' @param network.options options to calculate network
#' @param ... parameters that glmnet accepts
#'
#' @return an object just as glmnet
#' network parameter accepts:
#'
#'  * string to calculate network based on data (correlation, covariance)
#'  * matrix representing the network
#'  * vector with already calculated penalty weights (can also be used directly
#'  with glmnet)
#'
.glmSparseNetPrivate <- function(fun, xdata, ydata, network,
                                 experiment.name = NULL,
                                 network.options = networkOptions(),
                                 ...) {
  checkmate::assert_multi_class(
    xdata, c("MultiAssayExperiment", "SummarizedExperiment", "matrix", "Matrix")
  )
  checkmate::assert_multi_class(
    ydata, c("matrix", "data.frame", "Matrix", "Data.Frame")
  )

  #
  # Sequential check of xdata argument
  #   1. Checks if xdata is MultiAssayExperiment, if it is then transforms
  #      xdata in SummarizedExperiment
  #   2. Checks if xdata is SummarizedExperiment and extract data to Matrix
  #      form
  #   3. Checks if xdata is matrix, if it is transform to Matrix
  #  note: this needs to be sequential instead of if () else (), as one
  #   transformation will pipe to another
  xdata <- normalize_xdata(xdata, experiment.name)
  ydata <- normalize_ydata(ydata)

  penalty.factor <- if (is.character(network)) {
    .calcPenalty(xdata, network, network.options)
  } else if (is.matrix(network) || inherits(network, "Matrix")) {
    (Matrix::colSums(network) + Matrix::rowSums(network)) |>
      network.options$trans.fun()
  } else if (is.vector(network)) {
    length(network) != ncol(xdata) &&
      stop("Network vector size does not match xdata input")
    network |>
      network.options$trans.fun()
  } else {
    stop("There was an error with network argumnent")
  }

  penalty_factor <- penalty_factor + network.options$min.degree

  if (all(penalty.factor <= 0)) {
    warning(
      "The `penalty.factor` calculated from network (or given) has ",
      "all 0 values, this might lead to convergence problems. Try",
      " changing some of the network options."
    )
    # penalty.factor <- rep(1, length(penalty.factor)) # nolint: commented_code_linter
  } else if (any(penalty.factor == 0)) {
    warning(
      "The `penalty.factor` calculated from network (or given) has ",
      "some 0 values, this might lead to convergence problems. Try ",
      "using min.degree in network.options to tweak a minimum value."
    )
  }

  obj <- fun(xdata, ydata, penalty.factor = penalty.factor, ...)
  obj$penalty.factor <- penalty.factor

  obj
}

#' @keywords internal
normalize_ydata <- function(ydata) {
  if (inherits(xdata, "MultiAssayExperiment")) {
    # if ydata has rownames then it uses it to match with valid experiences
    #  this is done to avoid missorted objects
    if (
      is.matrix(ydata) || is.data.frame(ydata) || inherits(ydata, "DataFrame")
    ) {
      if (!is.null(rownames(ydata))) {
        ydata <- as.matrix(ydata[rownames(xdata@colData), ])
      }
    } else if (is.array(ydata) && !is.null(names(ydata))) {
      ydata <- ydata[rownames(xdata@colData)]
    }
  }
  ydata
}

#' @keywords internal
normalize_xdata <- function(xdata, experiment.name) {
  if (inherits(xdata, "MultiAssayExperiment")) {
    experiment.name %||%
      stop("`experiment.name` argument must be passed, see documentation.")

    # filter the MultiAssayExperiment keeping only individuals with data in
    #  specific experiment
    xdata <- as(xdata[, , experiment.name], "MatchedAssayExperiment")

    # stop if output xdata has no rows (should not happen)
    nrow(xdata@colData) == 0L &&
      stop(
        "Experiment has no observations or the MultiAssayExperiment object",
        " is corrupt."
      )

    xdata <- xdata[[experiment.name]]
  }

  if (inherits(xdata, "SummarizedExperiment")) {
    xdata <- t(SummarizedExperiment::assay(xdata))
  }

  if (inherits(xdata, "Matrix")) {
    xdata <- Matrix::as.matrix(xdata)
  }
  xdata
}
