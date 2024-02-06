#' Calculate GLM model with network-based regularization
#'
#' @param fun function to be called (glmnet or cv.glmnet)
#' @param xdata input data, can be a matrix or MultiAssayExperiment
#' @param ydata response data compatible with glmnet
#' @param network type of network, see below
#' @param experiment when xdata is a MultiAssayExperiment object this
#' parameter is required
#' @param options options to calculate network
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
#' @keywords internal
.glmSparseNetPrivate <- function(
    fun,
    xdata,
    ydata,
    network,
    experiment = NULL,
    options = networkOptions(),
    ...) {
  checkmate::assert_multi_class(
    xdata, c("MultiAssayExperiment", "SummarizedExperiment", "matrix", "Matrix")
  )
  checkmate::assert_multi_class(
    ydata, c("DataFrame", "data.frame", "matrix", "Matrix", "numeric")
  )

  xdata <- normalize_xdata_mae(xdata, experiment)
  xdata_norm <- normalize_xdata(xdata, experiment)
  ydata_norm <- normalize_ydata(xdata, ydata, experiment)

  penalty_factor <- if (is.character(network)) {
    .calcPenalty(xdata_norm, network, options)
  } else if (is.matrix(network) || inherits(network, "Matrix")) {
    options$trans.fun(Matrix::colSums(network) + Matrix::rowSums(network))
  } else if (is.vector(network)) {
    length(network) != ncol(xdata_norm) &&
      stop("Network vector size does not match xdata input")
    options$trans.fun(network)
  } else {
    stop("There was an error with network argumnent")
  }

  penalty_factor <- penalty_factor + options$min.degree

  if (all(penalty_factor <= 0)) {
    warning(
      "The `penalty.factor` calculated from network (or given) has ",
      "all 0 values, this might lead to convergence problems. Try",
      " changing some of the network options."
    )
    # penalty.factor <- rep(1, length(penalty.factor)) # nolint: commented_code_linter
  } else if (any(penalty_factor == 0)) {
    warning(
      "The `penalty.factor` calculated from network (or given) has ",
      "some 0 values, this might lead to convergence problems. Try ",
      "using min.degree in options to tweak a minimum value."
    )
  }

  obj <- fun(xdata_norm, ydata_norm, penalty.factor = penalty_factor, ...)
  obj$penalty.factor <- penalty_factor

  obj
}

#' @keywords internal
normalize_ydata <- function(xdata, ydata, experiment) {
  if (inherits(xdata, "MultiAssayExperiment")) {
    # if ydata has rownames then it uses it to match with valid experiences
    #  this is done to avoid missorted objects
    if (
      is.matrix(ydata) || is.data.frame(ydata) || inherits(ydata, "DataFrame")
    ) {
      if (!is.null(rownames(ydata))) {
        return(Matrix::as.matrix(ydata[rownames(xdata@colData), ]))
      }
    } else if (is.array(ydata) && !is.null(names(ydata))) {
      return(ydata[rownames(xdata@colData)])
    }
  }
  ydata
}

#' @keywords internal
normalize_xdata_mae <- function(xdata, experiment) {
  if (inherits(xdata, "MultiAssayExperiment")) {
    experiment %||%
      stop("`experiment` argument must be passed, see documentation.")

    # filter the MultiAssayExperiment keeping only individuals with data in
    #  specific experiment
    xdata <- methods::as(xdata[, , experiment], "MatchedAssayExperiment")

    # stop if output xdata has no rows (should not happen)
    nrow(xdata@colData) == 0L &&
      stop(
        "Experiment has no observations or the MultiAssayExperiment object",
        " is corrupt."
      )
  }
  xdata
}

#' Normalize `xdata` argument
#'
#' @description
#'
#' Sequential check of xdata argument
#'   1. Checks if xdata is MultiAssayExperiment, if it is then transforms
#'      xdata in SummarizedExperiment
#'   2. Checks if xdata is SummarizedExperiment and extract data to Matrix
#'      form
#'   3. Checks if xdata is matrix, if it is transform to Matrix
#'  note: this needs to be sequential instead of if () else (), as one
#'   transformation will pipe to another
#' @keywords internal
normalize_xdata <- function(xdata, experiment) {
  if (inherits(xdata, "MultiAssayExperiment")) {
    xdata <- xdata[[experiment]]
  }

  if (inherits(xdata, "SummarizedExperiment")) {
    xdata <- t(SummarizedExperiment::assay(xdata))
    rownames(xdata) <- TCGAutils::TCGAbarcode(rownames(xdata))
  }

  if (inherits(xdata, "Matrix")) {
    xdata <- Matrix::as.matrix(xdata)
  }
  xdata
}
