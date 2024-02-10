#' Calculate the upper triu of the matrix
#'
#' @param fun function that will calculate the edge weight between 2 nodes
#' @param funPrefix used to store low-level information on network as it can
#' become to large to be stored in memory
#' @inheritParams networkCorParallel
#'
#' @return depends on buildOutput parameter
.networkGenericParallel <- function(
    fun,
    funPrefix,
    xdata,
    buildOutput = "matrix",
    nCores = 1,
    forceRecalcNetwork = FALSE,
    showMessage = FALSE,
    ...) {
  # Windows only support 1 core
  if (.Platform$OS.type == "windows") {
    nCores <- 1
  }

  #
  xdataSha256 <- .digestCache(xdata)
  #
  fun_aux <- function(xdata, ...) {
    result <- parallel::mclapply(as.numeric(seq_len(ncol(xdata) - 1)),
      function(ix.i) {
        tryCatch(
          {
            result <- .runCache(
              .networkWorker,
              fun,
              xdata,
              ix.i,
              # run_cache arguments
              cache_digest = list(xdataSha256),
              cache_prefix = funPrefix,
              show_message = showMessage,
              force_recalc = forceRecalcNetwork,
              ...
            )
          },
          error = function(error.str) {
            futile.logger::flog.error(
              "This error has occured %s",
              error.str
            )
          }
        )
        if (buildOutput == "vector" || buildOutput == "matrix") {
          return(result)
        } else {
          return(TRUE)
        }
      },
      mc.cores = nCores, mc.silent = FALSE, mc.preschedule = TRUE
    )
    return(result)
  }

  result <- .runCache(
    fun_aux,
    xdata,
    # run_cache arguments
    cache_prefix = "fun_aux",
    cache_digest = list(xdataSha256),
    force_recalc = forceRecalcNetwork,
    show_message = showMessage,
    ...
  )
  if (buildOutput == "vector") {
    return(unlist(result))
  } else if (buildOutput == "matrix") {
    sparse.data <- data.frame(i = c(), j = c(), p = c())
    for (ix in rev(seq_along(result))) {
      line <- result[[ix]]
      sparse.data <- rbind(
        sparse.data,
        data.frame(
          i = array(ix, length(line)),
          j = ix + seq_along(line),
          p = as.vector(line)
        )
      )
      result[[ix]] <- NULL
    }
    return(Matrix::sparseMatrix(
      i = sparse.data$i, j = sparse.data$j,
      x = sparse.data$p, dims = c(
        ncol(xdata),
        ncol(xdata)
      ),
      symmetric = TRUE
    ))
  } else {
    return(NULL)
  }
}

#' Worker to calculate edge weight for each pair of ix.i node and following
#'
#' Note that it assumes it does not calculate for index below and equal to ix.i
#'
#' @param fun function to be used, can be cor, cov or any other defined function
#' @param xdata original data to calculate the function over
#' @param ix.i starting index, this can be used to save ony upper triu
#' @param ... extra parameters for fun
#'
#' @return a vector with size `ncol(xdata) - ix.i`
.networkWorker <- function(fun, xdata, ix.i, ...) {
  result <- fun(
    as.vector(xdata[, ix.i]),
    base::as.matrix(xdata[, seq(ix.i + 1, ncol(xdata))]), ...
  )
  result[is.na(result)] <- 0
  return(result)
}

#' Generic function to calculate degree based on data
#'
#' The assumption to use this function is that the network represented by a
#' matrix is symetric and without
#' any connection the node and itself.
#'
#' @param fun function that will calculate the edge weight between 2 nodes
#' @param funPrefix used to store low-level information on network as it can
#' become to large to be stored in memory
#' @param xdata calculate correlation matrix on each column
#' @param cutoff positive value that determines a cutoff value
#' @param considerUnweighted consider all edges as 1 if they are greater than 0
#' @param chunks calculate function at batches of this value (default is 1000)
#' @param nCores number of cores to be used
#' @param forceRecalcDegree force recalculation of penalty weights (but not
#' the network), instead of going to cache
#' @param forceRecalcNetwork force recalculation of network and penalty
#' weights, instead of going to cache
#' @param ... extra parameters for fun
#'
#' @return a vector of the degrees
.degreeGeneric <- function(fun = stats::cor, funPrefix = "operator", xdata,
                           cutoff = 0, considerUnweighted = FALSE,
                           chunks = 1000, forceRecalcDegree = FALSE,
                           forceRecalcNetwork = FALSE,
                           nCores = 1, ...) {
  # fail safe until windows has parallel computing support for mclapply
  if (.Platform$OS.type == "windows") {
    nCores <- 1
  }

  if (forceRecalcNetwork) {
    forceRecalcDegree <- forceRecalcNetwork
  }

  if (inherits(xdata, "matrix")) {
    xdata <- Matrix::Matrix(xdata)
  }

  if (!inherits(xdata, "Matrix")) {
    stop("xdata argument must be a matrix object")
  }

  chunkFunction <- function(xdata, max.ix, ix.outer, nCores, cutoff,
                             considerUnweighted, ...) {
    parallel::mclapply(seq(ix.outer, max.ix, 1),
      function(ix.i) {
        line <- .networkWorker(fun, xdata, ix.i, ...)
        #
        line[is.na(line)] <- 0 # failsafe (for example, when sd = 0)
        line <- abs(line)
        line[line < cutoff] <- 0
        if (considerUnweighted) {
          line[line != 0] <- 1
        }
        line <- c(rep(0, ix.i - 1), sum(line), line)
        return(line)
      },
      mc.cores = nCores, mc.allow.recursive = FALSE
    )
  }

  #
  # auxiliary function to be able to call with cache
  #
  weigthedAux <- function(xdata, cutoff, considerUnweighted, ...) {
    degree <- array(0, ncol(xdata))
    for (ix.outer in seq(1, ncol(xdata) - 1, chunks)) {
      max.ix <- min(ix.outer + chunks - 1, ncol(xdata) - 1)
      res.chunks <- .runCache(
        chunkFunction,
        xdata,
        max.ix,
        ix.outer,
        nCores,
        cutoff,
        considerUnweighted,
        ...,
        # run_cache arguments
        cache_digest = list(xdataSha256),
        cache_prefix = funPrefix,
        show_message = FALSE,
        force_recalc = forceRecalcNetwork
      )
      #
      res.chunks <- matrix(unlist(res.chunks),
        ncol = ncol(xdata), byrow = TRUE
      )
      degree <- degree + colSums(res.chunks)
    }
    names(degree) <- colnames(xdata)
    degree
  }
  #
  xdataSha256 <- .digestCache(xdata)

  val <- .runCache(
    weigthedAux,
    xdata,
    cutoff,
    considerUnweighted,
    # run_cache arguments
    cache_digest = list(xdataSha256),
    cache_prefix = sprintf("degree.%s", funPrefix),
    show_message = FALSE,
    force_recalc = forceRecalcDegree, ...
  )
  return(val)
}
