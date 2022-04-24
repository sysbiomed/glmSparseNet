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
  build.output  = 'matrix',
  n.cores       = 1,
  force.recalc.network  = FALSE,
  show.message  = FALSE,
  ...) {
  
  .networkGenericParallel(
    stats::cor, 
    'correlation', 
    xdata,
    build.output = build.output, 
    n.cores = n.cores,
    force.recalc.network = force.recalc.network,
    show.message = show.message, 
    ...
  )
}

#' Calculates the covariance network
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
#' networkCovParallel(xdata)
networkCovParallel <- function(xdata,
                               build.output  = 'matrix',
                               n.cores       = 1,
                               force.recalc.network  = FALSE,
                               show.message  = FALSE, ...) {
    .networkGenericParallel(stats::cov, 'covariance', xdata,
                            build.output = build.output, n.cores = n.cores,
                            force.recalc.network = force.recalc.network,
                            show.message = show.message, ...)
}

#' Calculate the upper triu of the matrix
#'
#' @param fun function that will calculate the edge weight between 2 nodes
#' @param fun.prefix used to store low-level information on network as it can
#' become to large to be stored in memory
#' @param xdata base data to calculate network
#' @param build.output if output returns a 'matrix', 'vector' of the upper triu
#' without the diagonal or NULL with any other argument
#' @param n.cores number of cores to be used
#' @param force.recalc.network force recalculation, instead of going to cache
#' @param show.message shows cache operation messages
#' @param ... extra parameters for fun
#'
#' @return depends on build.output parameter
.networkGenericParallel <- function(fun, fun.prefix,
                                    xdata,
                                    build.output  = 'matrix',
                                    n.cores       = 1,
                                    force.recalc.network  = FALSE,
                                    show.message  = FALSE, ...) {

    # Windows only support 1 core
    if (.Platform$OS.type == 'windows') {
        n.cores <- 1
    }

    #
    xdata.sha256 <- digest.cache(xdata)
    #
    fun.aux <- function(xdata, ...) {
        result <- parallel::mclapply(as.numeric(seq_len(ncol(xdata)-1)),
                                     function(ix.i) {
            tryCatch({
                result <- run.cache(
                    .networkWorker, fun,
                    xdata, ix.i,
                    #
                    cache.digest = list(xdata.sha256),
                    cache.prefix = fun.prefix,
                    show.message = show.message,
                    force.recalc = force.recalc.network,
                    ...
                )
            },
            error = function(error.str) {
                futile.logger::flog.error('This error has occured %s',
                                          error.str)
            })
            if (build.output == 'vector' || build.output == 'matrix') {
                return(result)
            } else {
                return(TRUE)
            }
        }, mc.cores = n.cores, mc.silent = FALSE, mc.preschedule = TRUE)
        return(result)
    }
    result <- run.cache(fun.aux, xdata,
                                    #
                                    cache.prefix = 'fun.aux',
                                    cache.digest = list(xdata.sha256),
                                    force.recalc = force.recalc.network,
                                    show.message = show.message,
                                    ...)
    if (build.output == 'vector') {
        return(unlist(result))
    } else if(build.output == 'matrix') {
        sparse.data <- data.frame(i = c(), j = c(), p = c())
        for (ix in rev(seq_along(result))) {
            line <- result[[ix]]
            sparse.data <- rbind(sparse.data,
                                 data.frame(i = array(ix, length(line)),
                                            j = ix + seq_along(line),
                                            p = as.vector(line)))
            result[[ix]] <- NULL
        }
        return(Matrix::sparseMatrix(i = sparse.data$i, j = sparse.data$j,
                                    x = sparse.data$p, dims = c(ncol(xdata),
                                                                ncol(xdata)),
                                    symmetric = TRUE))
    } else {
        return(NULL)
    }
}

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
degreeCor <- function(xdata, cutoff = 0, consider.unweighted = FALSE,
                      force.recalc.degree = FALSE, force.recalc.network = FALSE,
                      n.cores = 1, ...) {

    return(.degreeGeneric(stats::cor, 'correlation', xdata, cutoff = cutoff,
                          consider.unweighted = consider.unweighted,
                          force.recalc.degree = force.recalc.degree,
                          force.recalc.network = force.recalc.network,
                          n.cores = n.cores, ...))
}

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

    return(.degreeGeneric(stats::cov, 'correlation', xdata, cutoff = cutoff,
                          consider.unweighted = consider.unweighted,
                          force.recalc.degree = force.recalc.degree,
                          force.recalc.network = force.recalc.network,
                          n.cores = n.cores, ...))
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
    n.col <- ncol(xdata)
    xdata.i <- xdata[,ix.i]
    result  <- fun(as.vector(xdata[,ix.i]),
                   base::as.matrix(xdata[,seq(ix.i+1, ncol(xdata))]), ...)
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
#' @param fun.prefix used to store low-level information on network as it can
#' become to large to be stored in memory
#' @param xdata calculate correlation matrix on each column
#' @param cutoff positive value that determines a cutoff value
#' @param consider.unweighted consider all edges as 1 if they are greater than 0
#' @param chunks calculate function at batches of this value (default is 1000)
#' @param n.cores number of cores to be used
#' @param force.recalc.degree force recalculation of penalty weights (but not
#' the network), instead of going to cache
#' @param force.recalc.network force recalculation of network and penalty
#' weights, instead of going to cache
#' @param ... extra parameters for fun
#'
#' @return a vector of the degrees
.degreeGeneric <- function(fun = stats::cor, fun.prefix = 'operator', xdata,
                          cutoff = 0, consider.unweighted = FALSE,
                          chunks = 1000, force.recalc.degree = FALSE,
                          force.recalc.network = FALSE,
                          n.cores = 1, ...) {

    # fail safe until windows has parallel computing support for mclapply
    if (.Platform$OS.type == 'windows') {
        n.cores <- 1
    }

    if (force.recalc.network) {
        force.recalc.degree <- force.recalc.network
    }

    if (inherits(xdata, 'matrix')) {
        xdata <- Matrix::Matrix(xdata)
    }

    if (!inherits(xdata, 'Matrix')) {
        stop('xdata argument must be a matrix object')
    }

    chunk.function <- function(xdata, max.ix, ix.outer, n.cores, cutoff,
                               consider.unweighted, ...) {
        res.chunks <- parallel::mclapply(seq(ix.outer, max.ix , 1),
                                         function(ix.i) {
            line <- .networkWorker(fun, xdata, ix.i, ...)
            #
            line[is.na(line)]   <- 0 # failsafe (for example, when sd = 0)
            line                <- abs(line)
            line[line < cutoff] <- 0
            if (consider.unweighted) { line[line != 0] <- 1 }
            line <- c(rep(0, ix.i - 1), sum(line), line)
            return(line)
        }, mc.cores = n.cores, mc.allow.recursive = FALSE)
    }

    #
    # auxiliary function to be able to call with cache
    #
    weigthed.aux <- function(xdata, cutoff, consider.unweighted, ...) {
        degree <- array(0, ncol(xdata))
        for (ix.outer in seq(1, ncol(xdata) - 1, chunks)) {
            max.ix <- min(ix.outer + chunks - 1, ncol(xdata) - 1)
            res.chunks <- run.cache(
                chunk.function, xdata, max.ix,
                ix.outer, n.cores, cutoff,
                consider.unweighted, ...,
                cache.digest = list(xdata.sha256),
                cache.prefix = fun.prefix,
                show.message = FALSE,
                force.recalc = force.recalc.network
            )
            #
            res.chunks <- matrix(unlist(res.chunks),
                                 ncol = ncol(xdata), byrow = TRUE)
            degree   <- degree + colSums(res.chunks)
        }
        names(degree) <- colnames(xdata)
        return(degree)
    }
    #
    xdata.sha256 <- digest.cache(xdata)

    val <- run.cache(
        weigthed.aux, xdata, cutoff, consider.unweighted,
        cache.digest = list(xdata.sha256),
        cache.prefix = sprintf('degree.%s', fun.prefix),
        show.message = FALSE,
        force.recalc = force.recalc.degree, ...
    )
    return(val)
}

