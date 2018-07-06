#' Calculates the correlation network
#'
#' @param xdata base data to calculate network
#' @param build.output if output returns a 'matrix', 'vector' of the upper triu without the diagonal or NULL with any other argument
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
#' cor.parallel(xdata)
network.cor.parallel <- function(xdata,
                                 build.output  = 'matrix',
                                 n.cores       = 1,
                                 force.recalc.network  = FALSE,
                                 show.message  = FALSE, ...) {
  network.generic.parallel(stats::cor, 'correlation', xdata, build.output = build.output, n.cores = n.cores,
                           force.recalc.network = force.recalc.network,
                           show.message = show.message, ...)
}

#' Calculates the covariance network
#'
#' @param xdata base data to calculate network
#' @param build.output if output returns a 'matrix', 'vector' of the upper triu without the diagonal or NULL with any other argument
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
#' cov.parallel(xdata)
network.cov.parallel <- function(xdata,
                                 build.output  = 'matrix',
                                 n.cores       = 1,
                                 force.recalc.network  = FALSE,
                                 show.message  = FALSE, ...) {
  network.generic.parallel(stats::cov, 'covariance', xdata, build.output = build.output, n.cores = n.cores,
                           force.recalc.network = force.recalc.network,
                           show.message = show.message, ...)
}

#' Calculate the upper triu of the matrix
#'
#' @param fun function that will calculate the edge weight between 2 nodes
#' @param fun.prefix used to store low-level information on network as it can become to large to be stored in memory
#' @param xdata base data to calculate network
#' @param build.output if output returns a 'matrix', 'vector' of the upper triu without the diagonal or NULL with any other argument
#' @param n.cores number of cores to be used
#' @param force.recalc.network force recalculation, instead of going to cache
#' @param show.message shows cache operation messages
#' @param ... extra parameters for fun
#'
#' @return depends on build.output parameter
network.generic.parallel <- function(fun, fun.prefix,
                                     xdata,
                                     build.output  = 'matrix',
                                     n.cores       = 1,
                                     force.recalc.network  = FALSE,
                                     show.message  = FALSE, ...) {
  #
  xdata.sha256 <- loose.rock::digest.cache(xdata)
  #
  fun.aux <- function(xdata, ...) {
    result <- parallel::mclapply( as.numeric(1:(ncol(xdata)-1)), function(ix.i) {
      tryCatch({
        result <- loose.rock::run.cache(network.worker, fun,
                                       xdata, ix.i,
                                       #
                                       cache.digest = list(xdata.sha256),
                                       cache.prefix = fun.prefix,
                                       show.message = show.message,
                                       force.recalc = force.recalc.network,
                                       ...)
      },
      error = function(error.str) {
        futile.logger::flog.error('This error has occured %s', error.str)
      })
      if (build.output == 'vector' || build.output == 'matrix') {
        return(result)
      } else {
        return(TRUE)
      }
      #}
    }, mc.cores = n.cores, mc.silent = F)
    return(result)
  }
  result <- loose.rock::run.cache(fun.aux, xdata,
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
      sparse.data <- rbind(sparse.data, data.frame(i = array(ix, length(line)), j = ix + seq_along(line), p = line))
      result[[ix]] <- NULL
    }
    return(Matrix::sparseMatrix(i = sparse.data$i, j = sparse.data$j, x = sparse.data$p, dims = c(ncol(xdata), ncol(xdata)), symmetric = TRUE))
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
#' @param force.recalc.degree force recalculation of penalty weights (but not the network), instead of going to cache
#' @param force.recalc.network force recalculation of network and penalty weights, instead of going to cache
#' @param ... extra parameters for cor function
#'
#' @return a vector of the degrees
#' @export
#'
#' @examples
#' n.col <- 6
#' xdata <- matrix(rnorm(n.col * 4), ncol = n.col)
#' degree.cor(xdata)
#' degree.cor(xdata, cutoff = .5)
#' degree.cor(xdata, cutoff = .5, consider.unweighted = TRUE)
setGeneric('degree.cor', function(xdata, cutoff = 0, consider.unweighted = FALSE,
                                  force.recalc.degree = FALSE, force.recalc.network = FALSE,
                                  n.cores = 1, ...) {
  stop('first argument must be a matrix')
})

#' Calculate the degree of the correlation network based on xdata
#'
#' @param xdata calculate correlation matrix on each column
#' @param cutoff positive value that determines a cutoff value
#' @param consider.unweighted consider all edges as 1 if they are greater than 0
#' @param n.cores number of cores to be used
#' @param force.recalc.degree force recalculation of penalty weights (but not the network), instead of going to cache
#' @param force.recalc.network force recalculation of network and penalty weights, instead of going to cache
#' @param ... extra parameters for cor function
#'
#' @return a vector of the degrees
#' @export
setMethod('degree.cor', signature('matrix'), function(xdata, cutoff = 0, consider.unweighted = FALSE,
                                                      force.recalc.degree = FALSE, force.recalc.network = FALSE,
                                                      n.cores = 1, ...) {
  return(degree.generic(stats::cor, 'correlation', xdata, cutoff = cutoff, consider.unweighted = consider.unweighted,
                 force.recalc.degree = force.recalc.degree, force.recalc.network = force.recalc.network,
                 n.cores = n.cores, ...))
})

#' Calculate the degree of the covariance network based on xdata
#'
#' @param xdata calculate correlation matrix on each column
#' @param cutoff positive value that determines a cutoff value
#' @param consider.unweighted consider all edges as 1 if they are greater than 0
#' @param n.cores number of cores to be used
#' @param force.recalc.degree force recalculation of penalty weights (but not the network), instead of going to cache
#' @param force.recalc.network force recalculation of network and penalty weights, instead of going to cache
#' @param ... extra parameters for cov function
#'
#' @return a vector of the degrees
#' @export
#'
#' @examples
#' n.col <- 6
#' xdata <- matrix(rnorm(n.col * 4), ncol = n.col)
#' degree.cov(xdata)
#' degree.cov(xdata, cutoff = .5)
#' degree.cov(xdata, cutoff = .5, consider.unweighted = TRUE)
setGeneric('degree.cov', function(xdata, cutoff = 0, consider.unweighted = FALSE,
                                  force.recalc.degree = FALSE, force.recalc.network = FALSE,
                                  n.cores = 1, ...) {
  stop('first argument must be a matrix')
})

#' Calculate the degree of the covariance network based on xdata
#'
#' @param xdata calculate correlation matrix on each column
#' @param cutoff positive value that determines a cutoff value
#' @param consider.unweighted consider all edges as 1 if they are greater than 0
#' @param n.cores number of cores to be used
#' @param force.recalc.degree force recalculation of penalty weights (but not the network), instead of going to cache
#' @param force.recalc.network force recalculation of network and penalty weights, instead of going to cache
#' @param ... extra parameters for cov function
#'
#' @return a vector of the degrees
#' @export
setMethod('degree.cov', signature('matrix'), function(xdata, cutoff = 0, consider.unweighted = FALSE,
                                                      force.recalc.degree = FALSE, force.recalc.network = FALSE,
                                                      n.cores = 1, ...) {
  return(degree.generic(stats::cor, 'correlation', xdata, cutoff = cutoff, consider.unweighted = consider.unweighted,
                        force.recalc.degree = force.recalc.degree, force.recalc.network = force.recalc.network,
                        n.cores = n.cores, ...))
})



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
#'
#' @examples
#' network.worker(cor, matrix(rnorm(20*10), ncol = 10), 1)
#' network.worker(cor, matrix(rnorm(20*10), ncol = 10), 5)
network.worker <- function(fun, xdata, ix.i, ...) {
  #
  n.col <- ncol(xdata)
  xdata.i <- xdata[,ix.i]
  result  <- sapply((ix.i + 1):n.col, function(ix.j){
    fun(xdata.i, xdata[,ix.j], ...)
  })
  result[is.na(result)] <- 0
  return(result)
}

#' Generic function to calculate degree based on data
#'
#' The assumption to use this function is that the network represented by a matrix is symetric and without
#' any connection the node and itself.
#'
#' @param fun function that will calculate the edge weight between 2 nodes
#' @param fun.prefix used to store low-level information on network as it can become to large to be stored in memory
#' @param xdata calculate correlation matrix on each column
#' @param cutoff positive value that determines a cutoff value
#' @param consider.unweighted consider all edges as 1 if they are greater than 0
#' @param n.cores number of cores to be used
#' @param force.recalc.degree force recalculation of penalty weights (but not the network), instead of going to cache
#' @param force.recalc.network force recalculation of network and penalty weights, instead of going to cache
#' @param ... extra parameters for fun
#'
#' @return a vector of the degrees
#'
#' @examples
#' n.col <- 6
#' xdata <- matrix(rnorm(n.col * 4), ncol = n.col)
#' degree.generic(cor, 'cor', xdata)
degree.generic <- function(fun, fun.prefix = 'operator', xdata, cutoff = 0, consider.unweighted = FALSE,
                           force.recalc.degree = FALSE, force.recalc.network = FALSE,
                           n.cores = 1, ...) {
  if (force.recalc.network) {
    force.recalc.degree <- force.recalc.network
  }
  #
  # auxiliary function to be able to call with cache
  #
  weigthed.aux <- function(xdata, cutoff, consider.unweighted, ...) {
    degree <- array(0, ncol(xdata))
    added.sum <- 1000
    for (ix.outer in seq(1, ncol(xdata) - 1, added.sum)) {
      max.ix <- min(ix.outer + added.sum - 1, ncol(xdata) - 1)
      res.1000 <- parallel::mclapply(seq(ix.outer, max.ix , 1), function(ix.i) {
        line <- loose.rock::run.cache(network.worker, fun, xdata, ix.i,
                                      cache.digest = list(xdata.sha256),
                                      cache.prefix = fun.prefix,
                                      show.message = F,
                                      force.recalc = force.recalc.network,
                                      ...)
        if (any(!is.numeric(line))) {
          line <- loose.rock::run.cache(network.worker, fun, xdata, ix.i,
                                        cache.digest = list(xdata.sha256),
                                        cache.prefix = fun.prefix,
                                        show.message = F,
                                        force.recalc = T,
                                        ...)
        }
        #
        line[is.na(line)]   <- 0 # failsafe in case there was a failure in cov (i.e. sd = 0)
        line                <- abs(line)
        line[line < cutoff] <- 0
        if (consider.unweighted) { line[line != 0] <- 1 }
        line <- c(rep(0, ix.i - 1), sum(line), line)
        return(line)
      }, mc.cores = n.cores, mc.allow.recursive = FALSE)
      #
      res.1000 <- matrix(unlist(res.1000), ncol = ncol(xdata), byrow = TRUE)
      degree   <- degree + colSums(res.1000)
    }
    names(degree) <- colnames(xdata)
    return(degree)
  }
  #
  xdata.sha256 <- loose.rock::digest.cache(xdata)

  val <- loose.rock::run.cache(weigthed.aux, xdata, cutoff, consider.unweighted,
                             cache.digest = list(xdata.sha256),
                             cache.prefix = sprintf('degree.%s', fun.prefix),
                             show.message = FALSE,
                             force.recalc = force.recalc.degree, ...)
  return(val)
}

#' Calculate degree of correlation matrix
#'
#' @param xdata calculate correlation matrix on each column
#' @param lambdas.length sparsebn parameter see sparsebnUtils::sparsebnData documentation
#' @param cutoff positive value that determines a cutoff value
#' @param consider.unweighted consider all edges as 1 if they are greater than 0
#' @param n.cores number of cores to be used
#' @param force.recalc.degree force recalculation, instead of going to cache
#' @param force.recalc.network force recalculation of network and penalty weights, instead of going to cache
#' @param show.message shows cache operation messages
#' @param ... extra parameters for sparsebn::estimate.dag
#'
#' @return a vector of the degrees
#'
#' @examples
#' n.col <- 6
#' xdata <- matrix(rnorm(n.col * 4), ncol = n.col)
#' degree.sparsebn(xdata, 50)
setGeneric('degree.sparsebn', function(xdata, lambdas.length, cutoff = 0,
                                       consider.unweighted = TRUE,
                                       n.cores = 1,
                                       show.message = FALSE, force.recalc.degree = FALSE,
                                       force.recalc.network = FALSE, ...) {
  stop('first argument must be a matrix')
})

#' Calculate degree of correlation matrix
#'
#' @param xdata calculate correlation matrix on each column
#' @param lambdas.length sparsebn parameter see sparsebnUtils::sparsebnData documentation
#' @param cutoff positive value that determines a cutoff value
#' @param consider.unweighted consider all edges as 1 if they are greater than 0
#' @param n.cores number of cores to be used
#' @param force.recalc.degree force recalculation, instead of going to cache
#' @param force.recalc.network force recalculation of network and penalty weights, instead of going to cache
#' @param show.message shows cache operation messages
#' @param ... extra parameters for sparsebn::estimate.dag
#'
#' @return a vector of the degrees
#'
#' @export
setMethod('degree.sparsebn', signature('matrix'), function(xdata, lambdas.length, cutoff = 0,
                                                           consider.unweighted = FALSE,
                                                           n.cores = 1,
                                                           show.message = FALSE, force.recalc.degree = FALSE,
                                                           force.recalc.network = FALSE, ...) {
  if (force.recalc.network) {
    force.recalc.degree <- T
  }

  # generate lambdas
  lambdas      <- sparsebnUtils::generate.lambdas(nrow(xdata), lambdas.length = lambdas.length)
  # generate data that sparsebn understands)
  sparse.xdata <- loose.rock::run.cache(sparsebnUtils::sparsebnData, xdata, levels = NULL, ivn = NULL, type = 'continuous',
                                        cache.prefix = 'sparsebn.data')
  # estimate dag structure, upperbound was wrongfully set
  dag <- loose.rock::run.cache(sparsebn::estimate.dag, sparse.xdata, lambdas = lambdas, ...,
                               cache.prefix = 'dag',
                               force.recalc = force.recalc.network)
  # estimate parameters for dag
  dag.params <- loose.rock::run.cache(sparsebnUtils::estimate.parameters, dag, data = sparse.xdata, verbose = T, cache.prefix = 'dag.params')
  # choose a dag (will use the one with most edges)
  #my.dag <- select(dag, lambda = lambdas[length(lambdas)])
  #
  x.vec                    <- abs(dag.params[[50]]$coefs@x)
  dag.params[[50]]$coefs@x <- x.vec
  x.ix.v                   <- x.vec < cutoff
  dag.params[[50]]$coefs@x[x.ix.v] <- 0
  if (consider.unweighted) {
    dag.params[[50]]$coefs@x[dag.params[[50]]$coefs@x > 0] <- 1
  }

  val <- Matrix::colSums(dag.params[[50]]$coefs) + Matrix::rowSums(dag.params[[50]]$coefs)
  return(val)
})
