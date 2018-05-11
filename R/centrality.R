
#' Parallel calculation of correlation
#'
#' Calculates the correlation between a column and all which
#'  index is above it
#'
#' @param xdata
#' @param ix.i
setGeneric('cov.worker', function(xdata, ix.i, method = 'pearson') {
  stop('first argument must be a matrix')
})
setMethod('cov.worker', signature('matrix'), function(xdata, ix.i, method = 'pearson') {
  #
  n.col <- ncol(xdata)
  xdata.i <- xdata[,ix.i]
  result  <- sapply((ix.i + 1):n.col, function(ix.j){
    cov(xdata.i, xdata[,ix.j], method = method)
  })
  result[is.na(result)] <- 0
  return(result)
})

#' Calculate correlation of large matrix
#'
#' @param xdata calculate correlation matrix on each column
#' @param n.cores number of cores to be used
#' @param build.output string that can be 'matrix', 'vector' or NULL and determines the output of the function
#'
#' @return a mtarix if
#'
#' @export
#' @examples
#' n.col <- 6
#' xdata <- matrix(rnorm(n.col * 4), ncol = n.col)
#' cor.parallel(xdata)
setGeneric('cov.parallel', function(xdata, method = 'pearson', base.dir = verissimo::base.dir(), build.output = 'matrix', n.cores = parallel:::detectCores(),
                                    force.recalc = FALSE,
                                    show.message  = FALSE) {
  stop('first argument must be a matrix')
})

setMethod('cov.parallel', signature('matrix'), function(xdata, method = 'pearson',
                                                        base.dir      = verissimo::base.dir(),
                                                        build.output  = 'matrix',
                                                        n.cores       = parallel:::detectCores(),
                                                        force.recalc  = FALSE,
                                                        show.message  = FALSE) {
  xdata.sha256 <- verissimo::digest.cache(xdata)
  cov.aux <- function(xdata, method) {
    dir.create(base.dir, showWarnings = FALSE)
    result <- parallel::mclapply( as.numeric(1:(ncol(xdata)-1)), function(ix.i) {
      tryCatch({
        result <- verissimo::run.cache(cov.worker, xdata, ix.i, method = method,
                                      #
                                      base.dir     = base.dir,
                                      cache.digest = list(xdata.sha256),
                                      cache.prefix = 'covariance',
                                      show.message = show.message,
                                      force.recalc = force.recalc)
      },
      error = function(error.str) {
        flog.error('This error has occured %s', error.str)
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
  result <- verissimo::run.cache(cov.aux, xdata, method,
                                 #
                                 cache.prefix = 'cov.aux',
                                 cache.digest = list(xdata.sha256),
                                 base.dir = base.dir,
                                 force.recalc = force.recalc,
                                 show.message = show.message)
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
})













#' Parallel calculation of correlation
#'
#' Calculates the correlation between a column and all which
#'  index is above it
#'
#' @param xdata
#' @param ix.i
setGeneric('cor.worker', function(xdata, ix.i, method = 'pearson') {
  stop('first argument must be a matrix')
})
setMethod('cor.worker', signature('matrix'), function(xdata, ix.i, method = 'pearson') {
  #
  n.col <- ncol(xdata)
  xdata.i <- xdata[,ix.i]
  result  <- sapply((ix.i + 1):n.col, function(ix.j){
    cor(xdata.i, xdata[,ix.j], method = method)
  })
  result[is.na(result)] <- 0
  # out.result <- Matrix::Matrix(0, ncol = n.col, nrow = 1)
  # out.result[1,(ix.i+1):n.col] <- result
  return(result)
})

#' Calculate correlation of large matrix
#'
#' @param xdata calculate correlation matrix on each column
#' @param n.cores number of cores to be used
#' @param build.output string that can be 'matrix', 'vector' or NULL and determines the output of the function
#'
#' @return a mtarix if
#'
#' @export
#' @examples
#' n.col <- 6
#' xdata <- matrix(rnorm(n.col * 4), ncol = n.col)
#' cor.parallel(xdata)
setGeneric('cor.parallel', function(xdata, method = 'pearson',
                                    base.dir = verissimo::base.dir(), build.output  = 'matrix', n.cores = parallel:::detectCores(), force.recalc = FALSE,
                                    show.message  = FALSE) {
  stop('first argument must be a matrix')
})

setMethod('cor.parallel', signature('matrix'), function(xdata, method = 'pearson',
                                                        base.dir      = verissimo::base.dir(),
                                                        build.output  = 'matrix',
                                                        n.cores       = parallel:::detectCores(),
                                                        force.recalc  = FALSE,
                                                        show.message  = FALSE) {
  xdata.sha256 <- verissimo::digest.cache(xdata)
  cor.aux <- function(xdata, method) {
    dir.create(base.dir, showWarnings = FALSE)
    result <- parallel::mclapply( as.numeric(1:(ncol(xdata)-1)), function(ix.i) {
      tryCatch({
        result <- verissimo::run.cache(cor.worker, xdata, ix.i, method = method,
                                       #
                                       base.dir     = base.dir,
                                       cache.digest = list(xdata.sha256),
                                       cache.prefix = 'correlation',
                                       show.message = show.message,
                                       force.recalc = force.recalc)
      },
      error = function(error.str) {
        flog.error('This error has occured %s', error.str)
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
  result <- verissimo::run.cache(cor.aux, xdata, method,
                                 #
                                 cache.prefix = 'cor.aux',
                                 cache.digest = list(xdata.sha256),
                                 base.dir     = base.dir,
                                 force.recalc = force.recalc,
                                 show.message = show.message)
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
})

#' Calculate degree of correlation matrix
#'
#' @param xdata calculate correlation matrix on each column
#' @param method correlation method to be used
#' @param cutoff positive value that determines a cutoff value
#' @param consider.unweighted consider all edges as 1 if they are greater than 0
#' @param base.dir where to store the cache of the results
#' @param n.cores number of cores to be used
#' @param force.recalc force recalculation, instead of going to cache
#'
#' @return a vector of the degrees
#'
#' @examples
#' n.col <- 6
#' xdata <- matrix(rnorm(n.col * 4), ncol = n.col)
#' degree.weighted(xdata)
#' degree.weighted(xdata, cutoff = .5)
#' degree.weighted(xdata, cutoff = .5, consider.unweighted = T)
#' degree.weighted(xdata, cutoff = .5, consider.unweighted = T, force.recalc.degree = T, force.recalc.correlation = T)
setGeneric('degree.cor.weighted', function(xdata, method = 'pearson', cutoff = 0, consider.unweighted = FALSE,
                                       base.dir = verissimo::base.dir(), n.cores = parallel:::detectCores(),
                                       show.message = FALSE, force.recalc.degree = FALSE, force.recalc.correlation = FALSE) {
  stop('first argument must be a matrix')
})

setMethod('degree.cor.weighted', signature('matrix'), function(xdata, method = 'pearson', cutoff = 0, consider.unweighted = FALSE,
                                                           base.dir = verissimo::base.dir(), n.cores = parallel:::detectCores(),
                                                           show.message = FALSE, force.recalc.degree = FALSE, force.recalc.correlation = FALSE) {
  if (force.recalc.correlation) {
    force.recalc.degree <- T
  }
  #
  # auxiliary function to be able to call with cache
  #
  weigthed.aux <- function(xdata, cutoff, consider.unweighted, method, base.dir) {
    degree <- array(0, ncol(xdata))
    added.sum <- 1000
    for (ix.outer in seq(1, ncol(xdata) - 1, added.sum)) {
      max.ix <- min(ix.outer + added.sum - 1, ncol(xdata) - 1)
      res.1000 <- parallel::mclapply(seq(ix.outer, max.ix , 1), function(ix.i) {
        line <- verissimo::run.cache(cor.worker, xdata, ix.i, method = method,
                                    base.dir     = base.dir,
                                    cache.digest = list(xdata.sha256),
                                    cache.prefix = 'correlation',
                                    show.message = F,
                                    force.recalc = force.recalc.correlation)
        if (any(!is.numeric(line))) {
          line <- verissimo::run.cache(cor.worker, xdata, ix.i, method = method,
                                      base.dir     = base.dir,
                                      cache.digest = list(xdata.sha256),
                                      cache.prefix = 'correlation',
                                      show.message = F,
                                      force.recalc = T)
        }
        #
        line[is.na(line)]   <- 0 # failsafe in case there was a failure in cor (i.e. sd = 0)
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
  dir.create(base.dir, showWarnings = FALSE)
  xdata.sha256 = verissimo::digest.cache(xdata)
  val <- verissimo::run.cache(weigthed.aux, xdata, cutoff, consider.unweighted, method, base.dir,
                             base.dir     = base.dir,
                             cache.digest = list(xdata.sha256),
                             cache.prefix = 'degree.cor',
                             show.message = show.message,
                             force.recalc = force.recalc.degree)
  return(val)
})

#' Title
#'
#' @param xdata calculate correlation matrix on each column
#' @param method correlation method to be used
#' @param cutoff positive value that determines a cutoff value
#' @param consider.unweighted consider all edges as 1 if they are greater than 0
#' @param base.dir where to store the cache of the results
#' @param n.cores number of cores to be used
#' @param force.recalc force recalculation, instead of going to cache
#'
#' @return a vector of the degrees
#' @export
#'
#' @examples
#' n.col <- 6
#' xdata <- matrix(rnorm(n.col * 4), ncol = n.col)
#' degree(xdata)
#' degree(xdata, cutoff = .5)
#' degree(xdata, cutoff = .5, consider.unweighted = T)
setGeneric('degree.cor', function(xdata, method = 'pearson', cutoff = 0, consider.unweighted = FALSE,
                              base.dir = verissimo::base.dir(), n.cores = parallel:::detectCores(),
                              force.recalc = FALSE) {
  stop('first argument must be a matrix')
})

setMethod('degree.cor', signature('matrix'), function(xdata, method = 'pearson', cutoff = 0, consider.unweighted = FALSE,
                                                  base.dir = verissimo::base.dir(), n.cores = parallel:::detectCores(),
                                                  force.recalc = FALSE) {
  degree.weighted(xdata, base.dir = base.dir, cutoff = cutoff, consider.unweighted = T, n.cores = n.cores)
})















#' Calculate degree of correlation matrix
#'
#' @param xdata calculate correlation matrix on each column
#' @param method correlation method to be used
#' @param cutoff positive value that determines a cutoff value
#' @param consider.unweighted consider all edges as 1 if they are greater than 0
#' @param base.dir where to store the cache of the results
#' @param n.cores number of cores to be used
#' @param force.recalc force recalculation, instead of going to cache
#'
#' @return a vector of the degrees
#'
#' @examples
#' n.col <- 6
#' xdata <- matrix(rnorm(n.col * 4), ncol = n.col)
#' degree.weighted(xdata)
#' degree.weighted(xdata, cutoff = .5)
#' degree.weighted(xdata, cutoff = .5, consider.unweighted = T)
#' degree.weighted(xdata, cutoff = .5, consider.unweighted = T, force.recalc.degree = T, force.recalc.correlation = T)
setGeneric('degree.cov.weighted', function(xdata, method = 'pearson', cutoff = 0, consider.unweighted = FALSE,
                                       base.dir = verissimo::base.dir(), n.cores = parallel:::detectCores(),
                                       show.message = FALSE, force.recalc.degree = FALSE, force.recalc.covariance = FALSE) {
  stop('first argument must be a matrix')
})

setMethod('degree.cov.weighted', signature('matrix'), function(xdata, method = 'pearson', cutoff = 0, consider.unweighted = FALSE,
                                                           base.dir = verissimo::base.dir(), n.cores = parallel:::detectCores(),
                                                           show.message = FALSE, force.recalc.degree = FALSE, force.recalc.covariance = FALSE) {
  if (force.recalc.covariance) {
    force.recalc.degree <- T
  }
  #
  # auxiliary function to be able to call with cache
  #
  weigthed.aux <- function(xdata, cutoff, consider.unweighted, method) {
    degree <- array(0, ncol(xdata))
    added.sum <- 1000
    for (ix.outer in seq(1, ncol(xdata) - 1, added.sum)) {
      max.ix <- min(ix.outer + added.sum - 1, ncol(xdata) - 1)
      res.1000 <- parallel::mclapply(seq(ix.outer, max.ix , 1), function(ix.i) {
        line <- verissimo::run.cache(cov.worker, xdata, ix.i, method = method,
                                    base.dir     = base.dir,
                                    cache.digest = list(xdata.sha256),
                                    cache.prefix = 'covariance',
                                    show.message = T,
                                    force.recalc = force.recalc.covariance)
        if (any(!is.numeric(line))) {
          line <- verissimo::run.cache(cov.worker, xdata, ix.i, method = method,
                                      base.dir     = base.dir,
                                      cache.digest = list(xdata.sha256),
                                      cache.prefix = 'covariance',
                                      show.message = T,
                                      force.recalc = T)
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
  dir.create(base.dir, showWarnings = FALSE)
  xdata.sha256 <- verissimo::digest.cache(xdata)

  val <- verissimo::run.cache(weigthed.aux, xdata, cutoff, consider.unweighted, method,
                             base.dir     = base.dir,
                             cache.digest = list(xdata.sha256),
                             cache.prefix = 'degree.cov',
                             show.message = show.message,
                             force.recalc = force.recalc.degree)
  return(val)
})




























#' Calculate degree of correlation matrix
#'
#' @param xdata calculate correlation matrix on each column
#' @param method correlation method to be used
#' @param cutoff positive value that determines a cutoff value
#' @param consider.unweighted consider all edges as 1 if they are greater than 0
#' @param base.dir where to store the cache of the results
#' @param n.cores number of cores to be used
#' @param force.recalc force recalculation, instead of going to cache
#'
#' @return a vector of the degrees
#'
#' @examples
#' n.col <- 60
#' xdata <- matrix(rnorm(n.col * 4), ncol = n.col)
#' degree.cor.inv.weighted(xdata, n.cores = 1)
#' degree.cor.inv.weighted(xdata, cutoff = .5)
#' degree.cor.inv.weighted(xdata, cutoff = .5, consider.unweighted = T)
#' degree.cor.inv.weighted(xdata, cutoff = .5, consider.unweighted = T, force.recalc.degree = T, force.recalc.correlation = T)
setGeneric('degree.cor.inv.weighted', function(xdata, method = 'pearson', cutoff = 0, consider.unweighted = FALSE,
                                           base.dir = loose.rock::base.dir(), n.cores = parallel:::detectCores(),
                                           show.message = FALSE, force.recalc.degree = FALSE, force.recalc.correlation = FALSE) {
  stop('first argument must be a matrix')
})

setMethod('degree.cor.inv.weighted', signature('matrix'), function(xdata, method = 'pearson', cutoff = 0, consider.unweighted = FALSE,
                                                               base.dir = loose.rock::base.dir(), n.cores = parallel:::detectCores(),
                                                               show.message = FALSE, force.recalc.degree = FALSE, force.recalc.correlation = FALSE) {
  if (force.recalc.correlation) {
    force.recalc.degree <- T
  }
  #
  # auxiliary function to be able to call with cache
  #
  weigthed.aux <- function(xdata, cutoff, consider.unweighted, method, base.dir) {
    degree <- array(0, ncol(xdata))
    added.sum <- 1000
    for (ix.outer in seq(1, ncol(xdata) - 1, added.sum)) {
      max.ix <- min(ix.outer + added.sum - 1, ncol(xdata) - 1)
      res.1000 <- parallel::mclapply(seq(ix.outer, max.ix , 1), function(ix.i) {
        line <- verissimo::run.cache(cor.worker, xdata, ix.i, method = method,
                                     base.dir     = base.dir,
                                     cache.digest = list(xdata.sha256),
                                     cache.prefix = 'correlation',
                                     show.message = F,
                                     force.recalc = force.recalc.correlation)
        if (any(!is.numeric(line))) {
          line <- verissimo::run.cache(cor.worker, xdata, ix.i, method = method,
                                       base.dir     = base.dir,
                                       cache.digest = list(xdata.sha256),
                                       cache.prefix = 'correlation',
                                       show.message = F,
                                       force.recalc = T)
        }
        #
        line.old <- line
        line[is.na(line)]   <- 0 # failsafe in case there was a failure in cor (i.e. sd = 0)
        line                <- abs(line)
        line[line != 0]     <- 1/line
        line[line == 0]     <- max(line != 0) + 1
        line[line < cutoff] <- 0
        if (consider.unweighted) { line[line != 0] <- 1 }
        line <- c(rep(0, ix.i - 1), sum(line), line)
        return(line)
      }, mc.cores = n.cores, mc.allow.recursive = FALSE)
      #
      res.1000 <- matrix(unlist(res.1000), ncol = ncol(xdata), byrow = TRUE)
      print(res.1000)
      degree   <- degree + colSums(res.1000)
    }
    names(degree) <- colnames(xdata)
    return(degree)
  }
  #
  dir.create(base.dir, showWarnings = FALSE)
  xdata.sha256 = verissimo::digest.cache(xdata)
  val <- verissimo::run.cache(weigthed.aux, xdata, cutoff, consider.unweighted, method, base.dir,
                              base.dir     = base.dir,
                              cache.digest = list(xdata.sha256),
                              cache.prefix = 'degree.cor.inv',
                              show.message = show.message,
                              force.recalc = force.recalc.degree)
  return(val)
})
