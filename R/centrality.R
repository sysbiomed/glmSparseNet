#' Tmp directory to store results
#'
#' @return a path to a temporary directory
#'
#' @examples
#' base.dir()
base.dir <- function() {
  gsub('[a-zA-Z0-9]+$', 'network-cox', tempdir())
}


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
#' @param build.matrix boolean parameter to return the matrix in the end
#'
#' @return a mtarix if
#'
#' @export
#' @examples
#' n.col <- 6
#' xdata <- matrix(rnorm(n.col * 4), ncol = n.col)
#' cor.parallel(xdata)
setGeneric('cor.parallel', function(xdata, method = 'pearson',
                                    base.dir = network.cox::base.dir(), build.matrix = T, n.cores = parallel:::detectCores(), force.recalc = FALSE,
                                    show.message  = FALSE) {
  stop('first argument must be a matrix')
})

setMethod('cor.parallel', signature('matrix'), function(xdata, method = 'pearson',
                                                        base.dir      = network.cox::base.dir(),
                                                        build.matrix  = T,
                                                        n.cores       = parallel:::detectCores(),
                                                        force.recalc  = FALSE,
                                                        show.message  = FALSE) {
  dir.create(base.dir, showWarnings = FALSE)
  xdata.sha256 = verissimo::digest.cache(xdata)
  result <- parallel::mclapply( 1:(ncol(xdata)-1), function(ix.i) {

    tryCatch({
      result <- verissimo::runCache(cor.worker, xdata, ix.i, method = method,
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
    if (build.matrix) {
      return(result)
    } else {
      return(TRUE)
    }
  }, mc.cores = n.cores, mc.silent = F)

  if (build.matrix) {
    output <- Matrix::Matrix(0, ncol = ncol(xdata), nrow = ncol(xdata), sparse = TRUE)
    for(ix in 1:(ncol(xdata)-1)) {
      output[ix, (ix + 1):ncol(xdata)] <- result[[ix]]
    }
    return(output)
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
setGeneric('degree.weighted', function(xdata, method = 'pearson', cutoff = 0, consider.unweighted = FALSE,
                                       base.dir = network.cox::base.dir(), n.cores = parallel:::detectCores(),
                                       show.message = FALSE, force.recalc.degree = FALSE, force.recalc.correlation = FALSE) {
  stop('first argument must be a matrix')
})

setMethod('degree.weighted', signature('matrix'), function(xdata, method = 'pearson', cutoff = 0, consider.unweighted = FALSE,
                                                           base.dir = network.cox::base.dir(), n.cores = parallel:::detectCores(),
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
        line <- verissimo::runCache(cor.worker, xdata, ix.i, method = method,
                                    base.dir     = base.dir,
                                    cache.digest = list(xdata.sha256),
                                    cache.prefix = 'correlation',
                                    show.message = F,
                                    force.recalc = force.recalc.correlation)
        if (any(!is.numeric(line))) {
          line <- verissimo::runCache(cor.worker, xdata, ix.i, method = method,
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
  val <- verissimo::runCache(weigthed.aux, xdata, cutoff, consider.unweighted, method, base.dir,
                             base.dir     = base.dir,
                             cache.digest = list(xdata.sha256),
                             cache.prefix = 'degree',
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
setGeneric('degree', function(xdata, method = 'pearson', cutoff = 0, consider.unweighted = FALSE,
                              base.dir = network.cox::base.dir(), n.cores = parallel:::detectCores(),
                              force.recalc = FALSE) {
  stop('first argument must be a matrix')
})

setMethod('degree', signature('matrix'), function(xdata, method = 'pearson', cutoff = 0, consider.unweighted = FALSE,
                                                  base.dir = network.cox::base.dir(), n.cores = parallel:::detectCores(),
                                                  force.recalc = FALSE) {
  degree.weighted(xdata, base.dir = base.dir, cutoff = cutoff, consider.unweighted = T, n.cores = n.cores)
})

