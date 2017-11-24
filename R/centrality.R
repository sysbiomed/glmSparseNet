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
  out.result <- Matrix::Matrix(0, ncol = n.col, nrow = 1)
  out.result[1,(ix.i+1):n.col] <- result
  return(out.result)
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
#' calculateCorr(xdata) - Matrix::triu(cor(xdata), 1)
setGeneric('cor.parallel', function(xdata, method = 'pearson', base.dir = network.cox::base.dir(), build.matrix = T, n.cores = parallel:::detectCores()) {
  stop('first argument must be a matrix')
})

setMethod('cor.parallel', signature('matrix'), function(xdata, method = 'pearson', base.dir = network.cox::base.dir(), build.matrix = T, n.cores = parallel:::detectCores()) {
  dir.create(base.dir, showWarnings = FALSE)
  xdata.sha256 = verissimo::digest.cache(xdata)
  parallel::mclapply( 1:(ncol(xdata)-1), function(ix.i) {
    tryCatch({
      verissimo::runCache(cor.worker, xdata, ix.i, method = method, base.dir = base.dir, cache.digest = list(xdata.sha256), cache.prefix = 'correlation', show.message = F)
    },
    error = function(error.str) {
      flog.error('This error has occured %s', error.str)
    })
    return(TRUE)
  }, mc.cores = n.cores, mc.silent = F)

  if (build.matrix) {
    output <- Matrix::Matrix(0, ncol = ncol(xdata), nrow = ncol(xdata), sparse = TRUE)
    for(ix in 1:(ncol(xdata)-1)) {
      output[ix,] <- verissimo::runCache(cor.worker, xdata, ix, method = method, base.dir = base.dir, cache.digest = list(xdata.sha256), cache.prefix = 'correlation', show.message = F)
    }
    return(output)
  } else {
    return(NULL)
  }
})

#' Calculate degree of correlation matrix
#'
#' @param xdata calculate correlation matrix on each column
#' @param threshold positive value that determines a cutoff value
#'
#' @return a mtarix if
#'
#' @examples
#' n.col <- 6
#' xdata <- matrix(rnorm(n.col * 4), ncol = n.col)
#' weighted.degree(xdata)
#' weighted.degree(xdata, cutoff = .5)
setGeneric('degree.weighted', function(xdata, method = 'pearson', base.dir = network.cox::base.dir(), cutoff = 0, consider.unweighted = FALSE, n.cores = parallel:::detectCores()) {
  stop('first argument must be a matrix')
})

setMethod('degree.weighted', signature('matrix'), function(xdata, method = 'pearson', base.dir = network.cox::base.dir(), cutoff = 0, consider.unweighted = FALSE, n.cores = parallel:::detectCores()) {
  #
  # auxiliary function to be able to call with cache
  #
  weigthed.aux <- function(xdata, cutoff, consider.unweighted, method, base.dir) {
    degree <- array(0, ncol(xdata))
    added.sum <- 1000
    for (ix.outer in seq(1, ncol(xdata) - 1, added.sum)) {
      max.ix <- min(ix.outer + added.sum - 1, ncol(xdata) - 1)
      res.1000 <- matrix(unlist(parallel::mclapply(seq(ix.outer, max.ix , 1), function(ix.i) {
        line <- verissimo::runCache(cor.worker, xdata, ix.i, method = method,
                                    base.dir = base.dir, cache.digest = list(xdata.sha256), cache.prefix = 'correlation', show.message = F)
        #
        line[is.na(line)]   <- 0 # failsafe in case there was a failure in cor (i.e. sd = 0)
        line                <- as.vector(abs(line))
        line[line < cutoff] <- 0
        if (consider.unweighted) { line[line != 0] <- 1 }
        line[ix.i] <- line[ix.i]  + sum(line)
        return(as.vector(line))
      }, mc.cores = n.cores, mc.allow.recursive = FALSE)), ncol = ncol(xdata), byrow = TRUE)
      degree <- degree + colSums(res.1000)
    }
    names(degree) <- colnames(xdata)
    return(degree)
  }
  #
  dir.create(base.dir, showWarnings = FALSE)
  xdata.sha256 = verissimo::digest.cache(xdata)
  val <- verissimo::runCache(weigthed.aux, xdata, cutoff, consider.unweighted, method, base.dir,
                             base.dir = base.dir, cache.digest = list(xdata.sha256), cache.prefix = 'degree', show.message = F)
  return(val)
})

setGeneric('degree', function(xdata, base.dir = network.cox::base.dir(), cutoff = 0, n.cores = parallel:::detectCores()) {
  stop('first argument must be a matrix')
})

setMethod('degree', signature('matrix'), function(xdata, base.dir = network.cox::base.dir(), cutoff = 0, n.cores = parallel:::detectCores()) {
  degree.weighted(xdata, base.dir = base.dir, cutoff = cutoff, consider.unweighted = T, n.cores = n.cores)
})

