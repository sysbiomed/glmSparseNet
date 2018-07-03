#' Setup network options
#'
#' Setup network options, such as using weighted or unweighted degree,
#'  which centrality measure to use
#'
#' @param method in case of correlation and covariance, which method to use
#' @param unweighted calculate degree using unweighted network
#' @param cutoff cuttoff value in network edges to trim the network
#' @param centrality centrality measure to use, currently only supports degree
#' @param n.cores number of cores to use, default to 1
#'
#' @return a list of options
#' @export
#'
#' @examples
#' network.options.default(unweighted = FALSE)
network.options.default <- function(method     = 'pearson',
                                    unweighted = TRUE,
                                    cutoff     = 0,
                                    centrality = 'degree',
                                    min.degree = 0,
                                    n.cores    = 1) {
  return(list(method = method,
              unweighted = unweighted,
              cutoff = cutoff,
              centrality = centrality,
              n.cores = n.cores,
              min.degree = min.degree))
}


#' Calculate penalty based on data
#'
#' Internal method to calculate the network using data-dependant methods
#'
#' @param xdata input data
#' @param penalty.type which method to use
#' @param network.options options to be used
#'
#' @return
#'
#' @examples
#' xdata <- matrix(rnorm(100), ncol = 20)
#' calc.penalty(xdata, 'correlation')
#' calc.penalty(xdata, 'correlation', network.options.default(cutoff = .6))
#' calc.penalty(xdata, 'covariance')
#' calc.penalty(xdata, 'covariance', network.options.default(cutoff = .6))
calc.penalty <- function(xdata, penalty.type, network.options = network.options.default()) {
  if (network.options$centrality == 'degree') {
    if (penalty.type == 'correlation') {
      penalty.factor <- degree.cor.weighted(xdata,
                                            method              = network.options$method,
                                            consider.unweighted = network.options$unweighted,
                                            cutoff              = network.options$cutoff,
                                            #
                                            n.cores = network.options$n.cores,
                                            show.message = F)
    } else if (penalty.type == 'covariance') {
      penalty.factor <- degree.cov.weighted(xdata,
                                            method              = network.options$method,
                                            consider.unweighted = network.options$unweighted,
                                            cutoff              = network.options$cutoff,
                                            #
                                            n.cores = network.options$n.cores,
                                            show.message = F)
    } else {
      stop('Unkown network type, see documentation of network.glmnet')
    }
  } else {
    stop(sprintf('Centrality method not recognised: %d', network.options$centrality))
  }
  return(penalty.factor)
}
