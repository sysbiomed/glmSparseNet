#' Filter MultiAssayExperiment colData for specific experiment
#'
#' @param multi.assay MultiAssayExperiment object
#' @param experiment.name name of experiment
#'
#' @return a MultiExperimentAssay with only clinical data of that experiment
#' @export
#'
#' @examples
#' dat <- MultiAssayExperiment::miniACC
#' reduce.by.experiment(dat, 'RNASeq2GeneNorm')
reduce.by.experiment <- function(multi.assay, experiment.name) {
  # Get all valid individuals from experiment (lookup the mapping)
  valid.ydata.id <- multi.assay@sampleMap[multi.assay@sampleMap$assay == experiment.name, 'primary']

  # filter the MultiAssayExperiment keeping only individuals with data in specific experiment
  suppressMessages(new.multi.assay <- multi.assay[,rownames(multi.assay@colData) %in% valid.ydata.id])

  return(new.multi.assay)
}

#' Setup network options
#'
#' Setup network options, such as using weighted or unweighted degree,
#'  which centrality measure to use
#'
#' @param trans.fun see below
#' @param min.degree minimum value that individual penalty weight can take
#' @param method in case of correlation and covariance, which method to use
#' @param unweighted calculate degree using unweighted network
#' @param cutoff cuttoff value in network edges to trim the network
#' @param centrality centrality measure to use, currently only supports degree
#' @param n.cores number of cores to use, default to 1
#'
#' The trans.fun argument takes a function definition that will apply a transformation
#' to the penalty vector calculated from the degree. This transformation allows to
#' change how the penalty is applied.
#'
#' @seealso glmOrphan glmDegree
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
                                    n.cores    = 1,
                                    trans.fun  = function(x) { x }) {
  return(list(method = method,
              unweighted = unweighted,
              cutoff = cutoff,
              centrality = centrality,
              n.cores = n.cores,
              min.degree = min.degree,
              trans.fun = trans.fun))
}


#' Calculate penalty based on data
#'
#' Internal method to calculate the network using data-dependant methods
#'
#' @param xdata input data
#' @param penalty.type which method to use
#' @param network.options options to be used
#'
#' @return vector with penalty weights
#'
#' @examples
#' xdata <- matrix(rnorm(100), ncol = 20)
#' glmSparseNet:::calc.penalty(xdata, 'correlation')
#' glmSparseNet:::calc.penalty(xdata, 'correlation', network.options.default(cutoff = .6))
#' glmSparseNet:::calc.penalty(xdata, 'covariance')
#' glmSparseNet:::calc.penalty(xdata, 'covariance', network.options.default(cutoff = .6))
calc.penalty <- function(xdata, penalty.type, network.options = network.options.default()) {
  if (network.options$centrality == 'degree') {
    if (penalty.type == 'correlation') {
      penalty.factor <- degree.cor(xdata,
                                   method              = network.options$method,
                                   consider.unweighted = network.options$unweighted,
                                   cutoff              = network.options$cutoff,
                                   #
                                   n.cores = network.options$n.cores)
    } else if (penalty.type == 'covariance') {
      penalty.factor <- degree.cov(xdata,
                                   method              = network.options$method,
                                   consider.unweighted = network.options$unweighted,
                                   cutoff              = network.options$cutoff,
                                   #
                                   n.cores = network.options$n.cores)
    } else {
      stop('Unkown network type, see documentation of glmSparseNet')
    }
  } else {
    stop(sprintf('Centrality method not recognised: %d', network.options$centrality))
  }
  return(network.options$trans.fun(penalty.factor))
}


#' Heuristic function to penalize nodes with low degree
#'
#' @param x single value of vector
#'
#' @return transformed
#' @export
#'
#' @examples
#' hub.heuristic(rnorm(1:10))
hub.heuristic <- function(x) {
  tmp.fun <- function(x,
                      a = .20 - 1,
                      b = -1,
                      g = -1) {
    return(a + 10^(-b * (exp(x) + g)))
  }
  x <- x / max(x)
  return(tmp.fun(x))
}

#' Heuristic function to penalize nodes with high degree
#'
#' @param x single value of vector
#'
#' @return transformed
#' @export
#'
#' @examples
#' orphan.heuristic(rnorm(1:10))
orphan.heuristic <- function(x) {
  tmp.fun <- function(x,
                      a = .20 - 1,
                      b = -1,
                      g = -1) {
    return(a + 10^(-b * (exp(x) + g)))
  }
  x <- x / max(x)
  return(tmp.fun(1 - x))
}
