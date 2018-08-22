#' Get a balanced test and train dataset
#'
#' @param ... vectors of index (could be numeric or logical)
#' @param train.perc percentage of dataset to be training set
#' @param join.all join all index in the end in two vectors (train and test vectors)
#'
#' @return train and test index vectors (two lists if `join.all = FALSE`, two vectors otherwise)
#' @export
#'
#' @examples
#' set1 <- seq(20)
#' balanced.train.and.test(set1, train.perc = .9)
#' ####
#' set.seed(1985)
#' set1 <- rbinom(20, prob = 3/20, size = 1) == 1
#' balanced.train.and.test(set1, train.perc = .9)
#' ####
#' set1 <- c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,
#' TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE)
#' set2 <- !set1
#' balanced.train.and.test(set1, set2, train.perc = .9)
balanced.train.and.test <- function(..., train.perc = .9, join.all = TRUE) {
  # get arguments as list
  input.list <- list(...)
  # stop execution if train.perc is not between 1 and 0 (excluding 0)
  if (train.perc <= 0 || train.perc > 1) {
    stop('train.perc argument must be between [1,0[')
  }
  # initialize train set
  train.set <- list()
  test.set  <- list()
  # iterate on elipsis
  for (my.set in input.list) {
    # check if is vector of logical or numeric indexes
    if (is.vector(my.set) && (is.numeric(my.set) || is.logical(my.set))) {
      # make user ixs is a numbered index vector
      if (is.logical(my.set)) {
        ixs    <- seq_len(length(my.set))[my.set]
        max.ix <- length(my.set)
      } else {
        ixs    <- seq_len(max(my.set))[my.set]
        max.ix <- max(my.set)
      }
      # sample size to use
      sample.size  <- floor(train.perc * length(ixs))
      temp.set <- sample(ixs, size = sample.size)
      if (length(temp.set) == length(my.set) && train.perc < 1)
        warning('Training set is the same size as test set')
      temp.set <- seq_len(max.ix) %in% temp.set
      #
      train.set <- c(train.set, list(sort(which(temp.set))))

      if (train.perc == 1) {
        test.set  <- train.set
      } else {
        test.temp.set <- !temp.set
        test.temp.set[-ixs] <- FALSE
        test.set <- c(test.set, list(which(test.temp.set)))
      }

    } else {
      stop('Arguments must be either a logical or numeric vector, see help for more information.')
    }
  }
  if (join.all) {
    len <- length(train.set[[1]])
    master.train <- c()
    master.test <- c()
    for(ix in seq(train.set)) {
      master.train <- c(master.train, train.set[[ix]])
      master.test <- c(master.test, test.set[[ix]])
    }
    return(list(train = sort(master.train), test = sort(master.test)))
  }
  return(list(train = train.set, test = test.set))
}

#' Create balanced folds for cross validation
#'
#' @param ... vectors representing data
#' @param nfolds number of folds to be created
#'
#' @return list with given input, nfolds and result. The result is a list matching the input with foldid attributed to each position.
#' @export
#'
#' @examples
#' balanced.cv.folds(1:10, 11:15, nfolds = 2)
#' balanced.cv.folds(1:10, 11:13, nfolds = 10) # will give a warning
#' balanced.cv.folds(1:100, 101:133, nfolds = 10)
balanced.cv.folds <- function(..., nfolds = 10) {
  input.list <- list(...)
  output.list <- list()
  if (any(sapply(input.list, function(vec) {length(vec) < nfolds}))) {
    warning('Number of elements in vector (', length(unlist(input.list)), ') is less than \'nfolds\' (', nfolds, ')')
  }
  for (my.set in input.list) {
    #
    # count previous bins and order sequence on increasing count
    if (length(output.list) == 0) {
      my.sample <- rep(seq(nfolds),length = length(my.set))
    } else {
      my.tmp <- c()
      for(ix in seq(output.list)) {
        my.tmp <- c(my.tmp, output.list[[ix]])
      }
      my.count <- graphics::hist(my.tmp, plot = FALSE, breaks = 0:nfolds)$counts
      my.sample <- rep(seq(nfolds)[sort(my.count, index.return = TRUE)$ix], length = length(my.set))
    }
    #
    output.list <- c(output.list, list(sample(my.sample)))
  }
  if (length(output.list) == 1) {
    output.list = output.list[[1]]
    input.list = input.list[[1]]
  }
  return(list(input = input.list, output = output.list, nfolds = nfolds))
}


























#' Default digest method
#'
#' @param val object to calculate hash over
#'
#' @return a hash of the sha256
#' @export
#'
#' @examples
#' digest.cache(c(1,2,3,4,5))
digest.cache <- function(val) {
  digest::digest(val, algo = 'sha256')
}

#' Temporary directory for runCache
#'
#' @return a path to a temporary directory used by runCache
tempdir.cache <- function() {
  base.dir <- '.'
  return(file.path(dirname(base.dir), 'run-cache'))
}

#' Run function and save cache
#'
#' This method saves the function that's being called
#'
#' @param base.dir directory where data is stored
#' @param fun function call name
#' @param ... parameters for function call
#' @param seed when function call is random, this allows to set seed beforehand
#' @param cache.prefix prefix for file name to be generated from parameters (...)
#' @param cache.digest cache of the digest for one or more of the parameters
#' @param show.message show message that data is being retrieved from cache
#' @param force.recalc force the recalculation of the values
#' @param add.to.hash something to add to the filename generation
#'
#' @return the result of fun(...)
#' @export
#'
#' @examples
#' run.cache(c, 1, 2, 3, 4)
#' # next three should use the same cache
#' run.cache(c, 1, 2, 3, 4)
#' run.cache(c, 1, 2, 3, 4, cache.digest = list(digest.cache(1)))
#' run.cache(c, a=1, 2, c=3, 4) # should get result from cache
setGeneric("run.cache", function(fun,
                                 ...,
                                 seed = NULL,
                                 base.dir = NULL,
                                 cache.prefix = 'generic_cache',
                                 cache.digest = list(),
                                 show.message = NULL,
                                 force.recalc = FALSE,
                                 add.to.hash = NULL) {
  message('Wrong arguments, first argument must be a path and second a function!\n')
  message('  Usage: run(tmpBaseDir, functionName, 1, 2, 3, 4, 5)\n')
  message('  Usage: run(tmpBaseDir, functionName, 1, 2, 3, 4, 5, cache.prefix = \'someFileName\', force.recalc = TRUE)\n')
  stop('Arguments not supported.')
})

#' Run function and save cache
#'
#' @inheritParams run.cache
#'
#' @return the result of fun(...)
setMethod('run.cache',
          signature('function'),
          function(fun,
                   ...,
                   seed          = NULL,
                   base.dir      = NULL,
                   cache.prefix  = 'generic_cache',
                   cache.digest = list(),
                   show.message  = NULL,
                   force.recalc  = FALSE,
                   add.to.hash   = NULL) {
            #
            # base.dir
            if (is.null(base.dir)) { base.dir <- glmSparseNet.options('base.dir') }
            if (is.null(show.message)) { show.message <- glmSparseNet.options('show.message') }
            #
            #
            args <- list(...)
            if (!is.null(seed)) {
              args[['runCache.seed']] <- seed
              set.seed(seed)
            }
            if (!is.null(add.to.hash)) {
              args[['runCache.add.to.hash']] <- add.to.hash
            }
            #
            args <- lapply(seq_along(args), function(ix) {
              if (length(cache.digest) >= ix && !is.null(cache.digest[[ix]])) {
                return(cache.digest[[ix]])
              }
              digest.cache(args[[ix]])
            })
            if (is(fun, 'function')) {
              args[['cache.fun']] <- digest.cache(toString(attributes(fun)$srcref))
            } else if (is(fun, 'standardGeneric')) {
              aaa <- methods::findMethods(fun)
              args[['cache.fun']] <- digest.cache(sapply(names(aaa), function(ix) { digest.cache(toString(attributes(aaa[[ix]])$srcref)) }))
            } else {
              args[['cache.fun']] <- digest.cache(fun)
            }

            dir.create(base.dir, showWarnings = FALSE)

            my.digest   <- digest.cache(args)
            filename    <- sprintf('cache-%s-H_%s.RData', cache.prefix, my.digest)
            parent.path <- strtrim(my.digest, width = 4)
            #

            if (!dir.exists(base.dir)) {
              warning(sprintf('Could not create cache base folder at %s.. trying to use current working directory', base.dir))
              base.dir <- file.path(getwd(), 'run-cache')
              dir.create(base.dir, showWarnings = FALSE)
            }
            parent.dir <- file.path(base.dir, parent.path)
            dir.create(parent.dir, showWarnings = FALSE)

            if (!dir.exists(parent.dir)) {
              warning(sprintf('Could not create cache folder inside base.dir at %s.. trying to use current working directory', base.dir))
              base.dir   <- file.path(getwd(), 'run-cache')
              parent.dir <- file.path(base.dir, parent.path)
              dir.create(parent.dir, showWarnings = FALSE, recursive = TRUE)
            }

            if (dir.exists(parent.dir)) {
              path <- file.path(base.dir, parent.path, filename)
              #
              if (file.exists(path) && !force.recalc) {
                if (show.message) {
                  cat(sprintf('Loading from cache (not calculating): %s\n', path))
                }
                tryCatch({load(path)}, error = function(err) {
                  cat(sprintf('WARN:: %s -- file: %s.\n  -> Calculating again.\n', err, path))
                  result <<- fun(...)
                  if (show.message) {
                    cat(sprintf('Saving in cache: %s\n', path))
                  }
                  save(result, file = path)
                })
              } else {
                result <- fun(...)
                if (show.message) {
                  cat(sprintf('Saving in cache: %s\n', path))
                }
                save(result, file = path)
              }
            } else {
              warning(sprintf('Could not save cache, possibly cannot create directory: %s or %s', base.dir, file.path(base.dir, parent.path)))
              result <- fun(...)
            }
            return(result)
          })














#' Retrive coding genes from known databases
#'
#' It retrieves from NCBI and
#'
#' @param verbose show messages with number of genes retrieved
#'
#' @return a table with gene information
#' @export
#' @examples
#' # This can take a few minutes depending on the connection
#' \dontrun{
#'     coding.genes()
#' }
coding.genes <- function (verbose = TRUE)
{
  ensembl <- biomaRt::useMart("ensembl", host = 'http://www.ensembl.org')

  #
  # Uses hsapies from query

  dataset <- biomaRt::listDatasets(ensembl) %>%
    dplyr::filter(grepl('hsapien', dataset)) %>%
    dplyr::select(dataset) %>%
    dplyr::first() %>%
    biomaRt::useDataset(mart = ensembl)

  #
  protein.coding <- biomaRt::getBM(attributes = c("ensembl_gene_id","external_gene_name"),
                                   filters    = 'biotype',
                                   values     = c('protein_coding'),
                                   mart       = dataset,
                                   verbose    = FALSE)

  ccds <- utils::read.table(url("ftp://ftp.ncbi.nih.gov/pub/CCDS/current_human/CCDS.current.txt"),
                            sep = "\t", header = TRUE, comment.char = "|", stringsAsFactors = FALSE)

  ccds$ccds_status <- factor(proper(ccds$ccds_status))

  # Remove with ccds_status == Withdrawn
  ccds       <- ccds %>% dplyr::filter(!grepl('Withdrawm', rlang::UQ(as.name('ccds_status'))))
  ccds.genes <- unique(ccds$gene)

  if (any(ccds.genes == '' || is.na(ccds.genes))) {
    warning('Some genes from ccds have empty gene_name, skipping those')
    ccds.genes <- ccds.genes[ccds.genes == '' || is.na(ccds.genes)]
  }
  #
  biomart.genes    <- sort(unique(protein.coding$external_gene_name))
  ccds.extra.genes <- sort(ccds.genes[(!ccds.genes %in% biomart.genes)])

  coding <- biomaRt::getBM(attributes = c("ensembl_gene_id","external_gene_name"),
                           filters    = 'external_gene_name',
                           values     = c(biomart.genes, ccds.extra.genes),
                           mart       = dataset)
  if (verbose) {
    cat('Coding genes from biomaRt:', nrow(protein.coding),'\n')
    cat('   Coding genes from CCDS:', length(ccds.genes), '\n')
    cat('        Unique in biomaRt:', sum(!ccds.genes %in% biomart.genes), '\n')
    cat('           Unique in CCDS:', sum(!biomart.genes %in% ccds.genes), '\n')
    cat('-------------------------------\n')
    cat('                    genes:', nrow(coding), '\n')
  }

  return(coding)
}














#' Constants for 'glmSparseNet' package
#'
#' Log level constants and the logger options.
#'
#' The logging configuration is managed by 'glmSparseNet.options', a function
#' generated by OptionsManager within 'futile.options'.
#'
#' @name glmSparseNet.options
#' @usage glmSparseNet.options(..., simplify = FALSE, update = list())
#'
#' @param update pair list of update to options
#' @param ... TODO
#' @param simplify TODO
#'
#' @seealso \code{futile.options}
glmSparseNet.options <- futile.options::OptionsManager('glmSparseNet', default = list(base.dir = tempdir.cache(), show.message = TRUE))

#' change base.dir for run.cache
#'
#' @param path to base directory where cache is saved
#'
#' @return the new path
#' @export
#'
#' @examples
#' base.dir('/tmp/cache')
base.dir <- function(path = NULL) {
  if (!is.null(path))
    glmSparseNet.options(update = list('base.dir', path))
  return(glmSparseNet.options('base.dir'))
}

#' Show messages option in run.cache
#'
#' @param show.message boolean indicating to show messages or not
#'
#' @return the show.message option
#' @export
#'
#' @examples
#' show.message(FALSE)
show.message <- function(show.message = NULL) {
  if (!is.null(show.message))
    glmSparseNet.options(update = list('show.message', show.message))
  return(glmSparseNet.options('show.message'))
}







#' Capitalizes all words in string
#'
#' @param x String
#'
#' @return a capitalized string (all words)
#' @export
#'
#' @examples
#' proper('i saw a dEaD parrot')
proper <- function(x) {
  return(gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(x), perl=TRUE))
}





#' Custom pallete of colors
#'
#' @param ix index for a color
#'
#' @return a color
#' @export
#'
#' @examples
#' my.colors()
#' my.colors(5)
my.colors <- function(ix = NULL){
  ret.colors <- c('navyblue',  'forestgreen',  'tomato4',    'tan1',
                  'turquoise', 'springgreen1', 'brown',      'violetred1',
                  'ivory4',    'slateblue1',   'chocolate4', 'deeppink1',
                  'slategray4','coral3',       'darkblue',   'mediumorchid1', 'black')
  if (is.null(ix)) {
    return(ret.colors)
  }
  if (ix %% length(ret.colors) == 0) {
    ix <- length(ret.colors)
  }
  else {
    ix <- ix %% length(ret.colors)
  }
  return(ret.colors[ix])
}

#' Custom pallete of symbols in plots
#'
#' @param ix index for symbol
#'
#' @return a symbol
#' @export
#'
#' @examples
#' my.symbols()
#' my.symbols(2)
my.symbols <- function(ix = NULL) {
  ret.symbols <- c(0,4,1,8,5,
                   3,6,7,2,
                   9,10,11,12,
                   13,14,15,16,
                   17)
  if (is.null(ix)) {
    return(ret.symbols)
  }
  if (ix %% length(ret.symbols) == 0) {
    ix <- length(ret.symbols)
  } else {
    ix <- ix %% length(ret.symbols)
  }
  return(ret.symbols[ix])
}
