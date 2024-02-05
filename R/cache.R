#' Default digest method
#'
#' Sets a default caching algorithm to use with run.cache
#'
#' @param val object to calculate hash over
#'
#' @return a hash of the sha256
#'
#' @examples
#' glmSparseNet:::digest.cache(c(1,2,3,4,5))
#' glmSparseNet:::digest.cache('some example')
digest.cache <- function(val) {
  digest::digest(val, algo = 'sha256')
}

#' Temporary directory for runCache
#'
#' @return a path to a temporary directory used by runCache
tempdir.cache <- function() {
  base.dir <- getwd()
  return(file.path(base.dir, 'run-cache'))
}

#' Run function and save cache
#'
#' This method saves the function that's being called
#'
#' @param base.dir directory where data is stored
#' @param fun function call name
#' @param ... parameters for function call
#' @param seed when function call is random, this allows to set seed beforehand
#' @param cache.prefix prefix for file name to be generated from
#' parameters (...)
#' @param cache.digest cache of the digest for one or more of the parameters
#' @param show.message show message that data is being retrieved from cache
#' @param force.recalc force the recalculation of the values
#' @param add.to.hash something to add to the filename generation
#'
#' @return the result of fun(...)
#'
#' @examples
#' # [optional] save cache in a temporary directory
#' #
#' glmSparseNet:::base.dir(tempdir())
#' glmSparseNet:::run.cache(c, 1, 2, 3, 4)
#' #
#' # next three should use the same cache
#' #  note, the middle call should be a little faster as digest is not
#' #  calculated
#' #   for the first argument
#' glmSparseNet:::run.cache(c, 1, 2, 3, 4)
#' glmSparseNet:::run.cache(c, a=1, 2, c= 3, 4)
#' \donttest{
#' # Using a local folder
#' # glmSparseNet:::run.cache(c, 1, 2, 3, 4, base.dir = "runcache")
#' }
methods::setGeneric("run.cache", function(fun,
                                          ...,
                                          seed = NULL,
                                          base.dir = NULL,
                                          cache.prefix = 'generic_cache',
                                          cache.digest = list(),
                                          show.message = NULL,
                                          force.recalc = FALSE,
                                          add.to.hash = NULL) {
  message(
    'Wrong arguments, first argument must be a path and second a function!'
  )
  message('  Usage: run(tmpBaseDir, functionName, 1, 2, 3, 4, 5)')
  message(
    '  Usage: run(tmpBaseDir, functionName, 1, 2, 3, 4, 5, ',
    'cache.prefix = \'someFileName\', force.recalc = TRUE)'
  )
  stop('Arguments not supported.')
})

#' Build digest of function from the actual code
#'
#' @param fun function call name
#'
#' @return a digest
#'
#' @examples
#' glmSparseNet:::build.function.digest(sum)
#' glmSparseNet:::build.function.digest(c)
build.function.digest <- function(fun) {
  digest.fun <- if (methods::is(fun, 'standardGeneric')) {
    # if it is a generic, then use code for all methods
    methods.found <- methods::findMethods(fun)
    vapply(
      names(methods.found),
      function(ix) {
        if (is.null(attributes(methods.found[[ix]])$srcref)) {
          return(digest.cache(toString(body(methods.found[[ix]]))))
        } else {
          return(digest.cache(toString(attributes(methods.found[[ix]])$srcref)))
        }
      },
      'string'
    )
  } else if (is.primitive(fun)) {
    fun
  } else if (
    methods::is(fun, 'function') &&
    !is.null(attributes(fun)$srcref)) {
    toString(attributes(fun)$srcref)
  } else if (!is.null(body(fun))) {
    body(fun)
  } else {
    # default to just fun
    fun
  }

  return(digest.cache(digest.fun))
}

#' Write a file in run-cache directory to explain the origin
#'
#' @param base.dir directory where to build this file
#'
#' @return the path to the file it has written
#'
#' @examples
#'
#' glmSparseNet:::write.readme(tempdir())
write.readme <- function(base.dir) {
  readme.path <- file.path(base.dir, "what_is_this_folder.txt")

  readme.text <- c(
    "This directory was automatically created in R when function 'run.cache'",
    "was executed (from 'glmSparseNet' package). This might have been done by",
    "you directly or by another function to cache results.",
    "",
    "This folder can be safely deleted as it only contains a cache of the",
    "results of functions",
    "",
    "package link in BioConductor:",
    "   https://bioconductor.org/packages/release/bioc/html/glmSparseNet.html",
    "github link: ",
    "   https://github.com/sysbiomed/glmSparseNet",
    "",
    "Have a great day"
  )

  if (!file.exists(readme.path)) {
    tryCatch({
      fileConn <- file(readme.path)
      writeLines(readme.text, con = fileConn)
      close(fileConn)
    }, error = function(err) {
      # do nothing as an error here should not block the main process
    })
  }
  return(readme.path)
}

#' Create directories for cache
#'
#' @param base.dir tentative base dir to create.
#' @param parent.path first 4 characters of digest that will become parent
#' directory for the actual cache file (this reduces number of files per folder)
#'
#' @return a list of updated base.dir and parent.dir
#'
#' @examples
#' glmSparseNet:::create.directory.for.cache(tempdir(), 'abcd')
#' \donttest{
#'   glmSparseNet:::create.directory.for.cache(
#'     file.path(getwd(), 'run-cache'), 'abcd'
#'   )
#' }
create.directory.for.cache <- function (base.dir, parent.path) {

  # create the directory to store cache
  dir.create(base.dir, showWarnings = FALSE)

  if (!dir.exists(base.dir)) {
    warning(
      'Could not create cache base folder at ',
      '\'', base.dir, '\'',
      '... trying to use current working directory'
    )
    base.dir      <- glmSparseNet.options('base.dir')
    dir.create(base.dir, showWarnings = FALSE)

    if (!dir.exists(base.dir)) {
      base.dir <- file.path(getwd(), 'run-cache')
      dir.create(base.dir, showWarnings = FALSE)
    }
  }

  parent.dir <- file.path(base.dir, parent.path)
  dir.create(parent.dir, showWarnings = FALSE)

  if (!dir.exists(parent.dir)) {
    warning(
      'Could not create cache folder inside base.dir at ',
      base.dir,
      '.. trying to use globally defined base.dir or ',
      'if it fails current directory'
    )
    base.dir      <- glmSparseNet.options('base.dir')
    parent.dir    <- file.path(base.dir, parent.path)
    dir.create(parent.dir, showWarnings = FALSE, recursive = TRUE)

    if (!dir.exists(parent.dir)) {
      base.dir      <- base.dir <- file.path(getwd(), 'run-cache')
      parent.dir    <- file.path(base.dir, parent.path)
      dir.create(parent.dir, showWarnings = FALSE, recursive = TRUE)
    }
  }

  write.readme(base.dir)

  return(list(base.dir = base.dir, parent.dir = parent.dir))
}

#' Saving the cache
#'
#' @param result main result to save
#' @param path path to the file to save
#' @param compression compression method to be used
#' @param show.message TRUE to show messages, FALSE otherwise
#'
#' @return result of save operation
#'
#' @examples
#' glmSparseNet:::save.run.cache(
#'   35, file.path(tempdir(), 'save.run.cache.Rdata'), FALSE, TRUE
#' )
save.run.cache <- function(result, path, compression, show.message) {
  #
  tryCatch({
    spec <- tryCatch({.__NAMESPACE__.$spec}, error = function() {})
    epochMilliseconds <- as.double(Sys.time()) * 1000 # seconds
    #
    if (show.message) { message('Saving in cache:  ', path) }
    save(
      result,
      epochMilliseconds,
      spec,
      file = path,
      compress = compression,
      version = NULL
    )
  }, error = function(err) {
    warning(
      'Problem when saving cache. Attempting to deliver results...\n\n',
      '  What happened: ', err)
    NULL
  })
}

#' Run function and save cache
#'
#' @inheritParams run.cache
#' @inherit run.cache return examples details
methods::setMethod(
  'run.cache',
  signature('function'),
  function(
    fun, ...,
    # run.cache options
    seed         = NULL,   base.dir     = NULL, cache.prefix  = 'generic_cache',
    cache.digest = list(), show.message = NULL, force.recalc  = FALSE,
    add.to.hash   = NULL) {
    #
    # base.dir
    if (is.null(base.dir)) { base.dir <- glmSparseNet.options('base.dir') }
    if (is.null(show.message)) {
      show.message <- glmSparseNet.options('show.message')
    }
    compression <- glmSparseNet.options('compression')

    #
    args <- list(...)
    if (!is.null(seed)) {
      args[['runCache.seed']] <- seed
      do.call(set.seed, list(seed))
    }
    if (!is.null(add.to.hash)) {
      args[['runCache.add.to.hash']] <- add.to.hash
    }

    # Build digest of each of the arguments
    args <- lapply(seq_along(args), function(ix) {
      if (length(cache.digest) >= ix && !is.null(cache.digest[[ix]])) {
        return(cache.digest[[ix]])
      }
      digest.cache(args[[ix]])
    })

    # Build digest of the function's code
    #  (if it changes, then cache is invalidated)
    args[['cache.fun']] <- build.function.digest(fun)

    # digest all the arguments together
    my.digest <- digest.cache(args)

    filename    <- sprintf('cache-%s-H_%s.RData', cache.prefix, my.digest)
    parent.path <- strtrim(my.digest, width = 4)

    # create dir and update base.dir (in case it failed)
    cache.dir.paths <- create.directory.for.cache(base.dir, parent.path)
    parent.dir <- cache.dir.paths$parent.dir
    base.dir   <- cache.dir.paths$base.dir

    # Calculate
    result <- if (dir.exists(parent.dir)) {
      path <- file.path(base.dir, parent.path, filename)
      calculate.result(path = path,
                       compression = compression,
                       force.recalc = force.recalc,
                       show.message = show.message,
                       fun = fun,
                       ...)
    } else {
      warning(
        'Could not save cache, possibly cannot create directory: ',
        base.dir, ' or ', file.path(base.dir, parent.path),
        sep = ''
      )
      # just calculate
      fun(...)
    }
    return(result)
  })


#' Calculate/load result and save if necessary
#'
#' This is where the actual work is done
#'
#' @param path path to save cache
#' @param compression compression used in save
#' @param force.recalc force to recalculate cache
#' @param show.message boolean to show messages
#' @param fun function to be called
#' @param ... arguments to said function
#',
#' @return result of fun(...)
#'
#' @examples
#' glmSparseNet:::calculate.result(
#'   file.path(tempdir(),'calculate.result.Rdata'),
#'   'gzip',
#'   FALSE,
#'   TRUE,
#'   sum,
#'   1, 2, 3
#' )
calculate.result <- function(
  path, compression, force.recalc, show.message, fun, ...
) {
  #
  result <- NULL
  if (file.exists(path) && !force.recalc) {
    if (show.message) {
      message('Loading from cache (not calculating):\n  ', path)
    }
    result <- tryCatch(
      {
        tmp.env <- new.env()
        load(path, envir = tmp.env)
        if (
          show.message &&
          !is.null(tmp.env$epochMilliseconds) &&
          is.double(tmp.env$epochMilliseconds)
        ) {
          my.msg <- paste0(
            'Cache was created at ', .POSIXct(tmp.env$epochMilliseconds/1000)
          )
          if (!is.null(tmp.env$spec) && !is.na(tmp.env$spec['version'])) {
            message(my.msg, ' using glmSparseNet v', tmp.env$spec['version'])
          } else {
            message(my.msg, ' using glmSparseNet before v1.0.16 or before')
          }
        }
        tmp.env$result
      },
      error = function(err) {
        warning(
          'WARN:: ', err, ' -- file: ', path, '.\n  -> Calculating again.\n'
        )
        result.tmp <- fun(...)
        save.run.cache(result.tmp, path, compression, show.message)
        result.tmp
      }
    )
  } else {
    # calculate function
    result <- fun(...)
    save.run.cache(result, path, compression, show.message)
  }
  return(result)
}
