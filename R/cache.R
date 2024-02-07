#' Default digest method
#'
#' Sets a default caching algorithm to use with `.runCache`
#'
#' @param val object to calculate hash over
#'
#' @return a hash of the sha256
#'
#' @examples
#' glmSparseNet:::.digestCache(c(1, 2, 3, 4, 5))
#' glmSparseNet:::.digestCache("some example")
.digestCache <- function(val) {
  rlang::hash(val)
}

#' Temporary directory for runCache
#'
#' @return a path to a temporary directory used by runCache
.tempdirCache <- function() file.path(getwd(), "run-cache")

#' Run function and save cache
#'
#' This method saves the function that's being called
#'
#' @param base.dir directory where data is stored
#' @param fun function call name
#' @param ... parameters for function call
#' @param seed when function call is random, this allows to set seed beforehand
#' @param cache_prefix prefix for file name to be generated from
#' parameters (...)
#' @param cache_digest cache of the digest for one or more of the parameters
#' @param show_message show message that data is being retrieved from cache
#' @param force_recalc force the recalculation of the values
#' @param add_to_hash something to add to the filename generation
#'
#' @return the result of fun(...)
#'
#' @examples
#' # [optional] save cache in a temporary directory
#' #
#' glmSparseNet:::.baseDir(tempdir())
#' glmSparseNet:::.runCache(c, 1, 2, 3, 4)
#' #
#' # next three should use the same cache
#' #  note, the middle call should be a little faster as digest is not
#' #  calculated
#' #   for the first argument
#' glmSparseNet:::.runCache(c, 1, 2, 3, 4)
#' glmSparseNet:::.runCache(c, a = 1, 2, c = 3, 4)
#' \donttest{
#' # Using a local folder
#' # glmSparseNet:::.runCache(c, 1, 2, 3, 4, base_dir = "runcache")
#' }
methods::setGeneric(".runCache", function(
    fun,
    ...,
    seed = NULL,
    base_dir = NULL,
    cache_prefix = "generic_cache",
    cache_digest = list(),
    show_message = NULL,
    force_recalc = FALSE,
    add_to_hash = NULL) {
  message(
    "Wrong arguments, first argument must be a path and second a function!"
  )
  message("  Usage: run(tmpBaseDir, functionName, 1, 2, 3, 4, 5)")
  message(
    "  Usage: run(tmpBaseDir, functionName, 1, 2, 3, 4, 5, ",
    "cache.prefix = 'someFileName', force.recalc = TRUE)"
  )
  rlang::abort("Arguments not supported in `.runCache`.")
})

#' Build digest of function from the actual code
#'
#' @param fun function call name
#'
#' @return a digest
#'
#' @examples
#' glmSparseNet:::.buildFunctionDigest(sum)
#' glmSparseNet:::.buildFunctionDigest(c)
.buildFunctionDigest <- function(fun) {
  digest.fun <- if (methods::is(fun, "standardGeneric")) {
    # if it is a generic, then use code for all methods
    methods.found <- methods::findMethods(fun)
    vapply(
      names(methods.found),
      function(ix) {
        if (is.null(attributes(methods.found[[ix]])$srcref)) {
          return(.digestCache(toString(body(methods.found[[ix]]))))
        } else {
          return(.digestCache(toString(attributes(methods.found[[ix]])$srcref)))
        }
      },
      "string"
    )
  } else if (is.primitive(fun)) {
    fun
  } else if (
    methods::is(fun, "function") && !is.null(attributes(fun)$srcref)
  ) {
    toString(attributes(fun)$srcref)
  } else if (!is.null(body(fun))) {
    body(fun)
  } else {
    # default to just fun
    fun
  }

  return(.digestCache(digest.fun))
}

#' Write a file in run-cache directory to explain the origin
#'
#' @param base_dir directory where to build this file
#'
#' @return the path to the file it has written
#'
#' @examples
#'
#' glmSparseNet:::.writeReadme(tempdir())
.writeReadme <- function(base_dir) {
  readme_path <- file.path(base_dir, "what_is_this_folder.txt")

  readme_text <- c(
    "This directory was automatically created in R when function '.runCache'",
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

  if (!file.exists(readme_path)) {
    tryCatch(
      {
        fileConn <- file(readme_path)
        writeLines(readme_text, con = fileConn)
        close(fileConn)
      },
      error = function(err) {
        # do nothing as an error here should not block the main process
      }
    )
  }
  return(readme_path)
}

#' Create directories for cache
#'
#' @param base_dir tentative base dir to create.
#' @param parent_path first 4 characters of digest that will become parent
#' directory for the actual cache file (this reduces number of files per folder)
#'
#' @return a list of updated base.dir and parent.dir
#'
#' @examples
#' glmSparseNet:::.createDirectoryForCache(tempdir(), "abcd")
#' \donttest{
#' glmSparseNet:::.createDirectoryForCache(
#'   file.path(getwd(), "run-cache"), "abcd"
#' )
#' }
.createDirectoryForCache <- function(base_dir, parent_path) {
  checkmate::assert_string(base_dir)
  checkmate::assert_string(parent_path)

  # create the directory to store cache
  dir.create(base_dir, showWarnings = FALSE)

  if (!dir.exists(base_dir)) {
    warning(
      "Could not create cache base folder at ",
      "'", base_dir, "'",
      "... trying to use current working directory"
    )
    base_dir <- getOption("glmSparseNet.base_dir")
    dir.create(base_dir, showWarnings = FALSE)

    if (!dir.exists(base_dir)) {
      base_dir <- file.path(getwd(), "run-cache")
      dir.create(base_dir, showWarnings = FALSE)
    }
  }

  parent_dir <- file.path(base_dir, parent_path)
  dir.create(parent_dir, showWarnings = FALSE)

  if (!dir.exists(parent_dir)) {
    warning(
      "Could not create cache folder inside base_dir at ",
      base_dir,
      ".. trying to use globally defined base_dir or ",
      "if it fails current directory"
    )
    base_dir <- getOption("glmSparseNet.base_dir")
    parent_dir <- file.path(base_dir, parent_path)
    dir.create(parent_dir, showWarnings = FALSE, recursive = TRUE)

    if (!dir.exists(parent_dir)) {
      base_dir <- base_dir <- file.path(getwd(), "run-cache")
      parent_dir <- file.path(base_dir, parent_path)
      dir.create(parent_dir, showWarnings = FALSE, recursive = TRUE)
    }
  }

  .writeReadme(base_dir)

  list(base_dir = base_dir, parent_dir = parent_dir)
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
#' glmSparseNet:::.saveRunCache(
#'   35, file.path(tempdir(), "save_run_cache.Rdata"), FALSE, TRUE
#' )
.saveRunCache <- function(result, path, compression, show.message) {
  #
  tryCatch(
    {
      spec <- tryCatch(
        {
          .__NAMESPACE__.$spec
        },
        error = function() {}
      )
      epochMilliseconds <- as.double(Sys.time()) * 1000 # seconds
      #
      if (show.message) {
        message("Saving in cache:  ", path)
      }
      save(
        result,
        epochMilliseconds,
        spec,
        file = path,
        compress = compression,
        version = NULL
      )
    },
    error = function(err) {
      warning(
        "Problem when saving cache. Attempting to deliver results...\n\n",
        "  What happened: ", err
      )
      NULL
    }
  )
}

#' @describeIn dot-runCache accepts function as first argument and save cache
methods::setMethod(
  ".runCache",
  signature("function"),
  function(
      fun,
      ...,
      # run_cache options
      seed = NULL,
      base_dir = NULL,
      cache_prefix = "generic_cache",
      cache_digest = list(),
      show_message = NULL,
      force_recalc = FALSE,
      add_to_hash = NULL) {
    #
    # base_dir
    if (is.null(base_dir)) {
      base_dir <- .baseDir()
    }
    if (is.null(show_message)) {
      show_message <- .showMessage()
    }
    compression <- .cacheCompression()

    #
    args <- rlang::list2(...)
    if (!is.null(seed)) {
      args[["run_cache.seed"]] <- seed
      do.call(set.seed, list(seed))
    }
    if (!is.null(add_to_hash)) {
      args[["run_cache.add_to_hash"]] <- add_to_hash
    }

    # Build digest of each of the arguments
    args <- lapply(seq_along(args), function(ix) {
      if (length(cache_digest) >= ix && !is.null(cache_digest[[ix]])) {
        return(cache_digest[[ix]])
      }
      .digestCache(args[[ix]])
    })

    # Build digest of the function's code
    #  (if it changes, then cache is invalidated)
    args[["run_cache.fun"]] <- .buildFunctionDigest(fun)

    # digest all the arguments together
    my_digest <- .digestCache(args)

    filename <- sprintf("cache-%s-H_%s.RData", cache_prefix, my_digest)
    parent_path <- strtrim(my_digest, width = 4)

    # create dir and update base.dir (in case it failed)
    cache_dir_paths <- .createDirectoryForCache(base_dir, parent_path)
    parent_dir <- cache_dir_paths$parent_dir
    base_dir <- cache_dir_paths$base_dir

    # Calculate
    result <- if (dir.exists(parent_dir)) {
      path <- file.path(base_dir, parent_path, filename)
      .calculateResult(
        path = path,
        compression = compression,
        force_recalc = force_recalc,
        show_message = show_message,
        fun = fun,
        ...
      )
    } else {
      warning(
        "Could not save cache, possibly cannot create directory: ",
        base.dir, " or ", file.path(base_dir, parent_path),
        sep = ""
      )
      # just calculate
      fun(...)
    }
    return(result)
  }
)


#' Calculate/load result and save if necessary
#'
#' This is where the actual work is done
#'
#' @param path path to save cache
#' @param compression compression used in save
#' @param force_recalc force to recalculate cache
#' @param show_message boolean to show messages
#' @param fun function to be called
#' @param ... arguments to said function
#' ,
#' @return result of fun(...)
#'
#' @examples
#' glmSparseNet:::.calculateResult(
#'   file.path(tempdir(), "calculate_result.Rdata"),
#'   "gzip",
#'   FALSE,
#'   TRUE,
#'   sum,
#'   1, 2, 3
#' )
.calculateResult <- function(
    path, compression, force_recalc, show_message, fun, ...) {
  #
  result <- NULL
  if (file.exists(path) && !force_recalc) {
    if (show_message) {
      message("Loading from cache (not calculating):\n  ", path)
    }
    result <- tryCatch(
      {
        tmp_env <- new.env()
        load(path, envir = tmp_env)
        if (
          show_message && is.double(tmp_env$epochMilliseconds)
        ) {
          my_msg <- paste0(
            "Cache was created at ", .POSIXct(tmp_env$epochMilliseconds / 1000)
          )
          if (!is.null(tmp_env$spec) && !is.na(tmp_env$spec["version"])) {
            message(my_msg, " using glmSparseNet v", tmp_env$spec["version"])
          } else {
            message(my_msg, " using glmSparseNet before v1.0.16 or before")
          }
        }
        tmp_env$result
      },
      error = function(err) {
        warning(
          "WARN:: ", err, " -- file: ", path, ".\n  -> Calculating again.\n"
        )
        result_tmp <- fun(...)
        .saveRunCache(result_tmp, path, compression, show_message)
        result_tmp
      }
    )
  } else {
    # calculate function
    result <- fun(...)
    .saveRunCache(result, path, compression, show_message)
  }
  return(result)
}
