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
#' @param baseDir directory where data is stored
#' @param fun function call name
#' @param ... parameters for function call
#' @param seed when function call is random, this allows to set seed beforehand
#' @param cachePrefix prefix for file name to be generated from
#' parameters (...)
#' @param cacheDigest cache of the digest for one or more of the parameters
#' @param showMessage show message that data is being retrieved from cache
#' @param forceRecalc force the recalculation of the values
#' @param addToHash something to add to the filename generation
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
#' # glmSparseNet:::.runCache(c, 1, 2, 3, 4, baseDir = "runcache")
#' }
methods::setGeneric(".runCache", function(
    fun,
    ...,
    seed = NULL,
    baseDir = NULL,
    cachePrefix = "generic_cache",
    cacheDigest = list(),
    showMessage = NULL,
    forceRecalc = FALSE,
    addToHash = NULL) {
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
    digestFun <- if (methods::is(fun, "standardGeneric")) {
        # if it is a generic, then use code for all methods
        methodsFound <- methods::findMethods(fun)
        vapply(
            names(methodsFound),
            function(ix) {
                if (is.null(attributes(methodsFound[[ix]])$srcref)) {
                    return(.digestCache(toString(body(methodsFound[[ix]]))))
                } else {
                    return(
                        .digestCache(
                            toString(attributes(methodsFound[[ix]])$srcref)
                        )
                    )
                }
            },
            character(1L)
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

    return(.digestCache(digestFun))
}

#' Write a file in run-cache directory to explain the origin
#'
#' @param baseDir directory where to build this file
#'
#' @return the path to the file it has written
#'
#' @examples
#'
#' glmSparseNet:::.writeReadme(tempdir())
.writeReadme <- function(baseDir) {
    readmePath <- file.path(baseDir, "what_is_this_folder.txt")

    readmeText <- c(
        "This directory was automatically created in R when function .runCache",
        "was executed (from 'glmSparseNet' package). This might have been done",
        "by you directly or by another function to cache results.",
        "",
        "This folder can be safely deleted as it only contains a cache of the",
        "results of functions",
        "",
        "package link in BioConductor:",
        "https://bioconductor.org/packages/release/bioc/html/glmSparseNet.html",
        "github link: ",
        "   https://github.com/sysbiomed/glmSparseNet",
        "",
        "Have a great day"
    )

    if (!file.exists(readmePath)) {
        tryCatch(
            {
                fileConn <- file(readmePath)
                writeLines(readmeText, con = fileConn)
                close(fileConn)
            },
            error = function(err) {
                # do nothing as an error here should not block the main process
            }
        )
    }
    readmePath
}

#' Create directories for cache
#'
#' @param baseDir tentative base dir to create.
#' @param parentPath first 4 characters of digest that will become parent
#' directory for the actual cache file (this reduces number of files per folder)
#'
#' @return a list of updated baseDir and parentDir
#'
#' @examples
#' glmSparseNet:::.createDirectoryForCache(tempdir(), "abcd")
#' \donttest{
#' glmSparseNet:::.createDirectoryForCache(
#'     file.path(getwd(), "run-cache"), "abcd"
#' )
#' }
.createDirectoryForCache <- function(baseDir, parentPath) {
    checkmate::assert_string(baseDir)
    checkmate::assert_string(parentPath)

    # create the directory to store cache
    dir.create(baseDir, showWarnings = FALSE)

    if (!dir.exists(baseDir)) {
        warning(
            "Could not create cache base folder at ",
            "'", baseDir, "'",
            "... trying to use current working directory"
        )
        baseDir <- getOption("glmSparseNet.base_dir")
        dir.create(baseDir, showWarnings = FALSE)

        if (!dir.exists(baseDir)) {
            baseDir <- file.path(getwd(), "run-cache")
            dir.create(baseDir, showWarnings = FALSE)
        }
    }

    parentDir <- file.path(baseDir, parentPath)
    dir.create(parentDir, showWarnings = FALSE)

    if (!dir.exists(parentDir)) {
        warning(
            "Could not create cache folder inside baseDir at ",
            baseDir,
            ".. trying to use globally defined baseDir or ",
            "if it fails current directory"
        )
        baseDir <- getOption("glmSparseNet.base_dir")
        parentDir <- file.path(baseDir, parentPath)
        dir.create(parentDir, showWarnings = FALSE, recursive = TRUE)

        if (!dir.exists(parentDir)) {
            baseDir <- baseDir <- file.path(getwd(), "run-cache")
            parentDir <- file.path(baseDir, parentPath)
            dir.create(parentDir, showWarnings = FALSE, recursive = TRUE)
        }
    }

    .writeReadme(baseDir)

    list(baseDir = baseDir, parentDir = parentDir)
}

#' Saving the cache
#'
#' @param result main result to save
#' @param path path to the file to save
#' @param compression compression method to be used
#' @param showMessage TRUE to show messages, FALSE otherwise
#'
#' @return result of save operation
#'
#' @examples
#' glmSparseNet:::.saveRunCache(
#'     35, file.path(tempdir(), "save_run_cache.Rdata"), FALSE, TRUE
#' )
.saveRunCache <- function(result, path, compression, showMessage) {
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
            if (showMessage) message("Saving in cache:  ", path)
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
                "Problem when saving cache. Attempting to deliver results...",
                "\n\n  What happened: ",
                err
            )
            NULL
        }
    )
}

#' @describeIn dot-runCache accepts function as first argument and save cache
methods::setMethod(
    ".runCache",
    signature("function"),
    function(fun,
             ...,
             # run_cache options
             seed = NULL,
             baseDir = NULL,
             cachePrefix = "generic_cache",
             cacheDigest = list(),
             showMessage = NULL,
             forceRecalc = FALSE,
             addToHash = NULL) {
        #
        # baseDir
        if (is.null(baseDir)) {
            baseDir <- .baseDir()
        }
        if (is.null(showMessage)) {
            showMessage <- .showMessage()
        }
        compression <- .cacheCompression()

        #
        args <- rlang::list2(...)
        if (!is.null(seed)) {
            args[["run_cache.seed"]] <- seed
            do.call(set.seed, list(seed))
        }
        if (!is.null(addToHash)) {
            args[["run_cache.addToHash"]] <- addToHash
        }

        # Build digest of each of the arguments
        args <- lapply(seq_along(args), function(ix) {
            if (length(cacheDigest) >= ix && !is.null(cacheDigest[[ix]])) {
                return(cacheDigest[[ix]])
            }
            .digestCache(args[[ix]])
        })

        # Build digest of the function's code
        #  (if it changes, then cache is invalidated)
        args[["run_cache.fun"]] <- .buildFunctionDigest(fun)

        # digest all the arguments together
        myDigest <- .digestCache(args)

        filename <- sprintf("cache-%s-H_%s.RData", cachePrefix, myDigest)
        parentPath <- strtrim(myDigest, width = 4)

        # create dir and update baseDir (in case it failed)
        cacheDirPaths <- .createDirectoryForCache(baseDir, parentPath)
        parentDir <- cacheDirPaths$parentDir
        baseDir <- cacheDirPaths$baseDir

        # Calculate
        result <- if (dir.exists(parentDir)) {
            path <- file.path(baseDir, parentPath, filename)
            .calculateResult(
                path = path,
                compression = compression,
                forceRecalc = forceRecalc,
                showMessage = showMessage,
                fun = fun,
                ...
            )
        } else {
            warning(
                "Could not save cache, possibly cannot create directory: ",
                baseDir, " or ", file.path(baseDir, parentPath),
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
#' @param forceRecalc force to recalculate cache
#' @param showMessage boolean to show messages
#' @param fun function to be called
#' @param ... arguments to said function
#' ,
#' @return result of fun(...)
#'
#' @examples
#' glmSparseNet:::.calculateResult(
#'     file.path(tempdir(), "calculate_result.Rdata"),
#'     "gzip",
#'     FALSE,
#'     TRUE,
#'     sum,
#'     1, 2, 3
#' )
.calculateResult <- function(
    path,
    compression,
    forceRecalc,
    showMessage,
    fun,
    ...) {
    #
    result <- NULL
    if (file.exists(path) && !forceRecalc) {
        if (showMessage) {
            message("Loading from cache (not calculating):\n  ", path)
        }
        result <- tryCatch(
            {
                tmpEnv <- new.env()
                load(path, envir = tmpEnv)
                if (
                    showMessage && is.double(tmpEnv$epochMilliseconds)
                ) {
                    myMsg <- paste0(
                        "Cache was created at ",
                        .POSIXct(tmpEnv$epochMilliseconds / 1000)
                    )
                    if (
                        !is.null(tmpEnv$spec) &&
                            !is.na(tmpEnv$spec["version"])
                    ) {
                        message(
                            myMsg,
                            " using glmSparseNet v", tmpEnv$spec["version"]
                        )
                    } else {
                        message(
                            myMsg,
                            " using glmSparseNet before v1.0.16 or before"
                        )
                    }
                }
                tmpEnv$result
            },
            error = function(err) {
                warning(
                    err,
                    " -- file: ",
                    path, ".\n  -> Calculating again.\n"
                )
                resultTmp <- fun(...)
                .saveRunCache(resultTmp, path, compression, showMessage)
                resultTmp
            }
        )
    } else {
        # calculate function
        result <- fun(...)
        .saveRunCache(result, path, compression, showMessage)
    }
    return(result)
}
