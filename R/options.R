#' Change cache.compression for run_cache
#'
#' @param compression see compression parameter in save function
#'
#' @return the new compression
#'
#' @examples
#' glmSparseNet:::.cacheCompression("bzip2")
.cacheCompression <- function(compression = NULL) {
    checkmate::assert_string(compression, null.ok = TRUE)
    if (!is.null(compression)) options("glmSparseNet.compression" = compression)
    getOption("glmSparseNet.compression")
}

#' Change base dir for `.runCache
#'
#' @param path to base directory where cache is saved
#'
#' @return the new path
#'
#' @examples
#' glmSparseNet:::.baseDir("/tmp/cache")
.baseDir <- function(path = NULL) {
    checkmate::assert_string(path, null.ok = TRUE)
    if (!is.null(path)) options("glmSparseNet.base_dir" = path)
    getOption("glmSparseNet.base_dir")
}

#' Show messages option in .runCache
#'
#' @param showMessage boolean indicating to show messages or not
#'
#' @return the show.message option
#'
#' @examples
#' glmSparseNet:::.showMessage(FALSE)
.showMessage <- function(showMessage = NULL) {
    checkmate::assert_logical(showMessage, null.ok = TRUE)
    if (!is.null(showMessage)) {
        options("glmSparseNet.show_message" = showMessage)
    }
    getOption("glmSparseNet.show_message")
}
