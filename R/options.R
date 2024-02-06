
#' Change cache.compression for run.cache
#'
#' @param compression see compression parameter in save function
#'
#' @return the new compression
#'
#' @examples
#' glmSparseNet:::.cacheCompression("bzip2")
.cacheCompression <- function(compression = NULL) {
  if (!is.null(compression)) options("glmSparseNet.compression" = compression)
  getOption("glmSparseNet.compression")
}

#' Change base.dir for run.cache
#'
#' @param path to base directory where cache is saved
#'
#' @return the new path
#'
#' @examples
#' glmSparseNet:::.baseDir("/tmp/cache")
.baseDir <- function(path = NULL) {
  if (!is.null(path)) options("glmSparseNet.base_dir" = path)
  getOption("glmSparseNet.base.dir")
}

#' Show messages option in run.cache
#'
#' @param show.message boolean indicating to show messages or not
#'
#' @return the show.message option
#'
#' @examples
#' glmSparseNet:::.showMessage(FALSE)
.showMessage <- function(show.message = NULL) {
  if (!is.null(show.message)) {
    options("glmSparseNet.show_message" = show.message)
  }
  getOption("glmSparseNet.show_message")
}
