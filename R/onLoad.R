#' Internal list with all the supported options of glmSparseNet with defaults
#'
#' @keywords internal
.optionsList <- list(
  glmSparseNet.compression = getOption("glmSparseNet.compression", "gzip"),
  glmSparseNet.base_dir = getOption("glmSparseNet.base_dir", tempdir.cache()),
  glmSparseNet.show_message = getOption("glmSparseNet.show_message", TRUE)
)

.onLoad <- function(libname, pkgname) {
  op <- options()
  toset <- !(names(.optionsList) %in% names(op))
  if (any(toset)) options(.optionsList[toset])

  invisible()
}
