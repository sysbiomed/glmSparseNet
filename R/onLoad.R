#' Internal list with all the supported options of glmSparseNet with defaults
#' @noRd
.optionsList <- list(
    "glmSparseNet.compression" = getOption("glmSparseNet.compression", "gzip"),
    "glmSparseNet.base_dir" = getOption(
        "glmSparseNet.base_dir", .tempdirCache()
    ),
    "glmSparseNet.show_message" = getOption("glmSparseNet.show_essage", TRUE)
)

.onLoad <- function(libname, pkgname) {
    op <- options()
    toset <- !(names(.optionsList) %in% names(op))
    if (any(toset)) options(.optionsList[toset])

    invisible()
}
