#' @noRd
#' @keywords internal
#' @examples
#' .deprecatedDotParam("1.21", "test_me()", "n.folds")
#'
.deprecatedDotParam <- function(fun_name, argument_name, version = "1.21.0") {
  new_argument <- gsub("\\.(\\w)", "\\U\\1", argument_name, perl = TRUE)
  lifecycle::deprecate_warn(
    version,
    paste0(fun_name, "(", argument_name, " = )"),
    paste0(fun_name, "(", new_argument, " = )")
  )
}

#' @rdname balancedCvFolds
#' @usage # deprecated, please use balancedCvFolds()
#' balanced.cv.folds(..., nfolds = 10)
balanced.cv.folds <- function(..., nfolds = 10) {
  lifecycle::deprecate_soft("1.21.0", "balanced.cv.folds()", "balanced_cv_folds()")
  balancedCvFolds(..., nfolds = nfolds)
}

#' @rdname myColors
#' @usage # deprecated, please use my_colors()
#' my.colors(ix = NULL)
#' @export
my.colors <- function(ix = NULL) { # nolint: object_name_linter.
  lifecycle::deprecate_soft("1.21.0", "my.colors()", "myColors()")
  my_colors(ix)
}

#' @rdname mySymbols
#' @usage # deprecated, please use mySymbols()
#' my.symbols(ix = NULL)
#' @export
my.symbols <- function(ix = NULL) { # nolint: object_name_linter.
  lifecycle::deprecate_soft("1.21.0", "my.symbols()", "mySymbols()")
  my_symbols(ix)
}

#' Retrieve hallmarks of cancer count for genes
#'
#' `r lifecycle::badge("defunct")`
#' The API has been removed and this function is no longer available.
#'
#' @param genes gene names
#' @param metric see below
#' @param hierarchy see below
#' @param generate.plot flag to indicate if return object has a ggplot2 object
#' @param show.message flag to indicate if run.cache method shows messages
#'
#' @return data.frame with choosen metric and hierarchy
#' It also returns a vector with genes that do not have any
#' hallmarks.
#'
#' See http://chat.lionproject.net/api for more details on the
#' metric and hallmarks parameters
#'
#' To standardize the colors in the gradient you can use
#' scale_fill_gradientn(limits=c(0,1), colours=topo.colors(3)) to
#' limit between 0 and 1 for cprob and -1 and 1 for npmi
#'
#' @export
#'
#' @examples
#' hallmarks(c("MOB1A", "RFLNB", "SPIC"))
#' \donttest{
#' hallmarks(c("MOB1A", "RFLNB", "SPIC"), metric = "cprob")
#' }
hallmarks <- function(
    genes,
    metric = "count",
    hierarchy = "full",
    generate.plot = TRUE,
    show.message = FALSE) {
  lifecycle::deprecate_stop(
    "1.21.0", "hallmarks()",
    details = "API is no longer available"
  )
}
