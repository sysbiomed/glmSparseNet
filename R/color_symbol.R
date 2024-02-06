#' Custom pallete of colors
#'
#' @param ix index for a color
#'
#' @return a color
#' @export
#'
#' @examples
#' my_colors()
#' my_colors(5)
my_colors <- function(ix = NULL) {
  ret_colors <- c(
    "navyblue", "forestgreen", "tomato4", "tan1",
    "turquoise", "springgreen1", "brown", "violetred1",
    "ivory4", "slateblue1", "chocolate4", "deeppink1",
    "slategray4", "coral3", "darkblue", "mediumorchid1", "black"
  )
  if (is.null(ix)) {
    return(ret_colors)
  }
  if (ix %% length(ret_colors) == 0) {
    ix <- length(ret_colors)
  } else {
    ix <- ix %% length(ret_colors)
  }
  return(ret_colors[ix])
}

#' @rdname my_colors
#' @usage # deprecated, please use my_colors()
#' my.colors(ix = NULL)
#' @export
my.colors <- function(ix = NULL) { # nolint: object_name_linter.
  lifecycle::deprecate_soft("1.21.0", "my.colors()", "my_colors()")
  my_colors(ix)
}

#' Custom pallete of symbols in plots
#'
#' @param ix index for symbol
#'
#' @return a symbol
#' @export
#'
#' @examples
#' my_symbols()
#' my_symbols(2)
my_symbols <- function(ix = NULL) {
  ret_symbols <- c(
    0, 4, 1, 8, 5,
    3, 6, 7, 2,
    9, 10, 11, 12,
    13, 14, 15, 16,
    17
  )
  if (is.null(ix)) {
    return(ret_symbols)
  }
  if (ix %% length(ret_symbols) == 0) {
    ix <- length(ret_symbols)
  } else {
    ix <- ix %% length(ret_symbols)
  }
  return(ret_symbols[ix])
}

#' @rdname my_symbols
#' @usage # deprecated, please use my_symbols()
#' my.symbols(ix = NULL)
#' @export
my.symbols <- function(ix = NULL) { # nolint: object_name_linter.
  lifecycle::deprecate_soft("1.21.0", "my.symbols()", "my_symbols()")
  my_symbols(ix)
}
