#' Custom pallete of colors
#'
#' @param ix index for a color
#'
#' @return a color
#' @export
#'
#' @examples
#' myColors()
#' myColors(5)
myColors <- function(ix = NULL) {
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

#' Custom pallete of symbols in plots
#'
#' @param ix index for symbol
#'
#' @return a symbol
#' @export
#'
#' @examples
#' mySymbols()
#' mySymbols(2)
mySymbols <- function(ix = NULL) {
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
