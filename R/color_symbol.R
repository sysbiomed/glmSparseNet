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
    retColors <- c(
        "navyblue", "forestgreen", "tomato4", "tan1",
        "turquoise", "springgreen1", "brown", "violetred1",
        "ivory4", "slateblue1", "chocolate4", "deeppink1",
        "slategray4", "coral3", "darkblue", "mediumorchid1", "black"
    )
    if (is.null(ix)) {
        return(retColors)
    }
    if (ix %% length(retColors) == 0) {
        ix <- length(retColors)
    } else {
        ix <- ix %% length(retColors)
    }
    return(retColors[ix])
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
    retSymbols <- c(
        0, 4, 1, 8, 5,
        3, 6, 7, 2,
        9, 10, 11, 12,
        13, 14, 15, 16,
        17
    )
    if (is.null(ix)) {
        return(retSymbols)
    }
    if (ix %% length(retSymbols) == 0) {
        ix <- length(retSymbols)
    } else {
        ix <- ix %% length(retSymbols)
    }
    return(retSymbols[ix])
}
