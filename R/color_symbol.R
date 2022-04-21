#' Custom pallete of colors
#'
#' @param ix index for a color
#'
#' @return a color
#' @export
#'
#' @examples
#' my.colors()
#' my.colors(5)
my.colors <- function(ix = NULL){
  ret.colors <- c(
    'navyblue',  'forestgreen',  'tomato4',    'tan1',
    'turquoise', 'springgreen1', 'brown',      'violetred1',
    'ivory4',    'slateblue1',   'chocolate4', 'deeppink1',
    'slategray4','coral3',       'darkblue',   'mediumorchid1', 'black'
  )
  if (is.null(ix)) {
    return(ret.colors)
  }
  if (ix %% length(ret.colors) == 0) {
    ix <- length(ret.colors)
  }
  else {
    ix <- ix %% length(ret.colors)
  }
  return(ret.colors[ix])
}

#' Custom pallete of symbols in plots
#'
#' @param ix index for symbol
#'
#' @return a symbol
#' @export
#'
#' @examples
#' my.symbols()
#' my.symbols(2)
my.symbols <- function(ix = NULL) {
  ret.symbols <- c(0,4,1,8,5,
                   3,6,7,2,
                   9,10,11,12,
                   13,14,15,16,
                   17)
  if (is.null(ix)) {
    return(ret.symbols)
  }
  if (ix %% length(ret.symbols) == 0) {
    ix <- length(ret.symbols)
  } else {
    ix <- ix %% length(ret.symbols)
  }
  return(ret.symbols[ix])
}
