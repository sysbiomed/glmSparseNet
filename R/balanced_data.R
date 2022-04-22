
#' Create balanced folds for cross validation
#'
#' @param ... vectors representing data
#' @param nfolds number of folds to be created
#'
#' @return list with given input, nfolds and result. The result is a list
#' matching the input with foldid attributed to each position.
#'
#' @examples
#' glmSparseNet:::balanced.cv.folds(seq(10), seq(11, 15), nfolds = 2)
#' # will give a warning
#' glmSparseNet:::balanced.cv.folds(seq(10), seq(11, 13), nfolds = 10) 
#' glmSparseNet:::balanced.cv.folds(seq(100), seq(101, 133), nfolds = 10)
balanced.cv.folds <- function(..., nfolds = 10) {
  input.list <- list(...)
  output.list <- list()
  if (any(vapply(input.list, function(vec) {length(vec) < nfolds}, TRUE))) {
    warning(
      'Number of elements in vector (',
      length(unlist(input.list)),
      ') is less than \'nfolds\' (',
      nfolds,
      ')'
    )
  }
  for (my.set in input.list) {
    #
    # count previous bins and order sequence on increasing count
    if (length(output.list) == 0) {
      my.sample <- rep(seq(nfolds),length = length(my.set))
    } else {
      my.tmp <- c()
      for(ix in seq(output.list)) {
        my.tmp <- c(my.tmp, output.list[[ix]])
      }
      my.count <- graphics::hist(my.tmp, plot = FALSE, breaks = 0:nfolds)$counts
      my.sample <- rep(
        seq(nfolds)[sort(my.count, index.return = TRUE)$ix],
        length = length(my.set)
      )
    }
    #
    output.list <- c(output.list, list(sample(my.sample)))
  }
  if (length(output.list) == 1) {
    output.list = output.list[[1]]
    input.list = input.list[[1]]
  }
  return(list(input = input.list, output = output.list, nfolds = nfolds))
}





