#' Create balanced folds for cross validation using stratified sampling
#'
#' @param ... vectors representing data
#' @param nfolds number of folds to be created
#'
#' @return list with given input, nfolds and result. The result is a list
#' matching the input with foldid attributed to each position.
#'
#' @examples
#' balancedCvFolds(seq(10), seq(11, 15), nfolds = 2)
#'
#' # will give a warning
#' balancedCvFolds(seq(10), seq(11, 13), nfolds = 10)
#'
#' balancedCvFolds(seq(100), seq(101, 133), nfolds = 10)
#' @export
balancedCvFolds <- function(..., nfolds = 10) {
    inputList <- rlang::list2(...)
    outputList <- list()
    if (
        any(vapply(inputList, function(vec) length(vec) < nfolds, logical(1L)))
    ) {
        warning(
            "Number of elements in vector (",
            length(unlist(inputList)),
            ") is less than 'nfolds' (",
            nfolds,
            ")"
        )
    }
    for (mySet in inputList) {
        #
        # count previous bins and order sequence on increasing count
        if (length(outputList) == 0) {
            mySample <- rep(seq(nfolds), length = length(mySet))
        } else {
            myTmp <- c()
            for (ix in seq(outputList)) {
                myTmp <- c(myTmp, outputList[[ix]])
            }
            myCount <- graphics::hist(
                myTmp,
                plot = FALSE, breaks = 0:nfolds
            )$counts
            mySample <- rep(
                seq(nfolds)[sort(myCount, index.return = TRUE)$ix],
                length = length(mySet)
            )
        }
        #
        outputList <- c(outputList, list(sample(mySample)))
    }
    if (length(outputList) == 1L) {
        outputList <- outputList[[1L]]
        inputList <- inputList[[1L]]
    }
    return(list(input = inputList, output = outputList, nfolds = nfolds))
}
