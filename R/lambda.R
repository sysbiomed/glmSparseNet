#' Auxiliary function to generate suitable lambda parameters
#'
#' @param lambda.largest numeric value for largest number of lambda to consider
#' (usually with a target of 1 selected variable)
#' @param xdata X parameter for glmnet function
#' @param ydata Y parameter for glmnet function
#' @param family family parameter to glmnet function
#' @param orders.of.magnitude.smaller minimum value for lambda
#' (lambda.largest / 10^orders.of.magnitude.smaller)
#' @param lambda.per.order.magnitude how many lambdas to create for each order
#' of magnitude
#'
#' @return a numeric vector with suitable lambdas
#' @export
#'
#' @examples
#' buildLambda(5.4)
buildLambda <- function(lambda.largest = NULL,
                        xdata = NULL, ydata = NULL, family = NULL,
                        orders.of.magnitude.smaller = 3,
                        lambda.per.order.magnitude = 150) {
  if (!is.null(lambda.largest)) {
    lambda.first <- lambda.largest
  } else if (!is.null(xdata) && !is.null(ydata) && !is.null(family)) {
    fitted <- glmnet(xdata, ydata, family = family)
    lambda.first <- fitted$lambda[1]
  }

  lambda.nrow <- lambda.per.order.magnitude
  lambda.ncol <- orders.of.magnitude.smaller

  lambda <- (
    matrix(
      rep(1 / 10^seq(0, lambda.ncol - 1, 1), lambda.nrow),
      nrow = lambda.nrow, byrow = TRUE
    ) *
      array(
        lambda.first * seq(1 / lambda.nrow, 1, 1 / lambda.nrow),
        dim = c(lambda.nrow, lambda.ncol)
      )
  ) |>
    as.vector() |>
    unique() |>
    sort(decreasing = TRUE)

  return(lambda)
}
