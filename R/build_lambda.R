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
buildLambda <- function(
    lambdaLargest = NULL,
    xdata = NULL,
    ydata = NULL,
    family = NULL,
    ordersOfMagnitudeSmaller = 3,
    lambdaPerOrderMagnitude = 150,
    # Deprecated arguments with dots in name
    lambda.largest = deprecated(),
    orders.of.magnitude.smaller = deprecated(),
    lambda.per.order.magnitude = deprecated()) {
  if (lifecycle::is_present(lambda.largest)) {
    .deprecated_dot_param("buildLambda", "lambda.largest")
    lambda_largest <- lambda.largest
  }
  if (lifecycle::is_present(orders.of.magnitude.smaller)) {
    .deprecated_dot_param("buildLambda", "orders.of.magnitude.smaller")
    orders_of_magnitude_smaller <- orders.of.magnitude.smaller
  }
  if (lifecycle::is_present(lambda.per.order.magnitude)) {
    .deprecated_dot_param("buildLambda", "lambda.per.order.magnitude")
    lambda_per_order_magnitude <- lambda.per.order.magnitude
  }

  if (!is.null(lambda_largest)) {
    lambda_first <- lambda_largest
  } else if (!is.null(xdata) && !is.null(ydata) && !is.null(family)) {
    fitted <- glmnet::glmnet(xdata, ydata, family = family)
    lambda_first <- fitted$lambda[1]
  }

  lambda_nrow <- lambda_per_order_magnitude
  lambda_ncol <- orders_of_magnitude_smaller

  lambda <- (
    matrix(
      rep(1 / 10^seq(0, lambda_ncol - 1, 1), lambda_nrow),
      nrow = lambda_nrow, byrow = TRUE
    ) *
      array(
        lambda_first * seq(1 / lambda_nrow, 1, 1 / lambda_nrow),
        dim = c(lambda_nrow, lambda_ncol)
      )
  ) |>
    as.vector() |>
    unique() |>
    sort(decreasing = TRUE)

  return(lambda)
}
