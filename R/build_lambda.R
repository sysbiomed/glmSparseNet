#' Auxiliary function to generate suitable lambda parameters
#'
#' @param lambdaLargest numeric value for largest number of lambda to consider
#' (usually with a target of 1 selected variable)
#' @param xdata X parameter for glmnet function
#' @param ydata Y parameter for glmnet function
#' @param family family parameter to glmnet function
#' @param ordersOfMagnitudeSmaller minimum value for lambda
#' (lambda.largest / 10^orders.of.magnitude.smaller)
#' @param lambdaPerOrderMagnitude how many lambdas to create for each order
#' of magnitude
#' @param lambda.largest `r lifecycle::badge("deprecated")`
#' @param orders.of.magnitude.smaller `r lifecycle::badge("deprecated")`
#' @param lambda.per.order.magnitude `r lifecycle::badge("deprecated")`
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
        # nolint start: object_name_linter.
        lambda.largest = deprecated(),
        orders.of.magnitude.smaller = deprecated(),
        lambda.per.order.magnitude = deprecated()) {
    # nolint end: object_name_linter.
    # Lifecycle management: to remove after 1.23.0
    if (lifecycle::is_present(lambda.largest)) {
        .deprecatedDotParam("buildLambda", "lambda.largest")
        lambdaLargest <- lambda.largest
    }
    if (lifecycle::is_present(orders.of.magnitude.smaller)) {
        .deprecatedDotParam("buildLambda", "orders.of.magnitude.smaller")
        ordersOfMagnitudeSmaller <- orders.of.magnitude.smaller
    }
    if (lifecycle::is_present(lambda.per.order.magnitude)) {
        .deprecatedDotParam("buildLambda", "lambda.per.order.magnitude")
        lambdaPerOrderMagnitude <- lambda.per.order.magnitude
    }
    # Lifecycle management: end

    lambdaFirst <- if (!is.null(lambdaLargest)) {
        lambdaLargest
    } else if (!is.null(xdata) && !is.null(ydata) && !is.null(family)) {
        fitted <- glmnet::glmnet(xdata, ydata, family = family)
        fitted$lambda[1]
    }

    lambdaNRow <- lambdaPerOrderMagnitude
    lambdaNCol <- ordersOfMagnitudeSmaller

    lambda <- (
        matrix(
            rep(1 / 10^seq(0, lambdaNCol - 1, 1), lambdaNRow),
            nrow = lambdaNRow, byrow = TRUE
        ) *
            array(
                lambdaFirst * seq(1 / lambdaNRow, 1, 1 / lambdaNRow),
                dim = c(lambdaNRow, lambdaNCol)
            )
    ) |>
        as.vector() |>
        unique() |>
        sort(decreasing = TRUE)

    return(lambda)
}
