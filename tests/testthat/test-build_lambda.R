test_that("buildLambda: returns an empty vector without arguments", {
  buildLambda() |>
    checkmate::expect_double() |>
    expect_length(0)
})

test_that("buildLambda: returns 1 value per order of magnitude", {

  buildLambda(1, ordersOfMagnitudeSmaller = 5, lambdaPerOrderMagnitude = 1) |>
    expect_equal(c(1, .1, .01, .001, .0001))
})

test_that("buildLambda: uses same initial lambda as glmnet", {
  xdata <- matrix(rnorm(100), ncol = 5)
  ydata <- rnorm(nrow(xdata))

  res <- glmnet::glmnet(xdata, ydata, family = "gaussian")

  buildLambda(xdata = xdata, ydata = ydata, family = "gaussian") |>
    magrittr::extract(1) |>
    expect_equal(res$lambda[1])
})
