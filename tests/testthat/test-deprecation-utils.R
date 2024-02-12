test_that("Deprecated functions", {
  balanced.cv.folds(1:10, nfolds = 10) |> lifecycle::expect_deprecated()
  my.colors() |> lifecycle::expect_deprecated()
  my.symbols() |> lifecycle::expect_deprecated()
  hallmarks() |> lifecycle::expect_defunct()
})

test_that("glmOrphan: deprecated parameters", {
  data <- prepare_mae(10)

  glm_args <- list(
    data$xdata,
    data$ydata,
    "correlation",
    family = "cox",
    network.options = networkOptions(minDegree = .2),
    experiment.name = "RNASeq2GeneNorm"
  )

  list(glmOrphan, glmHub, glmDegree) |>
    lapply(function(fun_name) {
      do.call(fun_name, glm_args) |>
        coef() |>
        nrow() |>
        expect_equal(data$xdata[["RNASeq2GeneNorm"]] |> nrow()) |>
        expect_warning("experiments.* dropped") |>
        expect_message("harmonizing input")
    })
})

test_that("cv.glmOrphan: deprecated parameters", {
  data <- prepare_mae(max_rows = 10)

  glm_args <- list(
    data$xdata,
    data$ydata,
    "correlation",
    family = "cox",
    nfolds = 5,
    network.options = networkOptions(minDegree = .2),
    experiment.name = "RNASeq2GeneNorm"
  )

  list(cv.glmOrphan, cv.glmHub, cv.glmDegree) |>
    lapply(function(fun_name) {
      do.call(fun_name, glm_args) |>
        coef() |>
        nrow() |>
        expect_equal(data$xdata[["RNASeq2GeneNorm"]] |> nrow()) |>
        expect_warning("experiments.* dropped") |>
        expect_message("harmonizing input")
    })
})

test_that("buildLambda: deprecated parameters", {
  buildLambda(lambda.largest = NULL) |>
    lifecycle::expect_deprecated("lambdaLargest")

  buildLambda(lambda.per.order.magnitude = 100) |>
    lifecycle::expect_deprecated("lambdaPerOrderMagnitude")

  buildLambda(orders.of.magnitude.smaller = 3) |>
    lifecycle::expect_deprecated("ordersOfMagnitudeSmaller")
})
