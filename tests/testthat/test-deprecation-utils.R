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

test_that("networkCorParallel: deprecated parameters", {
  xdata <- matrix(rnorm(4), ncol = 2)
  networkCorParallel(xdata, build.output = "matrix") |>
    lifecycle::expect_deprecated("buildOutput")

  # write same expecations to n.cores force.recalc.network and show.message
  networkCorParallel(xdata, n.cores = 2) |>
    lifecycle::expect_deprecated("nCores")
  networkCorParallel(xdata, force.recalc.network = TRUE) |>
    lifecycle::expect_deprecated("forceRecalcNetwork")
  networkCorParallel(xdata, show.message = TRUE) |>
    lifecycle::expect_deprecated("showMessage")
})

test_that("networkCovParallel: deprecated parameters", {
  xdata <- matrix(rnorm(4), ncol = 2)
  networkCovParallel(xdata, build.output = "matrix") |>
    lifecycle::expect_deprecated("buildOutput")

  # write same expecations to n.cores force.recalc.network and show.message
  networkCovParallel(xdata, n.cores = 2) |>
    lifecycle::expect_deprecated("nCores")
  networkCovParallel(xdata, force.recalc.network = TRUE) |>
    lifecycle::expect_deprecated("forceRecalcNetwork")
  networkCovParallel(xdata, show.message = TRUE) |>
    lifecycle::expect_deprecated("showMessage")
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

test_that("separate2GroupsCox: deprecated parameters", {
  data <- prepare_ovarian()
  separate2GroupsCox(
    chosen.btas = c(1, 2), xdata = data$xdata, ydata = data$ydata
  ) |>
    lifecycle::expect_deprecated("chosenBetas")

  separate2GroupsCox(c(1, 2), data$xdata, data$ydata, no.plot = TRUE) |>
    lifecycle::expect_deprecated("noPlot")

  separate2GroupsCox(
    c(1, 2), data$xdata, data$ydata, noPlot = TRUE, expand.yzero = TRUE
  ) |>
    lifecycle::expect_deprecated("expandYZero")

  separate2GroupsCox(
    c(1, 2), data$xdata, data$ydata, noPlot = TRUE, plot.title = "Some title"
  ) |>
    lifecycle::expect_deprecated("plotTitle")

  separate2GroupsCox(
    c(1, 2), data$xdata, data$ydata, noPlot = TRUE, legend.outside = TRUE
  ) |>
    lifecycle::expect_deprecated("legendOutside")

  separate2GroupsCox(
    c(1, 2), data$xdata, data$ydata, noPlot = TRUE, stop.when.overlap = TRUE
  ) |>
    lifecycle::expect_deprecated("stopWhenOverlap")
})
