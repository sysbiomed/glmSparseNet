context("separate2GroupsCox")

test_that("Calculates kaplan-meier data", {
  ovarian <- survival::ovarian
  result <- separate2GroupsCox(
    c(age = 1),
    ovarian$age,
    data.frame(time = ovarian$futime, status = ovarian$fustat)
  )

  expect_lt(result$pvalue - 0.0518148, 1e-2)
})

test_that("Only one group in kaplan.meier gives error", {
  ovarian <- survival::ovarian
  expect_warning(expect_error(separate2GroupsCox(
    c(age = 0),
    ovarian$age,
    data.frame(time = ovarian$futime, status = ovarian$fustat)
  )))
})

test_that("All combinations of parameters possible for draw.kapan", {
  ovarian <- survival::ovarian
  xdata <- ovarian[, c("age", "resid.ds")]
  ydata <- data.frame(time = ovarian$futime, status = ovarian$fustat)

  # list, data.frame, data.frame
  separate2GroupsCox(list(c(1, 0)), xdata, ydata) |>
    expect_silent()
  # list, matrix, data.frame
  separate2GroupsCox(list(c(1, 0)), Matrix::as.matrix(xdata), ydata) |>
    expect_silent()

  # list, numeric, data.frame
  separate2GroupsCox(list(c(1)), xdata$age, ydata) |>
    expect_silent()
  # list, numeric, data.frame
  separate2GroupsCox(1, xdata$age, ydata) |>
    expect_silent()

  # numeric, data.frame, data.frame
  separate2GroupsCox(c(1, 0), xdata, ydata) |>
    expect_silent()
  # numeric, matrix, data.frame
  separate2GroupsCox(c(1, 0), Matrix::as.matrix(xdata), ydata) |>
    expect_silent()
})

test_that("Some bad arguments for separate2GroupsCox", {
  ovarian <- survival::ovarian
  xdata <- survival::ovarian[, c("age", "resid.ds")]
  ydata <- data.frame(time = ovarian$futime, status = ovarian$fustat)

  # list, data.frame, data.frame
  separate2GroupsCox(list(c(1, 0), c(0, 1, 2)), xdata, ydata) |>
    expect_error("All or some of the.*have different number")
  # list, matrix, data.frame
  separate2GroupsCox(list(c(1, 0)), Matrix::as.matrix(xdata), ydata[1:10, ]) |>
    expect_error("Rows in xdata .* and ydata .* must be the same")

  # list, numeric, data.frame
  separate2GroupsCox(list(c(1, 0)), xdata$age, ydata) |>
    expect_error()
  # list, numeric, data.frame
  separate2GroupsCox(c(1, 2), xdata$age, ydata) |>
    expect_error()

  # numeric, data.frame, data.frame
  separate2GroupsCox(c(1, 0, 1), xdata, ydata) |>
    expect_error()
  # numeric, matrix, data.frame
  separate2GroupsCox(c(1, 0), t(Matrix::as.matrix(xdata)), ydata) |>
    expect_error()
})

test_that("separate2GroupsCox: throws cuttof error when overlapping prob.", {
  ovarian <- survival::ovarian
  xdata <- survival::ovarian[, c("age", "resid.ds")]
  ydata <- data.frame(time = ovarian$futime, status = ovarian$fustat)

  separate2GroupsCox(c(1, 0), xdata, ydata, probs = c(.55, .45)) |>
    expect_error("The cutoff values")
})

test_that("separate2GroupsCox: throws warning when overlapping prob.", {
  ovarian <- survival::ovarian
  xdata <- survival::ovarian[, c("age", "resid.ds")]
  ydata <- data.frame(time = ovarian$futime, status = ovarian$fustat)

  separate2GroupsCox(
    c(1, 0), xdata, ydata, probs = c(.55, .45), stop.when.overlap = FALSE
  ) |>
    expect_warning("The cutoff values")
})

test_that("separate2GroupsCox: no plot is returned when argument is given", {
  ovarian <- survival::ovarian
  xdata <- survival::ovarian[, c("age", "resid.ds")]
  ydata <- data.frame(time = ovarian$futime, status = ovarian$fustat)

  separate2GroupsCox(
    c(1, 0), xdata, ydata, noPlot = TRUE
  ) |>
    magrittr::extract2("plot") |>
    expect_null()
})

test_that("separate2GroupsCox: kaplan-meier should have 4 groups", {
  ovarian <- survival::ovarian
  xdata <- survival::ovarian[, c("age", "resid.ds")]
  ydata <- data.frame(time = ovarian$futime, status = ovarian$fustat)

  separate2GroupsCox(list(yada = c(1, 0), bla = c(0, 1)), xdata, ydata) |>
    magrittr::extract2("km") |>
    magrittr::extract2("strata") |>
    expect_length(4)
})

test_that("separate2GroupsCox: kaplan-meier should have 4 groups", {
  ovarian <- survival::ovarian
  xdata <- survival::ovarian[, c("age", "resid.ds")]
  ydata <- data.frame(time = ovarian$futime, status = ovarian$fustat)

  separate2GroupsCox(list(c(1, 0), c(0, 1)), xdata, ydata) |>
    magrittr::extract2("km") |>
    magrittr::extract2("strata") |>
    expect_length(4)
})

