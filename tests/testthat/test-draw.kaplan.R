context("separate2GroupsCox")

library(survival)

test_that("Calculates kaplan-meier data", {
  result <- separate2GroupsCox(
    c(age = 1),
    ovarian$age,
    data.frame(time = ovarian$futime, status = ovarian$fustat)
  )

  expect_lt(result$pvalue - 0.0518148, 1e-2)
})

test_that("Only one group in kaplan.meier gives error", {
  expect_warning(expect_error(separate2GroupsCox(
    c(age = 0),
    ovarian$age,
    data.frame(time = ovarian$futime, status = ovarian$fustat)
  )))
})

test_that("All combinations of parameters possible for draw.kapan", {
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
  xdata <- ovarian[, c("age", "resid.ds")]
  ydata <- data.frame(time = ovarian$futime, status = ovarian$fustat)

  # list, data.frame, data.frame
  expect_error(separate2GroupsCox(list(c(1, 0), c(0, 1, 2)), xdata, ydata))
  # list, matrix, data.frame
  expect_error(
    separate2GroupsCox(list(c(1, 0)), Matrix::as.matrix(xdata), ydata[1:10, ])
  )

  # list, numeric, data.frame
  expect_error(separate2GroupsCox(list(c(1, 0)), xdata$age, ydata))
  # list, numeric, data.frame
  expect_error(separate2GroupsCox(c(1, 2), xdata$age, ydata))

  # numeric, data.frame, data.frame
  expect_error(separate2GroupsCox(c(1, 0, 1), xdata, ydata))
  # numeric, matrix, data.frame
  expect_error(separate2GroupsCox(c(1, 0), t(Matrix::as.matrix(xdata)), ydata))
})
