context("Covariance function")

test_that("networkCovParallel: Default methods", {
  withr::local_tempdir(pattern = "base.dir") |> .baseDir()
  xdata <- random_xdata()

  mat_non_diag <- cov(xdata)
  diag(mat_non_diag) <- 0
  test_mat <- networkCovParallel(
    xdata,
    nCores = 1, buildOutput = "matrix",
    #
    forceRecalc = TRUE
  )

  expect_lt(sum(Matrix::norm(test_mat - mat_non_diag, type = "1")), 5e-10)
  #
  test_mat <- networkCovParallel(
    xdata,
    nCores = 1, buildOutput = "matrix",
    #
    forceRecalc = FALSE
  )
  expect_lt(sum(Matrix::norm(test_mat - mat_non_diag, type = "1")), 5e-10)
})

test_that("networkCovParallel: Spearman correlation", {
  withr::local_tempdir(pattern = "base.dir") |> .baseDir()
  xdata <- random_xdata()

  my_method <- "spearman"
  mat_non_diag <- cov(xdata, method = my_method)
  diag(mat_non_diag) <- 0
  test_mat <- networkCovParallel(
    xdata,
    method = my_method, nCores = 1, buildOutput = "matrix",
    #
    forceRecalc = TRUE
  )

  expect_lt(sum(Matrix::norm(test_mat - mat_non_diag, type = "1")), 5e-10)
  #
  test_mat <- networkCovParallel(
    xdata,
    method = my_method, nCores = 1, buildOutput = "matrix",
    #
    forceRecalc = FALSE
  )
  expect_lt(sum(Matrix::norm(test_mat - mat_non_diag, type = "1")), 5e-10)
})

test_that("networkCovParallel: Pearson correlation", {
  withr::local_tempdir(pattern = "base.dir") |> .baseDir()
  xdata <- random_xdata()

  my_method <- "pearson"
  mat_non_diag <- cov(xdata, method = my_method)
  diag(mat_non_diag) <- 0
  #
  test_mat <- networkCovParallel(
    xdata,
    method = my_method, nCores = 1, buildOutput = "matrix",
    #
    forceRecalc = TRUE
  )

  expect_lt(sum(Matrix::norm(test_mat - mat_non_diag, type = "1")), 5e-10)
  #
  test_mat <- networkCovParallel(
    xdata,
    method = my_method, nCores = 1, buildOutput = "matrix",
    #
    forceRecalc = FALSE
  )
  expect_lt(sum(Matrix::norm(test_mat - mat_non_diag, type = "1")), 5e-10)
})

test_that("networkCovParallel: Multiple cores", {
  withr::local_tempdir(pattern = "base.dir") |> .baseDir()
  xdata <- random_xdata()

  my_method <- "pearson"
  mat_non_diag <- cov(xdata, method = my_method)
  diag(mat_non_diag) <- 0
  #
  test_mat <- networkCovParallel(
    xdata,
    method = my_method, nCores = 2, buildOutput = "matrix",
    #
    forceRecalc = TRUE
  )

  expect_lt(sum(Matrix::norm(test_mat - mat_non_diag, type = "1")), 5e-10)
  #
  test_mat <- networkCovParallel(
    xdata,
    method = my_method, nCores = 2, buildOutput = "matrix",
    #
    forceRecalc = FALSE
  )
  expect_lt(sum(Matrix::norm(test_mat - mat_non_diag, type = "1")), 5e-10)
})
