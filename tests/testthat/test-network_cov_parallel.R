context("Covariance function")

test_that("networkCovParallel: Default methods", {
  withr::local_tempdir(pattern = "base.dir") |> .baseDir()
  xdata <- random_xdata()

  mat_non_diag <- cov(xdata)
  diag(mat_non_diag) <- 0
  test_mat <- networkCovParallel(xdata,
    n.cores = 1, build.output = "matrix",
    #
    force.recalc = TRUE
  )

  expect_lt(sum(Matrix::norm(test_mat - mat_non_diag, type = "1")), 5e-10)
  #
  test_mat <- networkCovParallel(xdata,
    n.cores = 1, build.output = "matrix",
    #
    force.recalc = FALSE
  )
  expect_lt(sum(Matrix::norm(test_mat - mat_non_diag, type = "1")), 5e-10)
})

test_that("networkCovParallel: Spearman correlation", {
  withr::local_tempdir(pattern = "base.dir") |> .baseDir()
  xdata <- random_xdata()

  my_method <- "spearman"
  mat_non_diag <- cov(xdata, method = my_method)
  diag(mat_non_diag) <- 0
  test_mat <- networkCovParallel(xdata,
    method = my_method, n.cores = 1, build.output = "matrix",
    #
    force.recalc = TRUE
  )

  expect_lt(sum(Matrix::norm(test_mat - mat_non_diag, type = "1")), 5e-10)
  #
  test_mat <- networkCovParallel(xdata,
    method = my_method, n.cores = 1, build.output = "matrix",
    #
    force.recalc = FALSE
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
  test_mat <- networkCovParallel(xdata,
    method = my_method, n.cores = 1, build.output = "matrix",
    #
    force.recalc = TRUE
  )

  expect_lt(sum(Matrix::norm(test_mat - mat_non_diag, type = "1")), 5e-10)
  #
  test_mat <- networkCovParallel(xdata,
    method = my_method, n.cores = 1, build.output = "matrix",
    #
    force.recalc = FALSE
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
  test_mat <- networkCovParallel(xdata,
    method = my_method, n.cores = 2, build.output = "matrix",
    #
    force.recalc = TRUE
  )

  expect_lt(sum(Matrix::norm(test_mat - mat_non_diag, type = "1")), 5e-10)
  #
  test_mat <- networkCovParallel(xdata,
    method = my_method, n.cores = 2, build.output = "matrix",
    #
    force.recalc = FALSE
  )
  expect_lt(sum(Matrix::norm(test_mat - mat_non_diag, type = "1")), 5e-10)
})

test_that("networkCovParallel: with platform windows", {
  withr::local_tempdir(pattern = "base.dir") |> .baseDir()
  xdata <- random_xdata()

  local({
    .Platform$OS.type <- "windows"

    mat_non_diag <- cov(xdata)
    diag(mat_non_diag) <- 0
    test_mat <- networkCovParallel(
      xdata,
      n.cores = 1, build.output = "matrix",
      #
      force.recalc = TRUE
    )

    expect_lt(sum(Matrix::norm(test_mat - mat_non_diag, type = "1")), 5e-10)
    #
    test_mat <- networkCovParallel(
      xdata,
      n.cores = 1, build.output = "matrix",
      #
      force.recalc = FALSE
    )
    expect_lt(sum(Matrix::norm(test_mat - mat_non_diag, type = "1")), 5e-10)
  })
})
