context("Correlation matrix")

test_that("networkCorParallel: Default methods", {
  withr::local_tempdir(pattern = "base.dir") |> .baseDir()
  xdata <- random_xdata()

  mat_non_diag <- cor(xdata)
  diag(mat_non_diag) <- 0
  test_mat <- networkCorParallel(xdata,
    n.cores = 1, build.output = "matrix",
    #
    force.recalc = TRUE
  )

  expect_lt(sum(Matrix::norm(test_mat - mat_non_diag, type = "1")), 5e-10)
  #
  test_mat <- networkCorParallel(xdata,
    n.cores = 1, build.output = "matrix",
    #
    force.recalc = FALSE
  )
  expect_lt(sum(Matrix::norm(test_mat - mat_non_diag, type = "1")), 5e-10)
})

test_that("networkCorParallel: Spearman correlation", {
  withr::local_tempdir(pattern = "base.dir") |> .baseDir()
  xdata <- random_xdata()

  my_method <- "spearman"
  mat_non_diag <- cor(xdata, method = my_method)
  diag(mat_non_diag) <- 0
  test_mat <- networkCorParallel(xdata,
    method = my_method, n.cores = 1, build.output = "matrix",
    #
    force.recalc = TRUE
  )

  expect_lt(sum(Matrix::norm(test_mat - mat_non_diag, type = "1")), 5e-10)
  #
  test_mat <- networkCorParallel(xdata,
    method = my_method, n.cores = 1, build.output = "matrix",
    #
    force.recalc = FALSE
  )
  expect_lt(sum(Matrix::norm(test_mat - mat_non_diag, type = "1")), 5e-10)
})

test_that("networkCorParallel: Pearson correlation", {
  withr::local_tempdir(pattern = "base.dir") |> .baseDir()
  xdata <- random_xdata()

  my_method <- "pearson"
  mat_non_diag <- cor(xdata, method = my_method)
  diag(mat_non_diag) <- 0
  #
  test_mat <- networkCorParallel(xdata,
    method = my_method, n.cores = 1, build.output = "matrix",
    #
    force.recalc = TRUE
  )

  expect_lt(sum(Matrix::norm(test_mat - mat_non_diag, type = "1")), 5e-10)
  #
  test_mat <- networkCorParallel(xdata,
    method = my_method, n.cores = 1,
    build.output = "matrix",
    #
    force.recalc = FALSE
  )
  expect_lt(sum(Matrix::norm(test_mat - mat_non_diag, type = "1")), 5e-10)
})

test_that("networkCorParallel: Multiple cores", {
  withr::local_tempdir(pattern = "base.dir") |> .baseDir()

  xdata <- random_xdata()
  my_method <- "pearson"
  mat_non_diag <- cor(xdata, method = my_method)
  diag(mat_non_diag) <- 0
  #
  test_mat <- networkCorParallel(
    xdata,
    method = my_method, n.cores = 2,
    build.output = "matrix",
    #
    force.recalc = TRUE
  )

  expect_lt(sum(Matrix::norm(test_mat - mat_non_diag, type = "1")), 5e-10)
  #
  test_mat <- networkCorParallel(xdata,
    method = my_method, n.cores = 2, build.output = "matrix",
    #
    force.recalc = FALSE
  )
  expect_lt(sum(Matrix::norm(test_mat - mat_non_diag, type = "1")), 5e-10)
})

test_that("networkCorParallel: with platform windows", {
  withr::local_tempdir(pattern = "base.dir") |> .baseDir()

  local({
    .Platform$OS.type <- "windows"

    xdata <- random_xdata()
    my_method <- "pearson"
    mat_non_diag <- cor(xdata, method = my_method)
    diag(mat_non_diag) <- 0
    #
    test_mat <- networkCorParallel(
      xdata,
      method = my_method, n.cores = 2,
      build.output = "matrix",
      #
      force.recalc = TRUE
    )

    expect_lt(sum(Matrix::norm(test_mat - mat_non_diag, type = "1")), 5e-10)
    #
    test_mat <- networkCorParallel(
      xdata,
      method = my_method, n.cores = 2, build.output = "matrix",
      #
      force.recalc = FALSE
    )
    expect_lt(sum(Matrix::norm(test_mat - mat_non_diag, type = "1")), 5e-10)
  })
})
