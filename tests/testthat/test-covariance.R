context("Covariance function")

set.seed(1985)
xdata <- matrix(rnorm(30000), nrow = 500)

# use a temporary directory that can be written
glmSparseNet::base.dir(tempdir())

test_that("Default methods", {
  mat.non.diag <- cov(xdata)
  diag(mat.non.diag) <- 0
  test.mat <- network.cov.parallel(xdata, n.cores = 1, build.output = 'matrix',
                                   #
                                   force.recalc = TRUE)

  expect_lt(sum(Matrix::norm(test.mat - mat.non.diag, type = "1")), 5e-10)
  #
  test.mat <- network.cov.parallel(xdata, n.cores = 1, build.output = 'matrix',
                                   #
                                   force.recalc = FALSE)
  expect_lt(sum(Matrix::norm(test.mat - mat.non.diag, type = "1")), 5e-10)
})

test_that("Spearman correlation", {
  my.method <- 'spearman'
  mat.non.diag <- cov(xdata, method = my.method)
  diag(mat.non.diag) <- 0
  test.mat <- network.cov.parallel(xdata, method = my.method, n.cores = 1, build.output = 'matrix',
                                   #
                                   force.recalc = TRUE)

  expect_lt(sum(Matrix::norm(test.mat - mat.non.diag, type = "1")), 5e-10)
  #
  test.mat <- network.cov.parallel(xdata, method = my.method, n.cores = 1, build.output = 'matrix',
                                   #
                                   force.recalc = FALSE)
  expect_lt(sum(Matrix::norm(test.mat - mat.non.diag, type = "1")), 5e-10)
})

test_that("Pearson correlation", {
  my.method <- 'pearson'
  mat.non.diag <- cov(xdata, method = my.method)
  diag(mat.non.diag) <- 0
  #
  test.mat <- network.cov.parallel(xdata, method = my.method, n.cores = 1, build.output = 'matrix',
                                   #
                                   force.recalc = TRUE)

  expect_lt(sum(Matrix::norm(test.mat - mat.non.diag, type = "1")), 5e-10)
  #
  test.mat <- network.cov.parallel(xdata, method = my.method, n.cores = 1, build.output = 'matrix',
                                   #
                                   force.recalc = FALSE)
  expect_lt(sum(Matrix::norm(test.mat - mat.non.diag, type = "1")), 5e-10)
})

test_that("Multiple cores", {
  my.method    <- 'pearson'
  mat.non.diag <- cov(xdata, method = my.method)
  diag(mat.non.diag) <- 0
  #
  test.mat <- network.cov.parallel(xdata, method = my.method, n.cores = 4, build.output = 'matrix',
                                   #
                                   force.recalc = TRUE)

  expect_lt(sum(Matrix::norm(test.mat - mat.non.diag, type = "1")), 5e-10)
  #
  test.mat <- network.cov.parallel(xdata, method = my.method, n.cores = 4, build.output = 'matrix',
                                   #
                                   force.recalc = FALSE)
  expect_lt(sum(Matrix::norm(test.mat - mat.non.diag, type = "1")), 5e-10)
})
