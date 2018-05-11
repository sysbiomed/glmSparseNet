context("Covariance function")

set.seed(1985)
xdata <- matrix(rnorm(30000), nrow = 500)

test_that("Spearman covariance is the same", {
  my.method <- 'spearman'
  cov.non.diag <- cov(xdata, method = my.method)
  diag(cov.non.diag) <- 0
  test.cov <- cov.parallel(xdata, method = my.method, n.cores = 2, build.output = 'matrix',
                           #
                           base.dir = tempdir(), force.recalc = TRUE)

  expect_lt(sum(Matrix::norm(test.cov - cov.non.diag, type = "1")), 5e-16)
  #
  test.cov <- cov.parallel(xdata, method = my.method, n.cores = 2, build.output = 'matrix',
                           #
                           base.dir = tempdir(), force.recalc = FALSE)
  expect_lt(sum(Matrix::norm(test.cov - cov.non.diag, type = "1")), 5e-16)
})

test_that("Pearson covariance is the same", {
  my.method <- 'pearson'
  cov.non.diag <- cov(xdata, method = my.method)
  diag(cov.non.diag) <- 0
  #
  test.cov <- cov.parallel(xdata, method = my.method, n.cores = 2, build.output = 'matrix',
                           #
                           base.dir = tempdir(), force.recalc = TRUE)

  expect_lt(sum(Matrix::norm(test.cov - cov.non.diag, type = "1")), 5e-16)
  #
  test.cov <- cov.parallel(xdata, method = my.method, n.cores = 2, build.output = 'matrix',
                           #
                           base.dir = tempdir(), force.recalc = FALSE)
  expect_lt(sum(Matrix::norm(test.cov - cov.non.diag, type = "1")), 5e-16)
})
