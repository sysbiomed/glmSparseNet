context("Covariance function")

set.seed(1985)
xdata <- matrix(rnorm(30000), nrow = 500)

test_that("Spearman correlation is the same", {
  my.method <- 'spearman'
  cor.non.diag <- cor(xdata, method = my.method)
  diag(cor.non.diag) <- 0
  test.cov <- cor.parallel(xdata, method = my.method, n.cores = 2, build.output = 'matrix',
                           #
                           base.dir = tempdir(), force.recalc = TRUE)

  expect_lt(sum(Matrix::norm(test.cov - cor.non.diag, type = "1")), 5e-16)
  #
  test.cov <- cor.parallel(xdata, method = my.method, n.cores = 2, build.output = 'matrix',
                           #
                           base.dir = tempdir(), force.recalc = FALSE)
  expect_lt(sum(Matrix::norm(test.cov - cor.non.diag, type = "1")), 5e-16)
})

test_that("Pearson correlation is the same", {
  my.method <- 'pearson'
  cor.non.diag <- cor(xdata, method = my.method)
  diag(cor.non.diag) <- 0
  #
  test.cov <- cor.parallel(xdata, method = my.method, n.cores = 2, build.output = 'matrix',
                           #
                           base.dir = tempdir(), force.recalc = TRUE)

  expect_lt(sum(Matrix::norm(test.cov - cor.non.diag, type = "1")), 5e-16)
  #
  test.cov <- cor.parallel(xdata, method = my.method, n.cores = 2, build.output = 'matrix',
                           #
                           base.dir = tempdir(), force.recalc = FALSE)
  expect_lt(sum(Matrix::norm(test.cov - cor.non.diag, type = "1")), 5e-16)
})
