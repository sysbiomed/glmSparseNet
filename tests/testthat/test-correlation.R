context("Correlation function")

set.seed(1985)
xdata <- matrix(rnorm(30000), nrow = 500)

test_that("spearman correlation is the same", {
  expect_lt(sum(Matrix::norm(cor.parallel(xdata[,1:10],
                                          method = 'spearman',
                                          n.cores = 2,
                                          build.matrix = T,
                                          base.dir = tempdir(), force.recalc = TRUE) - Matrix::triu(cor(xdata[,1:10], method = 'spearman'), k = 1), type = "1")),
            5e-16)
  #
  expect_lt(sum(Matrix::norm(cor.parallel(xdata[,1:10],
                                          method = 'spearman',
                                          n.cores = 2,
                                          build.matrix = T,
                                          base.dir = tempdir()) - Matrix::triu(cor(xdata[,1:10], method = 'spearman'), k = 1), type = "1")),
            5e-16)
})

test_that("Pearson correlation is the same", {
  expect_lt(sum(Matrix::norm(cor.parallel(xdata[,1:10],
                                  method = 'pearson',
                                  n.cores = 2,
                                  build.matrix = T,
                                  base.dir = tempdir()) - Matrix::triu(cor(xdata[,1:10], method = 'pearson'), k = 1), type = "1")),
            5e-16)
  expect_lt(sum(Matrix::norm(cor.parallel(xdata[,1:10],
                                          method = 'pearson',
                                          n.cores = 2,
                                          build.matrix = T,
                                          base.dir = tempdir(), force.recalc = TRUE) - Matrix::triu(cor(xdata[,1:10], method = 'pearson'), k = 1), type = "1")),
            5e-16)
})
