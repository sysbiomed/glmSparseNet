context("degree")

set.seed(1985)
xdata <- matrix(rnorm(30000), nrow = 500)

test_that('Degree is the same (pearson)', {
  cor.parallel(xdata, method = 'pearson', n.cores = 2, build.matrix = T)
  cor.m <- abs(cor(xdata, method = 'pearson')) - diag(ncol(xdata))
  diff.degree <- degree.weighted(xdata, method = 'pearson', cutoff = 0, n.cores = 16) - colSums(cor.m)
  expect_lt(sum(abs(diff.degree)), 5e-15)
})

test_that('Degree is the same (spearman)', {
  cor.parallel(xdata, method = 'spearman', n.cores = 2, build.matrix = T)
  cor.m <- abs(cor(xdata, method = 'spearman')) - diag(ncol(xdata))
  diff.degree <- degree.weighted(xdata, method = 'spearman', cutoff = 0, n.cores = 16) - colSums(cor.m)
  expect_lt(sum(abs(diff.degree)), 5e-15)
})
