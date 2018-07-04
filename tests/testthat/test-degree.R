set.seed(1985)
xdata <- matrix(rnorm(30000), nrow = 500)

cor.p <- abs(cor(xdata, method = 'pearson')) - diag(ncol(xdata))
cor.s <- abs(cor(xdata, method = 'spearman')) - diag(ncol(xdata))

# use a temporary directory that can be written
loose.rock::base.dir(tempdir())

context('Degree - Correlation - Pearson')

test_that('Degree with cutoff', {
  cor.p.0.5 <- cor.p
  cor.p.0.5[cor.p.0.5 < 0.5] <- 0
  diff.degree <- degree.cor(xdata, method = 'pearson', cutoff = 0.5, n.cores = 1, force.recalc.degree = T, force.recalc.network = T) - colSums(cor.p.0.5)
  expect_lt(sum(abs(diff.degree)), 5e-15)
})

test_that('Degree forcing recalculation', {
  diff.degree <- degree.cor(xdata, method = 'pearson', cutoff = 0, n.cores = 1, force.recalc.degree = T, force.recalc.network = T) - colSums(cor.p)
  expect_lt(sum(abs(diff.degree)), 5e-15)
})

test_that('Degree forcing recalculation of degree only', {
  diff.degree <- degree.cor(xdata, method = 'pearson', cutoff = 0, n.cores = 1, force.recalc.degree = T) - colSums(cor.p)
  expect_lt(sum(abs(diff.degree)), 5e-15)
})

test_that('Degree using cache', {
  degree.cor(xdata, method = 'pearson', cutoff = 0, n.cores = 1, force.recalc.degree = F) - colSums(cor.p)
  diff.degree <- degree.cor(xdata, method = 'pearson', cutoff = 0, n.cores = 1, force.recalc.degree = F) - colSums(cor.p)
  expect_lt(sum(abs(diff.degree)), 5e-15)
})

context('Degree - Correlation - Spearman')

test_that('Degree with cutoff', {
  cor.s.0.5 <- cor.s
  cor.s.0.5[cor.s.0.5 < 0.5] <- 0
  diff.degree <- degree.cor(xdata, method = 'spearman', cutoff = 0.5, n.cores = 1, force.recalc.degree = T, force.recalc.network = T) - colSums(cor.s.0.5)
  expect_lt(sum(abs(diff.degree)), 5e-15)
})

test_that('Degree forcing recalculation of all', {
  diff.degree <- degree.cor(xdata, method = 'spearman', cutoff = 0, n.cores = 1, force.recalc.degree = T, force.recalc.network = T) - colSums(cor.s)
  expect_lt(sum(abs(diff.degree)), 5e-15)
})

test_that('Degree forcing recalculation of degree', {
  degree.cor(xdata, method = 'spearman', cutoff = 0.05, n.cores = 1, force.recalc.degree = T, force.recalc.network = T) - colSums(cor.s)
  diff.degree <- degree.cor(xdata, method = 'spearman', cutoff = 0, n.cores = 1, force.recalc.degree = T) - colSums(cor.s)
  expect_lt(sum(abs(diff.degree)), 5e-15)
})

test_that('Degree using cache', {
  # forcing recalculation
  degree.cor(xdata, method = 'spearman', cutoff = 0, n.cores = 1, force.recalc.degree = T, force.recalc.network = T) - colSums(cor.s)
  # actual call to get from cache
  diff.degree <- degree.cor(xdata, method = 'spearman', cutoff = 0, n.cores = 1, force.recalc.degree = F) - colSums(cor.s)
  expect_lt(sum(abs(diff.degree)), 5e-15)
})
