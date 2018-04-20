set.seed(1985)
xdata <- matrix(rnorm(1000000), nrow = 500)

cov.p <- abs(cov(xdata, method = 'pearson'))
diag(cov.p) <- 0
cov.s <- abs(cov(xdata, method = 'spearman'))
diag(cov.s) <- 0

context('Degree - Covariance - Pearson')

test_that('Degree with cutoff', {
  cov.p.0.05 <- cov.p
  cov.p.0.05[cov.p.0.05 < 0.05] <- 0
  diff.degree <- degree.cov.weighted(xdata, method = 'pearson', cutoff = 0.05,
                                     n.cores = 16, base.dir = tempdir(),
                                     force.recalc.degree = T,
                                     force.recalc.covariance = T) - colSums(cov.p.0.05)
  expect_lt(sum(abs(diff.degree)), 5e-10)
})

test_that('Degree forcing recalculation', {
  diff.degree <- degree.cov.weighted(xdata, method = 'pearson', cutoff = 0, n.cores = 16, base.dir = tempdir(), force.recalc.degree = T, force.recalc.covariance =  T) - colSums(cov.p)
  expect_lt(sum(abs(diff.degree)), 5e-10)
})

test_that('Degree forcing recalculation of degree only', {
  diff.degree <- degree.cov.weighted(xdata, method = 'pearson', cutoff = 0, n.cores = 16, base.dir = tempdir(), force.recalc.degree = T) - colSums(cov.p)
  expect_lt(sum(abs(diff.degree)), 5e-10)
})

test_that('Degree using cache', {
  degree.cov.weighted(xdata, method = 'pearson', cutoff = 0, n.cores = 16, base.dir = tempdir(), force.recalc.degree = F) - colSums(cov.p)
  diff.degree <- degree.cov.weighted(xdata, method = 'pearson', cutoff = 0, n.cores = 16, base.dir = tempdir(), force.recalc.degree = F) - colSums(cov.p)
  expect_lt(sum(abs(diff.degree)), 5e-10)
})

context('Degree - Covariance - Spearman')

test_that('Degree with cutoff', {
  cov.s.0.05 <- cov.s
  cov.s.0.05[cov.s.0.05 < 0.05] <- 0
  diff.degree <- degree.cov.weighted(xdata, method = 'spearman', cutoff = 0.05, n.cores = 16, base.dir = tempdir(), force.recalc.degree = T, force.recalc.covariance = T) - colSums(cov.s.0.05)
  expect_lt(sum(abs(diff.degree)), 5e-10)
})

test_that('Degree forcing recalculation of all', {
  diff.degree <- degree.cov.weighted(xdata, method = 'spearman', cutoff = 0, n.cores = 16, base.dir = tempdir(), force.recalc.degree = T, force.recalc.covariance = T) - colSums(cov.s)
  expect_lt(sum(abs(diff.degree)), 5e-10)
})

test_that('Degree forcing recalculation of degree', {
  degree.cov.weighted(xdata, method = 'spearman', cutoff = 0, n.cores = 16, base.dir = tempdir(), force.recalc.degree = T, force.recalc.covariance = T) - colSums(cov.s)
  diff.degree <- degree.cov.weighted(xdata, method = 'spearman', cutoff = 0, n.cores = 16, base.dir = tempdir(), force.recalc.degree = T) - colSums(cov.s)
  expect_lt(sum(abs(diff.degree)), 5e-10)
})

test_that('Degree using cache', {
  # forcing recalculation
  degree.cov.weighted(xdata, method = 'spearman', cutoff = 0, n.cores = 16, base.dir = tempdir(), force.recalc.degree = T, force.recalc.covariance = T) - colSums(cov.s)
  # actual call to get from cache
  diff.degree <- degree.cov.weighted(xdata, method = 'spearman', cutoff = 0, n.cores = 16, base.dir = tempdir(), force.recalc.degree = F) - colSums(cov.s)
  expect_lt(sum(abs(diff.degree)), 5e-10)
})
