set.seed(1985)
xdata <- matrix(rnorm(70000), nrow = 500)
cov.p <- abs(cov(xdata, method = 'pearson'))
diag(cov.p) <- 0
cov.s <- abs(cov(xdata, method = 'spearman'))
diag(cov.s) <- 0

context('Degree - Covariance - Pearson')

# use a temporary directory that can be written
glmSparseNet::base.dir(tempdir())

test_that('Degree with cutoff', {
  cov.p.0.05 <- cov.p
  cov.p.0.05[cov.p.0.05 < 0.05] <- 0
  diff.degree <- degree.cov(xdata, method = 'pearson', cutoff = 0.05,
                            chunks = 10, n.cores = 2,
                            force.recalc.degree = T,
                            force.recalc.network = T) - colSums(cov.p.0.05)
  expect_lt(sum(abs(diff.degree)), 1e-09)
})

test_that('Degree forcing recalculation', {
  diff.degree <- degree.cov(xdata, method = 'pearson', cutoff = 0, chunks = 10, n.cores = 2, force.recalc.degree = T, force.recalc.network =  T) - colSums(cov.p)
  expect_lt(sum(abs(diff.degree)), 1e-09)
})

test_that('Degree forcing recalculation of degree only', {
  diff.degree <- degree.cov(xdata, method = 'pearson', cutoff = 0, chunks = 10, n.cores = 2, force.recalc.degree = T) - colSums(cov.p)
  expect_lt(sum(abs(diff.degree)), 1e-09)
})

test_that('Degree using cache', {
  degree.cov(xdata, method = 'pearson', cutoff = 0, chunks = 10, n.cores = 2, force.recalc.degree = F) - colSums(cov.p)
  diff.degree <- degree.cov(xdata, method = 'pearson', cutoff = 0, chunks = 10, n.cores = 2, force.recalc.degree = F) - colSums(cov.p)
  expect_lt(sum(abs(diff.degree)), 1e-09)
})

context('Degree - Covariance - Spearman')

test_that('Degree with cutoff', {
  cov.s.0.05 <- cov.s
  cov.s.0.05[cov.s.0.05 < 0.05] <- 0
  diff.degree <- degree.cov(xdata, method = 'spearman', cutoff = 0.05, chunks = 10, n.cores = 2, force.recalc.degree = T, force.recalc.network = T) - colSums(cov.s.0.05)
  expect_lt(sum(abs(diff.degree)), 1e-09)
})

test_that('Degree forcing recalculation of all', {
  diff.degree <- degree.cov(xdata, method = 'spearman', cutoff = 0, chunks = 10, n.cores = 2, force.recalc.degree = T, force.recalc.network = T) - colSums(cov.s)
  expect_lt(sum(abs(diff.degree)), 1e-09)
})

test_that('Degree forcing recalculation of degree', {
  degree.cov(xdata, method = 'spearman', cutoff = 0, chunks = 10, n.cores = 2, force.recalc.degree = T, force.recalc.network = T) - colSums(cov.s)
  diff.degree <- degree.cov(xdata, method = 'spearman', cutoff = 0, chunks = 10, n.cores = 2, force.recalc.degree = T) - colSums(cov.s)
  expect_lt(sum(abs(diff.degree)), 1e-09)
})

test_that('Degree using cache', {
  # forcing recalculation
  degree.cov(xdata, method = 'spearman', cutoff = 0, chunks = 10, n.cores = 2, force.recalc.degree = T, force.recalc.network = T) - colSums(cov.s)
  # actual call to get from cache
  diff.degree <- degree.cov(xdata, method = 'spearman', cutoff = 0, chunks = 10, n.cores = 2, force.recalc.degree = F) - colSums(cov.s)
  expect_lt(sum(abs(diff.degree)), 1e-09)
})
