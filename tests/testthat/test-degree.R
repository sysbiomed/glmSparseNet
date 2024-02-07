set.seed(1985)
xdata <- matrix(rnorm(7000), nrow = 175)

corP <- abs(cor(xdata, method = "pearson")) - diag(ncol(xdata))
corS <- abs(cor(xdata, method = "spearman")) - diag(ncol(xdata))

# use a temporary directory that can be written
withr::local_tempdir(pattern = "base.dir") |>
  .baseDir()

context("Degree - Correlation - Pearson")

test_that("Degree with cutoff", {
  corP_0_5 <- corP
  corP_0_5[corP_0_5 < 0.5] <- 0
  diff.degree <- degreeCor(
    xdata,
    method = "pearson",
    cutoff = 0.5,
    chunks = 10,
    n.cores = 2,
    force.recalc.degree = TRUE,
    force.recalc.network = TRUE
  ) - Matrix::colSums(corP_0_5)
  expect_lt(sum(abs(diff.degree)), 5e-14)
})

test_that("Degree forcing recalculation", {
  diff.degree <- degreeCor(
    xdata,
    method = "pearson",
    cutoff = 0,
    chunks = 10,
    n.cores = 2,
    force.recalc.degree = TRUE,
    force.recalc.network = TRUE
  ) - Matrix::colSums(corP)
  expect_lt(sum(abs(diff.degree)), 5e-14)
})

test_that("Degree forcing recalculation of degree only", {
  diff.degree <- degreeCor(
    xdata,
    method = "pearson",
    cutoff = 0,
    chunks = 10,
    n.cores = 2,
    force.recalc.degree = TRUE
  ) - Matrix::colSums(corP)
  expect_lt(sum(abs(diff.degree)), 5e-14)
})

test_that("Degree using cache", {
  degreeCor(
    xdata,
    method = "pearson",
    cutoff = 0,
    chunks = 10,
    n.cores = 2,
    force.recalc.degree = FALSE
  ) - Matrix::colSums(corP)
  diff.degree <- degreeCor(
    xdata,
    method = "pearson",
    cutoff = 0,
    chunks = 10,
    n.cores = 2,
    force.recalc.degree = FALSE
  ) - Matrix::colSums(corP)
  expect_lt(sum(abs(diff.degree)), 5e-14)
})

context("Degree - Correlation - Spearman")

test_that("Degree with cutoff", {
  corS_0_5 <- corS
  corS_0_5[corS_0_5 < 0.5] <- 0
  diff.degree <- degreeCor(
    xdata,
    method = "spearman",
    cutoff = 0.5,
    chunks = 10,
    n.cores = 2,
    force.recalc.degree = TRUE,
    force.recalc.network = TRUE
  ) - Matrix::colSums(corS_0_5)
  expect_lt(sum(abs(diff.degree)), 5e-14)
})

test_that("Degree forcing recalculation of all", {
  diff.degree <- degreeCor(
    xdata,
    method = "spearman",
    cutoff = 0,
    chunks = 10,
    n.cores = 2,
    force.recalc.degree = TRUE,
    force.recalc.network = TRUE
  ) - Matrix::colSums(corS)
  expect_lt(sum(abs(diff.degree)), 5e-14)
})

test_that("Degree forcing recalculation of degree", {
  degreeCor(
    xdata,
    method = "spearman",
    cutoff = 0.05,
    chunks = 10,
    n.cores = 2,
    force.recalc.degree = TRUE,
    force.recalc.network = TRUE
  ) - Matrix::colSums(corS)
  diff.degree <- degreeCor(
    xdata,
    method = "spearman",
    cutoff = 0,
    chunks = 10,
    n.cores = 2,
    force.recalc.degree = TRUE
  ) - Matrix::colSums(corS)
  expect_lt(sum(abs(diff.degree)), 5e-14)
})

test_that("Degree using cache", {
  # forcing recalculation
  degreeCor(
    xdata,
    method = "spearman",
    cutoff = 0,
    chunks = 10,
    n.cores = 2,
    force.recalc.degree = TRUE,
    force.recalc.network = TRUE
  ) - Matrix::colSums(corS)
  # actual call to get from cache
  diff.degree <- degreeCor(
    xdata,
    method = "spearman",
    cutoff = 0,
    chunks = 10,
    n.cores = 2,
    force.recalc.degree = FALSE
  ) - Matrix::colSums(corS)
  expect_lt(sum(abs(diff.degree)), 5e-14)
})
