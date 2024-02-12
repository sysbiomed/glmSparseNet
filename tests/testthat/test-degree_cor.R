set.seed(1985)
xdata <- matrix(rnorm(7000), nrow = 175)

corPearson <- abs(cor(xdata, method = "pearson")) - diag(ncol(xdata))
corSpearman <- abs(cor(xdata, method = "spearman")) - diag(ncol(xdata))

# use a temporary directory that can be written
withr::local_tempdir(pattern = "base.dir") |>
  .baseDir()

context("Degree - Correlation - Pearson")

test_that("Degree with cutoff", {
  corPearsonLocal <- corPearson
  corPearsonLocal[corPearsonLocal < 0.5] <- 0
  diffDegree <- degreeCor(
    xdata,
    method = "pearson",
    cutoff = 0.5,
    chunks = 10,
    nCores = 2,
    forceRecalcDegree = TRUE,
    forceRecalcNetwork = TRUE
  ) - Matrix::colSums(corPearsonLocal)
  expect_lt(sum(abs(diffDegree)), 5e-14)
})

test_that("Degree forcing recalculation", {
  diffDegree <- degreeCor(
    xdata,
    method = "pearson",
    cutoff = 0,
    chunks = 10,
    nCores = 2,
    forceRecalcDegree = TRUE,
    forceRecalcNetwork = TRUE
  ) - Matrix::colSums(corPearson)
  expect_lt(sum(abs(diffDegree)), 5e-14)
})

test_that("Degree forcing recalculation of degree only", {
  diffDegree <- degreeCor(
    xdata,
    method = "pearson",
    cutoff = 0,
    chunks = 10,
    nCores = 2,
    forceRecalcDegree = TRUE
  ) - Matrix::colSums(corPearson)
  expect_lt(sum(abs(diffDegree)), 5e-14)
})

test_that("Degree using cache", {
  degreeCor(
    xdata,
    method = "pearson",
    cutoff = 0,
    chunks = 10,
    nCores = 2,
    forceRecalcDegree = FALSE
  ) - Matrix::colSums(corPearson)
  diffDegree <- degreeCor(
    xdata,
    method = "pearson",
    cutoff = 0,
    chunks = 10,
    nCores = 2,
    forceRecalcDegree = FALSE
  ) - Matrix::colSums(corPearson)
  expect_lt(sum(abs(diffDegree)), 5e-14)
})

context("Degree - Correlation - Spearman")

test_that("Degree with cutoff", {
  corSpearmanLocal <- corSpearman
  corSpearmanLocal[corSpearmanLocal < 0.5] <- 0
  diffDegree <- degreeCor(
    xdata,
    method = "spearman",
    cutoff = 0.5,
    chunks = 10,
    nCores = 2,
    forceRecalcDegree = TRUE,
    forceRecalcNetwork = TRUE
  ) - Matrix::colSums(corSpearmanLocal)
  expect_lt(sum(abs(diffDegree)), 5e-14)
})

test_that("Degree forcing recalculation of all", {
  diffDegree <- degreeCor(
    xdata,
    method = "spearman",
    cutoff = 0,
    chunks = 10,
    nCores = 2,
    forceRecalcDegree = TRUE,
    forceRecalcNetwork = TRUE
  ) - Matrix::colSums(corSpearman)
  expect_lt(sum(abs(diffDegree)), 5e-14)
})

test_that("Degree forcing recalculation of degree", {
  degreeCor(
    xdata,
    method = "spearman",
    cutoff = 0.05,
    chunks = 10,
    nCores = 2,
    forceRecalcDegree = TRUE,
    forceRecalcNetwork = TRUE
  ) - Matrix::colSums(corSpearman)
  diffDegree <- degreeCor(
    xdata,
    method = "spearman",
    cutoff = 0,
    chunks = 10,
    nCores = 2,
    forceRecalcDegree = TRUE
  ) - Matrix::colSums(corSpearman)
  expect_lt(sum(abs(diffDegree)), 5e-14)
})

test_that("Degree using cache", {
  # forcing recalculation
  degreeCor(
    xdata,
    method = "spearman",
    cutoff = 0,
    chunks = 10,
    nCores = 2,
    forceRecalcDegree = TRUE,
    forceRecalcNetwork = TRUE
  ) - Matrix::colSums(corSpearman)
  # actual call to get from cache
  diffDegree <- degreeCor(
    xdata,
    method = "spearman",
    cutoff = 0,
    chunks = 10,
    nCores = 2,
    forceRecalcDegree = FALSE
  ) - Matrix::colSums(corSpearman)
  expect_lt(sum(abs(diffDegree)), 5e-14)
})
