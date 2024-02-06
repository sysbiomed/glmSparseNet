set.seed(1985)
xdata <- matrix(rnorm(7000), nrow = 175)

cor.p <- abs(cor(xdata, method = "pearson")) - diag(ncol(xdata))
cor.s <- abs(cor(xdata, method = "spearman")) - diag(ncol(xdata))

# use a temporary directory that can be written
withr::local_tempdir(pattern = "base.dir") |>
  .baseDir()

context("Degree - Correlation - Pearson")

test_that("Degree with cutoff", {
  cor.p.0.5 <- cor.p
  cor.p.0.5[cor.p.0.5 < 0.5] <- 0
  diff.degree <- degreeCor(
    xdata,
    method = "pearson",
    cutoff = 0.5,
    chunks = 10,
    n.cores = 2,
    force.recalc.degree = TRUE,
    force.recalc.network = TRUE
  ) - Matrix::colSums(cor.p.0.5)
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
  ) - Matrix::colSums(cor.p)
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
  ) - Matrix::colSums(cor.p)
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
  ) - Matrix::colSums(cor.p)
  diff.degree <- degreeCor(
    xdata,
    method = "pearson",
    cutoff = 0,
    chunks = 10,
    n.cores = 2,
    force.recalc.degree = FALSE
  ) - Matrix::colSums(cor.p)
  expect_lt(sum(abs(diff.degree)), 5e-14)
})

context("Degree - Correlation - Spearman")

test_that("Degree with cutoff", {
  cor.s.0.5 <- cor.s
  cor.s.0.5[cor.s.0.5 < 0.5] <- 0
  diff.degree <- degreeCor(
    xdata,
    method = "spearman",
    cutoff = 0.5,
    chunks = 10,
    n.cores = 2,
    force.recalc.degree = TRUE,
    force.recalc.network = TRUE
  ) - Matrix::colSums(cor.s.0.5)
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
  ) - Matrix::colSums(cor.s)
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
  ) - Matrix::colSums(cor.s)
  diff.degree <- degreeCor(
    xdata,
    method = "spearman",
    cutoff = 0,
    chunks = 10,
    n.cores = 2,
    force.recalc.degree = TRUE
  ) - Matrix::colSums(cor.s)
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
  ) - Matrix::colSums(cor.s)
  # actual call to get from cache
  diff.degree <- degreeCor(
    xdata,
    method = "spearman",
    cutoff = 0,
    chunks = 10,
    n.cores = 2,
    force.recalc.degree = FALSE
  ) - Matrix::colSums(cor.s)
  expect_lt(sum(abs(diff.degree)), 5e-14)
})
