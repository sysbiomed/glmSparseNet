set.seed(1985)
xdata <- matrix(rnorm(7000), nrow = 175)

covPearson <- abs(cov(xdata, method = "pearson"))
diag(covPearson) <- 0
covSpearman <- abs(cov(xdata, method = "spearman"))
diag(covSpearman) <- 0

# use a temporary directory that can be written
withr::local_tempdir(pattern = "base.dir") |>
    .baseDir()

describe("Degree - Covariance - Pearson", {
    it("Degree with cutoff", {
        covPearsonLocal <- covPearson
        covPearsonLocal[covPearsonLocal < 0.05] <- 0
        diffDegree <- degreeCov(
            xdata,
            method = "pearson",
            cutoff = 0.05,
            chunks = 10,
            nCores = 2,
            forceRecalcDegree = TRUE,
            forceRecalcNetwork = TRUE
        ) - Matrix::colSums(covPearsonLocal)
        expect_lt(sum(abs(diffDegree)), 1e-09)
    })

    it("Degree forcing recalculation", {
        diffDegree <- degreeCov(
            xdata,
            method = "pearson",
            cutoff = 0,
            chunks = 10,
            nCores = 2,
            forceRecalcDegree = TRUE,
            forceRecalcNetwork = TRUE
        ) - Matrix::colSums(covPearson)
        expect_lt(sum(abs(diffDegree)), 1e-09)
    })

    it("Degree forcing recalculation of degree only", {
        diffDegree <- degreeCov(
            xdata,
            method = "pearson",
            cutoff = 0,
            chunks = 10,
            nCores = 2,
            forceRecalcDegree = TRUE
        ) - Matrix::colSums(covPearson)
        expect_lt(sum(abs(diffDegree)), 1e-09)
    })

    it("Degree using cache", {
        degreeCov(
            xdata,
            method = "pearson",
            cutoff = 0,
            chunks = 10,
            nCores = 2,
            forceRecalcDegree = FALSE
        ) - Matrix::colSums(covPearson)
        diffDegree <- degreeCov(
            xdata,
            method = "pearson",
            cutoff = 0,
            chunks = 10,
            nCores = 2,
            forceRecalcDegree = FALSE
        ) - Matrix::colSums(covPearson)
        expect_lt(sum(abs(diffDegree)), 1e-09)
    })
})

describe("Degree - Covariance - Spearman", {
    it("Degree with cutoff", {
        covSpearmanLocal <- covSpearman
        covSpearmanLocal[covSpearmanLocal < 0.05] <- 0
        diffDegree <- degreeCov(
            xdata,
            method = "spearman",
            cutoff = 0.05,
            chunks = 10,
            nCores = 2,
            forceRecalcDegree = TRUE,
            forceRecalcNetwork = TRUE
        ) - Matrix::colSums(covSpearmanLocal)
        expect_lt(sum(abs(diffDegree)), 1e-09)
    })

    it("Degree forcing recalculation of all", {
        diffDegree <- degreeCov(
            xdata,
            method = "spearman",
            cutoff = 0,
            chunks = 10,
            nCores = 2,
            forceRecalcDegree = TRUE,
            forceRecalcNetwork = TRUE
        ) - Matrix::colSums(covSpearman)
        expect_lt(sum(abs(diffDegree)), 1e-09)
    })

    it("Degree forcing recalculation of degree", {
        degreeCov(
            xdata,
            method = "spearman",
            cutoff = 0,
            chunks = 10,
            nCores = 2,
            forceRecalcDegree = TRUE,
            forceRecalcNetwork = TRUE
        ) - Matrix::colSums(covSpearman)
        diffDegree <- degreeCov(
            xdata,
            method = "spearman",
            cutoff = 0,
            chunks = 10,
            nCores = 2,
            forceRecalcDegree = TRUE
        ) - Matrix::colSums(covSpearman)
        expect_lt(sum(abs(diffDegree)), 1e-09)
    })

    it("Degree using cache", {
        # forcing recalculation
        degreeCov(
            xdata,
            method = "spearman",
            cutoff = 0,
            chunks = 10,
            nCores = 2,
            forceRecalcDegree = TRUE,
            forceRecalcNetwork = TRUE
        ) - Matrix::colSums(covSpearman)
        # actual call to get from cache
        diffDegree <- degreeCov(
            xdata,
            method = "spearman",
            cutoff = 0,
            chunks = 10,
            nCores = 2,
            forceRecalcDegree = FALSE
        ) - Matrix::colSums(covSpearman)
        expect_lt(sum(abs(diffDegree)), 1e-09)
    })
})