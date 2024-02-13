context("Covariance function")

test_that("networkCovParallel: Default methods", {
    withr::local_tempdir(pattern = "base.dir") |> .baseDir()
    xdata <- randomXData()

    matrixNonDiag <- cov(xdata)
    diag(matrixNonDiag) <- 0
    testMatrix <- networkCovParallel(
        xdata,
        nCores = 1, buildOutput = "matrix",
        #
        forceRecalc = TRUE
    )

    expect_lt(sum(Matrix::norm(testMatrix - matrixNonDiag, type = "1")), 5e-10)
    #
    testMatrix <- networkCovParallel(
        xdata,
        nCores = 1, buildOutput = "matrix",
        #
        forceRecalc = FALSE
    )
    expect_lt(sum(Matrix::norm(testMatrix - matrixNonDiag, type = "1")), 5e-10)
})

test_that("networkCovParallel: Spearman correlation", {
    withr::local_tempdir(pattern = "base.dir") |> .baseDir()
    xdata <- randomXData()

    myMethod <- "spearman"
    matrixNonDiag <- cov(xdata, method = myMethod)
    diag(matrixNonDiag) <- 0
    testMatrix <- networkCovParallel(
        xdata,
        method = myMethod, nCores = 1, buildOutput = "matrix",
        #
        forceRecalc = TRUE
    )

    expect_lt(sum(Matrix::norm(testMatrix - matrixNonDiag, type = "1")), 5e-10)
    #
    testMatrix <- networkCovParallel(
        xdata,
        method = myMethod, nCores = 1, buildOutput = "matrix",
        #
        forceRecalc = FALSE
    )
    expect_lt(sum(Matrix::norm(testMatrix - matrixNonDiag, type = "1")), 5e-10)
})

test_that("networkCovParallel: Pearson correlation", {
    withr::local_tempdir(pattern = "base.dir") |> .baseDir()
    xdata <- randomXData()

    myMethod <- "pearson"
    matrixNonDiag <- cov(xdata, method = myMethod)
    diag(matrixNonDiag) <- 0
    #
    testMatrix <- networkCovParallel(
        xdata,
        method = myMethod, nCores = 1, buildOutput = "matrix",
        #
        forceRecalc = TRUE
    )

    expect_lt(sum(Matrix::norm(testMatrix - matrixNonDiag, type = "1")), 5e-10)
    #
    testMatrix <- networkCovParallel(
        xdata,
        method = myMethod, nCores = 1, buildOutput = "matrix",
        #
        forceRecalc = FALSE
    )
    expect_lt(sum(Matrix::norm(testMatrix - matrixNonDiag, type = "1")), 5e-10)
})

test_that("networkCovParallel: Multiple cores", {
    withr::local_tempdir(pattern = "base.dir") |> .baseDir()
    xdata <- randomXData()

    myMethod <- "pearson"
    matrixNonDiag <- cov(xdata, method = myMethod)
    diag(matrixNonDiag) <- 0
    #
    testMatrix <- networkCovParallel(
        xdata,
        method = myMethod, nCores = 2, buildOutput = "matrix",
        #
        forceRecalc = TRUE
    )

    expect_lt(sum(Matrix::norm(testMatrix - matrixNonDiag, type = "1")), 5e-10)
    #
    testMatrix <- networkCovParallel(
        xdata,
        method = myMethod, nCores = 2, buildOutput = "matrix",
        #
        forceRecalc = FALSE
    )
    expect_lt(sum(Matrix::norm(testMatrix - matrixNonDiag, type = "1")), 5e-10)
})
