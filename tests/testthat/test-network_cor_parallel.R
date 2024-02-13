context("Correlation matrix")

test_that("networkCorParallel: Default methods", {
    withr::local_tempdir(pattern = "base.dir") |> .baseDir()
    xdata <- randomXData()

    matrixNonDiag <- cor(xdata)
    diag(matrixNonDiag) <- 0
    testMatrix <- networkCorParallel(xdata,
        nCores = 1, buildOutput = "matrix",
        #
        forceRecalc = TRUE
    )

    expect_lt(sum(Matrix::norm(testMatrix - matrixNonDiag, type = "1")), 5e-10)
    #
    testMatrix <- networkCorParallel(xdata,
        nCores = 1, buildOutput = "matrix",
        #
        forceRecalc = FALSE
    )
    expect_lt(sum(Matrix::norm(testMatrix - matrixNonDiag, type = "1")), 5e-10)
})

test_that("networkCorParallel: Spearman correlation", {
    withr::local_tempdir(pattern = "base.dir") |> .baseDir()
    xdata <- randomXData()

    myMethod <- "spearman"
    matrixNonDiag <- cor(xdata, method = myMethod)
    diag(matrixNonDiag) <- 0
    testMatrix <- networkCorParallel(xdata,
        method = myMethod, nCores = 1, buildOutput = "matrix",
        #
        forceRecalc = TRUE
    )

    expect_lt(sum(Matrix::norm(testMatrix - matrixNonDiag, type = "1")), 5e-10)
    #
    testMatrix <- networkCorParallel(xdata,
        method = myMethod, nCores = 1, buildOutput = "matrix",
        #
        forceRecalc = FALSE
    )
    expect_lt(sum(Matrix::norm(testMatrix - matrixNonDiag, type = "1")), 5e-10)
})

test_that("networkCorParallel: Pearson correlation", {
    withr::local_tempdir(pattern = "base.dir") |> .baseDir()
    xdata <- randomXData()

    myMethod <- "pearson"
    matrixNonDiag <- cor(xdata, method = myMethod)
    diag(matrixNonDiag) <- 0
    #
    testMatrix <- networkCorParallel(xdata,
        method = myMethod, nCores = 1, buildOutput = "matrix",
        #
        forceRecalc = TRUE
    )

    expect_lt(sum(Matrix::norm(testMatrix - matrixNonDiag, type = "1")), 5e-10)
    #
    testMatrix <- networkCorParallel(xdata,
        method = myMethod, nCores = 1,
        buildOutput = "matrix",
        #
        forceRecalc = FALSE
    )
    expect_lt(sum(Matrix::norm(testMatrix - matrixNonDiag, type = "1")), 5e-10)
})

test_that("networkCorParallel: Multiple cores", {
    withr::local_tempdir(pattern = "base.dir") |> .baseDir()
    xdata <- randomXData()

    myMethod <- "pearson"
    matrixNonDiag <- cor(xdata, method = myMethod)
    diag(matrixNonDiag) <- 0
    #
    testMatrix <- networkCorParallel(
        xdata,
        method = myMethod, nCores = 2,
        buildOutput = "matrix",
        #
        forceRecalc = TRUE
    )

    expect_lt(sum(Matrix::norm(testMatrix - matrixNonDiag, type = "1")), 5e-10)
    #
    testMatrix <- networkCorParallel(xdata,
        method = myMethod, nCores = 2, buildOutput = "matrix",
        #
        forceRecalc = FALSE
    )
    expect_lt(sum(Matrix::norm(testMatrix - matrixNonDiag, type = "1")), 5e-10)
})

test_that("networkCorParallel: build vector", {
    withr::local_tempdir(pattern = "base.dir") |> .baseDir()
    xdata <- randomXData()

    myMethod <- "pearson"
    matrixNonDiag <- cor(xdata, method = myMethod)
    diag(matrixNonDiag) <- 0

    #
    testMatrix <- networkCorParallel(
        xdata,
        method = myMethod, nCores = 2,
        buildOutput = "vector",
        #
        forceRecalc = TRUE
    )

    sum(testMatrix[1:59] - as.vector(matrixNonDiag)[2:60]) |>
        expect_lt(5e-10)
})
