test_that("glmOrphan: coefficients have the same number of variables", {
    xdata <- randomXData(100, 20)
    glmOrphan(
        xdata,
        rnorm(nrow(xdata)),
        "correlation",
        family = "gaussian",
        options = networkOptions(minDegree = .2)
    ) |>
        coef() |>
        nrow() |>
        expect_equal(ncol(xdata) + 1)
})

test_that("cv.glmOrphan: coefficients have the same number of variables", {
    xdata <- matrix(rnorm(100), ncol = 5)
    cv.glmOrphan(
        xdata,
        rnorm(nrow(xdata)),
        "correlation",
        family = "gaussian",
        nfolds = 5,
        options = networkOptions(minDegree = .2)
    ) |>
        coef("lambda.min") |>
        nrow() |>
        expect_equal(ncol(xdata) + 1)
})

test_that("cv.glmOrphan: folds are respected", {
    xdata <- matrix(rnorm(100), ncol = 5)
    cv.glmOrphan(
        xdata,
        rnorm(nrow(xdata)),
        "correlation",
        family = "gaussian",
        nfolds = 5,
        options = networkOptions(minDegree = .2),
        keep = TRUE
    ) |>
        magrittr::extract2("foldid") |>
        unique() |>
        expect_length(5)
})
