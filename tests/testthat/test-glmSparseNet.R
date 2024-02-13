test_that("glmSparseNet: simple call", {
    xdata <- matrix(rnorm(100), ncol = 20)
    glmSparseNet(
        xdata, rnorm(nrow(xdata)), "correlation", family = "gaussian"
    ) |>
        expect_s3_class("glmnet")
})

test_that("glmSparseNet: simple call with MultiAssayExperiment", {
    data("miniACC", package = "MultiAssayExperiment")

    xdata <- miniACC

    eventIx <- which(!is.na(
        MultiAssayExperiment::colData(xdata)$days_to_death
    ))
    censIx <- which(!is.na(
        MultiAssayExperiment::colData(xdata)$days_to_last_followup
    ))

    xdata$surv_event_time <- array(
        NA_integer_, nrow(MultiAssayExperiment::colData(xdata))
    )
    xdata$surv_event_time[eventIx] <- xdata$days_to_death[eventIx]
    xdata$surv_event_time[censIx] <- xdata$days_to_last_followup[censIx]

    # Keep only valid individuals
    validIx <- as.vector(
        !is.na(xdata$surv_event_time) &
            !is.na(xdata$vital_status) &
            xdata$surv_event_time > 0
    )
    xdataValid <- xdata[
        , rownames(MultiAssayExperiment::colData(xdata))[validIx]
    ]
    ydataValid <- MultiAssayExperiment::colData(
        xdataValid
    )[, c("surv_event_time", "vital_status")]
    colnames(ydataValid) <- c("time", "status")

    glmSparseNet(
        xdataValid,
        ydataValid,
        family = "cox",
        network = "correlation",
        experiment = "RNASeq2GeneNorm"
    ) |>
        expect_warning("'experiments' dropped;") |>
        expect_s3_class("glmnet")
})

test_that("cv.glmSparseNet: simple call", {
    xdata <- matrix(rnorm(500), ncol = 5)
    cv.glmSparseNet(
        xdata,
        rnorm(nrow(xdata)), "correlation",
        family = "gaussian"
    ) |>
        expect_s3_class("cv.glmnet")
})

test_that("cv.glmSparseNet: simple call", {
    #
    # load data
    data("miniACC", package = "MultiAssayExperiment")
    xdata <- miniACC

    #
    # build valid data with days of last follow up or to event
    eventIx <- which(!is.na(xdata$days_to_death))
    censIx <- which(!is.na(xdata$days_to_last_followup))
    xdata$surv_event_time <- array(
        NA_integer_, nrow(MultiAssayExperiment::colData(xdata))
    )
    xdata$surv_event_time[eventIx] <- xdata$days_to_death[eventIx]
    xdata$surv_event_time[censIx] <- xdata$days_to_last_followup[censIx]

    #
    # Keep only valid individuals
    validIx <- as.vector(
        !is.na(xdata$surv_event_time) &
            !is.na(xdata$vital_status) &
            xdata$surv_event_time > 0
    )
    xdataValid <- xdata[
        , rownames(MultiAssayExperiment::colData(xdata))[validIx]
    ]
    ydataValid <- MultiAssayExperiment::colData(
        xdataValid
    )[, c("surv_event_time", "vital_status")]
    colnames(ydataValid) <- c("time", "status")

    #
    cv.glmSparseNet(
        xdataValid,
        ydataValid,
        nfolds = 5,
        family = "cox",
        network = "correlation",
        experiment = "RNASeq2GeneNorm"
    ) |>
        expect_warning("'experiments' dropped;") |>
        expect_s3_class("cv.glmnet")
})
