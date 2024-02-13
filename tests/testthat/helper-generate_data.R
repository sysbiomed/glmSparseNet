randomXData <- function(nValues = 30000, nRow = 500, seed = 1985) {
    set.seed(seed)
    matrix(rnorm(nValues), nrow = nRow)
}

prepareMAE <- function(maxRows = NULL) {
    tmpEnv <- new.env()
    data("miniACC", package = "MultiAssayExperiment", envir = tmpEnv)

    xdata <- tmpEnv$miniACC

    if (!is.null(maxRows)) xdata <- xdata[, 1:maxRows]

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

    list(xdata = xdataValid, ydata = ydataValid)
}

prepareMockInteractions <- function() {
    dplyr::tibble(
        protein1 = c(
            "9606.ENSP00000000233", "9606.ENSP00000000234",
            "9606.ENSP00000000235", "9606.ENSP00000000236",
            "9606.ENSP00000000237"
        ),
        protein2 = c(
            "9606.ENSP00000272298", "9606.ENSP00000253401",
            "9606.ENSP00000401445", "9606.ENSP00000418915",
            "9606.ENSP00000327801"
        ),
        neighborhood = 0,
        neighborhood_transferred = 0,
        fusion = 0,
        cooccurence = c(332, 0, 0, 0, 0),
        homology = 0,
        coexpression = c(0, 0, 0, 0, 69),
        coexpression_transferred = c(62, 0, 0, 61, 61),
        experiments = 0,
        experiments_transferred = c(181, 186, 160, 158, 78),
        database = 0,
        database_transferred = 0,
        textmining = c(0, 0, 0, 542, 0),
        textmining_transferred = c(125, 56, 0, 0, 89),
        combined_score = c(490, 198, 159, 606, 167)
    )
}

prepareOvarian <- function(columns = c("age", "resid.ds")) {
    list(
        xdata = survival::ovarian[, columns],
        ydata = data.frame(
            time = survival::ovarian$futime,
            status = survival::ovarian$fustat
        )
    )
}
