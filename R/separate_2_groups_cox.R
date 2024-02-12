#' Separate data in High and Low risk groups (based on Cox model)
#'
#' Draws multiple kaplan meyer survival curves (or just 1) and calculates
#' logrank test
#'
#' @param chosenBetas list of testing coefficients to calculate prognostic
#' indexes, for example `list(Age = some_vector)`.
#' @param xdata n x m matrix with n observations and m variables.
#' @param ydata Survival object.
#' @param probs How to separate high and low risk patients `50%-50%` is the
#' default, but for top and bottom `40%` -> `c(.4,.6)`.
#' @param noPlot Only calculate p-value and do not generate survival curve
#' plot.
#' @param plotTitle Name of file if.
#' @param xlim Optional argument to limit the x-axis view.
#' @param ylim Optional argument to limit the y-axis view.
#' @param legendOutside If TRUE legend will be outside plot, otherwise inside.
#' @param expandYZero expand to y = 0.
#' @param stopWhenOverlap when probs vector allows for overlapping of samples
#' in both groups, then stop.
#' @param chosen.btas `r lifecycle::badge("deprecated")`
#' @param no.plot `r lifecycle::badge("deprecated")`
#' @param plot.title `r lifecycle::badge("deprecated")`
#' @param legend.outside `r lifecycle::badge("deprecated")`
#' @param expand.yzero `r lifecycle::badge("deprecated")`
#' @param stop.when.overlap `r lifecycle::badge("deprecated")`
#'
#' Otherwise it will calculate with duplicate samples, i.e. simply adding them
#' to xdata and ydata (in a different group).
#' @param ... additional parameters to survminer::ggsurvplot
#'
#' @return object with logrank test and kaplan-meier survival plot
#'
#' A list with plot, p-value and kaplan-meier object. The plot was drawn from
#' survminer::ggsurvplot with only the palette, data and fit arguments being
#' defined and keeping all other defaults that can be customized as additional
#' parameters to this function.
#'
#' @export
#'
#' @seealso [survminer::ggsurvplot()]
#' @examples
#' xdata <- survival::ovarian[, c("age", "resid.ds")]
#' ydata <- data.frame(
#'   time = survival::ovarian$futime,
#'   status = survival::ovarian$fustat
#' )
#' separate2GroupsCox(c(age = 1, 0), xdata, ydata)
#' separate2GroupsCox(c(age = 1, 0.5), xdata, ydata)
#' separate2GroupsCox(
#'   c(age = 1), c(1, 0, 1, 0, 1, 0),
#'   data.frame(time = runif(6), status = rbinom(6, 1, .5))
#' )
#' separate2GroupsCox(list(
#'   aa = c(age = 1, 0.5),
#'   bb = c(age = 0, 1.5)
#' ), xdata, ydata)
separate2GroupsCox <- function(
    chosenBetas,
    xdata,
    ydata,
    probs = c(.5, .5),
    noPlot = FALSE,
    plotTitle = "SurvivalCurves",
    xlim = NULL,
    ylim = NULL,
    expandYZero = FALSE,
    legendOutside = FALSE,
    stopWhenOverlap = TRUE,
    ...,
    # Deprecated arguments with dots in name
    chosen.btas = deprecated(), # nolint: object_name_linter.
    no.plot = deprecated(), # nolint: object_name_linter.
    plot.title = deprecated(), # nolint: object_name_linter.
    expand.yzero = deprecated(), # nolint: object_name_linter.
    legend.outside = deprecated(), # nolint: object_name_linter.
    stop.when.overlap = deprecated()) { # nolint: object_name_linter.
  # Lifecycle management: to remove after 1.23.0
  if (lifecycle::is_present(chosen.btas)) {
    .deprecatedDotParam("separate2GroupsCox", "chosen.btas", "chosenBetas")
    chosenBetas <- chosen.btas
  }
  if (lifecycle::is_present(no.plot)) {
    .deprecatedDotParam("separate2GroupsCox", "no.plot")
    noPlot <- no.plot
  }
  if (lifecycle::is_present(plot.title)) {
    .deprecatedDotParam("separate2GroupsCox", "plot.title")
    plotTitle <- plot.title
  }
  if (lifecycle::is_present(expand.yzero)) {
    .deprecatedDotParam("separate2GroupsCox", "expand.yzero", "expandYZero")
    expandYZero <- expand.yzero
  }
  if (lifecycle::is_present(legend.outside)) {
    .deprecatedDotParam("separate2GroupsCox", "legend.outside")
    legendOutside <- legend.outside
  }
  if (lifecycle::is_present(stop.when.overlap)) {
    .deprecatedDotParam("separate2GroupsCox", "stop.when.overlap")
    stopWhenOverlap <- stop.when.overlap
  }
  # Lifecycle management: end

  checkmate::assert(
    .var.name = "chosenBetas",
    checkmate::check_list(chosenBetas, types = "numeric"),
    checkmate::check_numeric(chosenBetas),
  )
  checkmate::assert(
    .var.name = "xdata",
    checkmate::check_matrix(xdata),
    checkmate::check_data_frame(xdata),
    checkmate::check_numeric(xdata),
  )

  checkmate::assert_data_frame(ydata)
  checkmate::check_numeric(probs, len = 2)
  checkmate::assert_logical(noPlot)
  checkmate::assert_character(plotTitle)
  checkmate::assert_numeric(xlim, len = 2, null.ok = TRUE)
  checkmate::assert_numeric(ylim, len = 2, null.ok = TRUE)
  checkmate::assert_logical(expandYZero)
  checkmate::assert_logical(legendOutside)
  checkmate::assert_logical(stopWhenOverlap)

  #
  # convert between compatible formats
  if (inherits(chosenBetas, "numeric")) chosenBetas <- list(chosenBetas)

  if (!checkmate::test_names(chosenBetas)) {
    generatedNames <- seq_along(chosenBetas) |> as.character()
    if (is.null(names(chosenBetas))) {
      names(chosenBetas) <- generatedNames
    } else {
      emptyIx <- !nzchar(names(chosenBetas))
      names(chosenBetas)[emptyIx] <- generatedNames[emptyIx]
    }
  }

  # convert between compatible formats
  xdata <- Matrix::as.matrix(xdata)

  if (nrow(xdata) != nrow(ydata)) {
    rlang::abort(
      sprintf(
        "Rows in xdata (%d) and ydata (%d) must be the same",
        nrow(xdata), nrow(ydata)
      )
    )
  }
  if (!all(ncol(xdata) == vapply(chosenBetas, length, integer(1L)))) {
    rlang::abort(
      sprintf(
        paste(
          "All or some of the chosenBetas (%s) have different",
          "number of variables from xdata (%d)"
        ),
        paste(vapply(chosenBetas, length, 1), collapse = ", "),
        ncol(xdata)
      )
    )
  }
  #
  # creates a matrix from list of chosenBetas
  chosenBetasMat <- chosenBetas |>
    vapply(function(e) as.vector(e), numeric(ncol(xdata)))

  # calculate prognostic indexes for each patient and btas
  prognosticIndex <- xdata %*% chosenBetasMat

  colnames(prognosticIndex) <- names(chosenBetas)
  futile.logger::flog.debug("")
  futile.logger::flog.debug("prognosticIndex", prognosticIndex,
    capture = TRUE
  )

  # populate a data.frame with all patients (multiple rows per patients if has
  # multiple btas) already calculate high/low risk groups
  prognosticIndexDf <- buildPrognosticIndexDataFrame(
    ydata, probs, stopWhenOverlap, prognosticIndex
  )

  # factor the group
  prognosticIndexDf$group <- factor(prognosticIndexDf$group)
  # rename the factor to low / high risk
  newFactorStr <- .generateLegend(chosenBetas)

  newFactorStrL <- as.list(as.character(seq_len(2 * length(chosenBetas))))
  names(newFactorStrL) <- newFactorStr

  prognosticIndexDf$group <- do.call(
    forcats::fct_collapse,
    c(list(prognosticIndexDf$group), newFactorStrL)
  )
  #
  length(levels(prognosticIndexDf$group)) == 1 &&
    rlang::abort(
      paste(
        "separate2GroupsCox(): There is only one group, cannot create",
        "kaplan-meir curve with low and high risk groups"
      )
    )

  futile.logger::flog.debug("")
  futile.logger::flog.debug("prognosticIndexDf", prognosticIndexDf,
    capture = TRUE
  )
  #
  # Generate the Kaplan-Meier survival object
  km <- survival::survfit(survival::Surv(time, status) ~ group,
    data = prognosticIndexDf
  )
  km$custom.data <- prognosticIndexDf
  futile.logger::flog.debug("")
  futile.logger::flog.debug("kaplan-meier object", km, capture = TRUE)
  # Calculate the logrank test p-value
  survProb <- survival::survdiff(survival::Surv(time, status) ~ group,
    data = prognosticIndexDf
  )
  futile.logger::flog.debug("")
  futile.logger::flog.debug("survProb object", survProb, capture = TRUE)
  pValue <- 1 - stats::pchisq(survProb$chisq, df = 1)

  futile.logger::flog.debug("")
  futile.logger::flog.debug("pvalue: %g\n", pValue)

  .plotSurvival(
    noPlot,
    km,
    pValue,
    prognosticIndexDf,
    length(chosenBetas),
    plotTitle,
    xlim,
    ylim,
    expandYZero,
    legendOutside,
    ...
  )
}

#' @keywords internal
buildPrognosticIndexDataFrame <- function(
    ydata,
    probs,
    stopWhenOverlap,
    prognosticIndex) {
  prognosticIndexDf <- data.frame(
    time = c(), status = c(), group = c(), index = c()
  )

  for (ix in seq_len(dim(prognosticIndex)[2])) {
    # threshold
    #
    #
    sampleIxs <- rownames(prognosticIndex) %||%
      seq_len(nrow(prognosticIndex))

    tempGroup <- array(-1, dim(prognosticIndex)[1])
    piThres <- stats::quantile(
      prognosticIndex[, ix],
      probs = c(probs[1], probs[2])
    )

    if (
      sum(prognosticIndex[, ix] <= piThres[1]) == 0 ||
        sum(prognosticIndex[, ix] > piThres[2]) == 0
    ) {
      piThres[1] <- stats::median(unique(prognosticIndex[, ix]))
      piThres[2] <- piThres[1]
    }

    # low risk
    lowRiskIx <- prognosticIndex[, ix] <= piThres[1]
    tempGroup[lowRiskIx] <- (2 * ix) - 1
    # high risk
    highRiskIx <- prognosticIndex[, ix] > piThres[2]
    tempGroup[highRiskIx] <- (2 * ix)

    if (
      length(unique(prognosticIndex)) > 1 &&
        sum(lowRiskIx) + sum(highRiskIx) > length(prognosticIndex)
    ) {
      str.message <- paste0(
        "The cutoff values given to the function allow for some over ",
        "samples in both groups, with:\n  high risk size (",
        sum(highRiskIx), ") ",
        "+ low risk size (", sum(lowRiskIx), ") not equal to ",
        "xdata/ydata rows (", sum(highRiskIx) + sum(lowRiskIx),
        " != ", length(prognosticIndex), ")\n\n"
      )

      stopWhenOverlap && stop(str.message, "Stopping execution...")

      warning(
        str.message,
        "We are continuing with execution as parameter `stopWhenOverlap` ",
        "is FALSE.\n",
        "  note: This adds duplicate samples to ydata and xdata xdata"
      )

      overlapSamples <- which(as.vector(highRiskIx & lowRiskIx))
      #
      prognosticIndex <-
        t(t(c(prognosticIndex[, ], prognosticIndex[overlapSamples, ])))
      ydata <- rbind(ydata, ydata[overlapSamples, ])

      sampleIxs <- c(sampleIxs, sampleIxs[overlapSamples])
      tempGroup <- c(tempGroup, rep((2 * ix) - 1, length(overlapSamples)))
    }
    #
    validIx <- tempGroup != -1
    #
    prognosticIndexDf <- rbind(
      prognosticIndexDf,
      data.frame(
        pi = prognosticIndex[validIx, ix],
        time = ydata$time[validIx],
        status = ydata$status[validIx],
        group = tempGroup[validIx],
        index = sampleIxs[validIx]
      )
    )
  }

  prognosticIndexDf
}

#' @keywords internal
.plotSurvival <- function(
    noPlot,
    km,
    pValue,
    prognosticIndexDf,
    chosenBetasLen,
    plotTitle,
    xlim,
    ylim,
    expandYZero,
    legendOutside,
    ...) {
  if (noPlot) {
    return(list(pvalue = pValue, plot = NULL, km = km))
  }
  #
  # Plot survival curve
  #
  # remove group= from legend
  names(km$strata) <- gsub("group=", "", names(km$strata))
  # if there are more than 1 btas then lines should have transparency
  # (removed as it was not being used .5 and 1)

  colIx <- c("seagreen", "indianred2")
  if (chosenBetasLen > 1L) {
    colIx <- myColors()[c(
      1, 2, 4, 3, 10, 6, 12, 9, 5, 7, 8, 11, 13, 14, 15, 16, 17
    )]
  }

  p1 <- survminer::ggsurvplot(
    km,
    conf.int = FALSE,
    palette = colIx,
    data = prognosticIndexDf,
    ggtheme = ggplot2::theme_minimal(),
    ...
  )

  if (isTRUE(expandYZero)) {
    p1$plot <- p1$plot + ggplot2::expand_limits(y = .047)
  }
  # limit the x axis if needed
  if (!is.null(xlim)) {
    p1$plot <- p1$plot + ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
  }
  if (!is.null(ylim)) {
    p1$plot <- p1$plot + ggplot2::coord_cartesian(ylim = ylim, xlim = xlim)
  }

  p1$plot <- p1$plot + if (chosenBetasLen == 1L) {
    ggplot2::ggtitle(
      paste0(gsub("_", " ", plotTitle), "\np_value = ", pValue)
    )
  } else {
    ggplot2::ggtitle(paste0(gsub("_", " ", plotTitle)))
  }

  p1$plot <- p1$plot +
    ggplot2::labs(colour = paste0("p-value = ", format(pValue)))

  p1$plot <- p1$plot + ggplot2::theme(
    legend.key = ggplot2::element_blank(),
    legend.title = ggplot2::element_text(
      colour = "grey10",
      size = 10
    ),
    legend.background = ggplot2::element_rect(colour = "gray")
  )

  p1$plot <- p1$plot + if (isTRUE(legendOutside)) {
    ggplot2::theme(legend.key.size = ggplot2::unit(20, "points"))
  } else {
    ggplot2::theme(
      legend.position = c(1, 1),
      legend.justification = c(1, 1),
      legend.key.size = ggplot2::unit(20, "points")
    )
  }

  # return p-value, plot and km object
  list(pvalue = pValue, plot = p1, km = km)
}

#' @keywords internal
.generateLegend <- function(chosenBetas) {
  as.vector(vapply(seq_along(chosenBetas), function(ix) {
    if (!is.null(names(chosenBetas)) && length(names(chosenBetas)) >= ix) {
      e <- names(chosenBetas)[ix]
      as.list(paste0(c("Low risk - ", "High risk - "), e))
    } else {
      list("Low risk", "High risk")
    }
  }, list(1, 2)))
}
