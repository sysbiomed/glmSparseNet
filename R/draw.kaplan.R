#' draw.kaplan deprecation warning
#'
#' @seealso separate2groups.cox
#'
#' @param ... see separate2groups.cox
#'
#' @return object with logrank test and kaplan-meier survival plot
#' @export
draw.kaplan <- function(...) {
  .Deprecated('separate2groups.cox',
              package='glmSparseNet',
              'The \'draw.kaplan\' function was renamed to \'separate2groups.cox\'.',
              old = as.character(sys.call(sys.parent()))[1L])
}


#' separate2groups.cox
#'
#' Mega function that draws multiple kaplan meyer survival curves (or just 1)
#'
#' @param chosen.btas list of testing coefficients to calculate prognostic indexes, for example ``list(Age = some_vector)``
#' @param xdata n x m matrix with n observations and m variables
#' @param ydata Survival object
#' @param probs How to separate high and low risk patients 50\%-50\% is the default, but for top and bottom 40\% -> c(.4,.6)
#' @param no.plot Only calculate p-value and do not generate survival curve plot
#' @param plot.title Name of file if
#' @param xlim Optional argument to limit the x-axis view
#' @param ylim Optional argument to limit the y-axis view
#' @param legend.outside If TRUE legend will be outside plot, otherwise inside
#' @param expand.yzero expand to y = 0
#'
#' @return object with logrank test and kaplan-meier survival plot
#'
#' A list with plot, p-value and kaplan-meier object
#'
#' @export
#'
#' @examples
#' data('ovarian', package = 'survival')
#' xdata <- ovarian[,c('age', 'resid.ds')]
#' ydata <- data.frame(time = ovarian$futime, status = ovarian$fustat)
#' separate2groups.cox(c(age = 1, 0), xdata, ydata)
#' separate2groups.cox(c(age = 1, 0.5), xdata, ydata)
#' separate2groups.cox(c(age = 1), c(1,0,1,0,1,0), data.frame(time = runif(6), status = rbinom(6, 1, .5)))
#' separate2groups.cox(list(aa = c(age = 1, 0.5), bb = c(age = 0, 1.5)), xdata, ydata)
setGeneric('separate2groups.cox', function(chosen.btas, xdata, ydata,
                                   probs          = c(.5, .5),
                                   no.plot        = FALSE,
                                   plot.title     = 'SurvivalCurves',
                                   xlim           = NULL,
                                   ylim           = NULL,
                                   expand.yzero   = FALSE,
                                   legend.outside = FALSE) {
  stop('wrong arguments, see help for separate2groups.cox')
})


#' separate2groups.cox
#'
#' @inheritParams separate2groups.cox
#'
#' @return object with logrank test and kaplan-meier survival plot
#' @export
#' @inherit separate2groups.cox return examples
setMethod('separate2groups.cox', signature(chosen.btas = 'numeric', xdata = 'data.frame', ydata = 'data.frame'), function(chosen.btas, xdata, ydata, probs, no.plot, plot.title, xlim, ylim, expand.yzero, legend.outside) { separate2groups.cox(list(chosen.btas), as.matrix(xdata), ydata, probs = probs, no.plot = no.plot, plot.title = plot.title, xlim = xlim, ylim = ylim, expand.yzero = expand.yzero, legend.outside = legend.outside)})

#' separate2groups.cox
#'
#' @inheritParams separate2groups.cox
#'
#' @export
#' @inherit separate2groups.cox return examples
setMethod('separate2groups.cox', signature(chosen.btas = 'numeric', xdata = 'matrix',     ydata = 'data.frame'), function(chosen.btas, xdata, ydata, probs, no.plot, plot.title, xlim, ylim, expand.yzero, legend.outside) {separate2groups.cox(list(chosen.btas), xdata, ydata, probs = probs, no.plot = no.plot, plot.title = plot.title, xlim = xlim, ylim = ylim, expand.yzero = expand.yzero, legend.outside = legend.outside)})

#' separate2groups.cox
#'
#' @inheritParams separate2groups.cox
#'
#' @export
#' @inherit separate2groups.cox return examples
setMethod('separate2groups.cox', signature(chosen.btas = 'numeric', xdata = 'numeric',    ydata = 'data.frame'), function(chosen.btas, xdata, ydata, probs, no.plot, plot.title, xlim, ylim, expand.yzero, legend.outside) {separate2groups.cox(list(chosen.btas), as.matrix(xdata), ydata, probs = probs, no.plot = no.plot, plot.title = plot.title, xlim = xlim, ylim = ylim, expand.yzero = expand.yzero, legend.outside = legend.outside)})

#' separate2groups.cox
#'
#' @inheritParams separate2groups.cox
#'
#' @export
#' @inherit cv.glmSparseNet return examples
setMethod('separate2groups.cox', signature(chosen.btas = 'list',    xdata = 'data.frame', ydata = 'data.frame'), function(chosen.btas, xdata, ydata, probs, no.plot, plot.title, xlim, ylim, expand.yzero, legend.outside) {separate2groups.cox(chosen.btas, as.matrix(xdata), ydata, probs = probs, no.plot = no.plot, plot.title = plot.title, xlim = xlim, ylim = ylim, expand.yzero = expand.yzero, legend.outside = legend.outside)})

#' separate2groups.cox
#'
#' @inheritParams separate2groups.cox
#'
#' @export
#' @inherit separate2groups.cox return examples
setMethod('separate2groups.cox', signature(chosen.btas = 'list',    xdata = 'numeric',    ydata = 'data.frame'), function(chosen.btas, xdata, ydata, probs, no.plot, plot.title, xlim, ylim, expand.yzero, legend.outside) { separate2groups.cox(chosen.btas, as.matrix(xdata), ydata, probs = probs, no.plot = no.plot, plot.title = plot.title, xlim = xlim, ylim = ylim, expand.yzero = expand.yzero, legend.outside = legend.outside)})

#' separate2groups.cox
#'
#' @inheritParams separate2groups.cox
#' @export
#' @inherit separate2groups.cox return examples
setMethod('separate2groups.cox', signature(chosen.btas = 'list', xdata = 'matrix', ydata = 'data.frame'),
          function(chosen.btas, xdata, ydata, probs, no.plot, plot.title, xlim, ylim, expand.yzero, legend.outside) {

            if (nrow(xdata) != nrow(ydata)) {
              stop(sprintf('Rows in xdata (%d) and ydata (%d) must be the same', nrow(xdata), nrow(ydata)))
            } else if (!all(ncol(xdata) == vapply(chosen.btas, length, 1))) {
              stop(sprintf('All or some of the chosen.btas (%s) have different number of variables from xdata (%d)', paste(vapply(chosen.btas, length, 1), collapse = ', '), ncol(xdata)))
            }
            #
            # creates a matrix from list of chosen.btas
            chosen.btas.mat <- vapply(chosen.btas, function(e){as.vector(e)}, rep(1.0, ncol(xdata)))
            # calculate prognostic indexes for each patient and btas
            prognostic.index <- tryCatch(xdata %*% chosen.btas.mat, error = function(err) {
              cat('      xdata is.matrix(.) =', is.matrix(xdata), '\n')
              cat('chosen.btas is.matrix(.) =', is.matrix(chosen.btas.mat), '\n')
              cat('      xdata (nrow)x(ncol) =', sprintf('%dx%d', nrow(xdata), ncol(xdata)), '\n')
              cat('chosen.btas (nrow)x(ncol) =', sprintf('%dx%d', nrow(chosen.btas.mat), ncol(chosen.btas.mat)), '\n')
              stop(err)
            })

            colnames(prognostic.index) <- names(chosen.btas)
            futile.logger::flog.debug('')
            futile.logger::flog.debug('prognostic.index', prognostic.index, capture = TRUE)
            prognostic.index.df <- data.frame(time = c(), status = c(), group = c())
            # populate a data.frame with all patients (multiple rows per patients if has multiple btas)
            # already calculate high/low risk groups

            for (ix in seq_len(dim(prognostic.index)[2])) {
              # threshold
              #
              #
              temp.group <- array(-1, dim(prognostic.index)[1])
              pi.thres <- stats::quantile(prognostic.index[,ix], probs = c(probs[1], probs[2]))

              if (sum(prognostic.index[,ix] <=  pi.thres[1]) == 0 ||
                  sum(prognostic.index[,ix] >  pi.thres[2]) == 0) {
                pi.thres[1] <- stats::median(unique(prognostic.index[,ix]))
                pi.thres[2] <- pi.thres[1]
              }

              # low risk
              temp.group[prognostic.index[,ix] <=  pi.thres[1]] <- (2 * ix) - 1
              # high risk
              temp.group[prognostic.index[,ix] > pi.thres[2]] <- (2 * ix)
              #
              valid_ix <- temp.group != -1
              #
              prognostic.index.df <- rbind(prognostic.index.df,
                                           data.frame(pi     = prognostic.index[valid_ix,ix],
                                                      time   = ydata$time[valid_ix],
                                                      status = ydata$status[valid_ix],
                                                      group  = temp.group[valid_ix]))
            }
            # factor the group
            prognostic.index.df$group <- factor(prognostic.index.df$group)
            # rename the factor to low / high risk
            new.factor.str            <- as.vector(vapply(seq_along(chosen.btas), function(ix) {
              if (!is.null(names(chosen.btas)) && length(names(chosen.btas)) >= ix) {
                e <- names(chosen.btas)[ix]
                as.list(paste0(c('Low risk - ', 'High risk - '), e))
              } else {
                list('Low risk', 'High risk')
              }
            }, list(1,2)))

            new.factor.str.l <- as.list(as.character(seq_len(2*length(chosen.btas))))
            names(new.factor.str.l) <- new.factor.str

            . <- NULL # Satisfy R CMD CHECK
            prognostic.index.df$group <- prognostic.index.df$group %>%
              list %>%
              c(new.factor.str.l) %>%
              do.call(forcats::fct_collapse, .)
            #
            if (length(levels(prognostic.index.df$group)) == 1) {
              stop('separate2groups.cox(): There is only one group, cannot create kaplan-meir curve with low and high risk groups')
            }
            futile.logger::flog.debug('')
            futile.logger::flog.debug('prognostic.index.df', prognostic.index.df, capture = TRUE)
            #
            # Generate the Kaplan-Meier survival object
            km        <- survival::survfit(survival::Surv(time, status) ~ group,  data = prognostic.index.df)
            futile.logger::flog.debug('')
            futile.logger::flog.debug('kaplan-meier object', km, capture = TRUE)
            # Calculate the logrank test p-value
            surv.prob <- survival::survdiff(survival::Surv(time, status) ~ group,  data = prognostic.index.df)
            futile.logger::flog.debug('')
            futile.logger::flog.debug('surv.prob object', surv.prob, capture = TRUE)
            p_value   <- 1 - stats::pchisq(surv.prob$chisq, df = 1)

            futile.logger::flog.debug('')
            futile.logger::flog.debug('pvalue: %g\n', p_value)

            if (no.plot) {
              return(list(pvalue = p_value, plot = NULL, km = km))
            }

            #
            # Plot survival curve
            #
            # remove group= from legend
            names(km$strata) <- gsub('group=','',names(km$strata))
            # if there are more than 1 btas then lines should have transparency
            if (length(chosen.btas) > 1) {
              my.alpha <- .5
            } else {
              my.alpha <- 1
            }

            # plot using ggfortify library's autoplot.survfit
            requireNamespace('ggfortify')
            p1 <- ggplot2::autoplot(km, conf.int = FALSE,
                                    xlab = 'Time', ylab = 'Cumulative Survival',
                                    surv.size = 1, censor.alpha = .8, surv.alpha = my.alpha)
            # generate title name
            titlename <- gsub('_', ' ', plot.title)
            titlename <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", titlename, perl=TRUE)
            #
            # add light theme (that has a white grid)
            p1 <- p1 + ggplot2::theme_light()
            # change legend options in ggplot
            p1 <- p1 + ggplot2::theme(legend.key = ggplot2::element_blank(), legend.title = ggplot2::element_text(colour = "grey10", size = 10),
                                      legend.background = ggplot2::element_rect(colour = "gray"))
            # make sure the 0% is shown
            if (expand.yzero)
              p1 <- p1 + ggplot2::expand_limits(y=.047)
            # limit the x axis if needed
            if (!is.null(xlim))
              p1 <- p1 + ggplot2::coord_cartesian(xlim=xlim, ylim = ylim)
            if (!is.null(ylim))
              p1 <- p1 + ggplot2::coord_cartesian(ylim=ylim, xlim = xlim)
            #
            # colors for the lines
            #  if more than one btas then paired curves (low and high) should have the same color
            #  otherwise, red and green!
            if (length(chosen.btas) > 1) {
              p1 <- p1 + ggplot2::scale_colour_manual(values = c(loose.rock::my.colors()[c(1,2,4,3,10,6,12,9,5,7,8,11,13,14,15,16,17)]))
              p1 <- p1 + ggplot2::theme(legend.title = ggplot2::element_blank())
              width <- 6
              height <- 4
            } else {
              p1 <- p1 + ggplot2::scale_colour_manual(values = c('seagreen', 'indianred2'))
              p1 <- p1 + ggplot2::labs(colour = paste0("p-value = ", format(p_value)))
              width <- 6
              height <- 4
            }
            if (legend.outside == TRUE)
              p1 <- p1 + ggplot2::theme(legend.key.size = ggplot2::unit(20,"points"))
            else
              p1 <- p1 + ggplot2::theme(legend.position = c(1,1), legend.justification = c(1, 1), legend.key.size = ggplot2::unit(20,"points"))
            # save to file
            #
            # after saving, show title in R plot
            if (length(chosen.btas) == 1) {
              p1 <- p1 + ggplot2::ggtitle(paste0(gsub('_', ' ', plot.title),'\np_value = ',p_value))
            } else {
              p1 <- p1 + ggplot2::ggtitle(paste0(gsub('_', ' ', plot.title)))
            }
            # return p-value, plot and km object
            return(list(pvalue = p_value, plot = p1, km = km))
          })
