#' draw.kaplan
#'
#' Mega function that draws multiple kaplan meyer survival curves (or just 1)
#'
#' @param chosen.btas list of testing coefficients to calculate prognostic indexes, for example ``list(Age = some_vector)``
#' @param xdata n x m matrix with n observations and m variables
#' @param ydata Survival object
#' @param probs How to separate high and low risk patients 50\%-50\% is the default, but for top and bottom 40\% -> c(.4,.6)
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
#' draw.kaplan(c(age = 1, 0), xdata, ydata)
#' draw.kaplan(c(age = 1, 0.5), xdata, ydata)
#' draw.kaplan(c(age = 1), c(1,0,1,0,1,0), data.frame(time = runif(6), status = rbinom(6, 1, .5)))
#' draw.kaplan(list(aa = c(age = 1, 0.5), bb = c(age = 1, 1.5)), xdata, ydata)
setGeneric('draw.kaplan', function(chosen.btas, xdata, ydata,
                                   probs          = c(.5, .5),
                                   plot.title       = 'SurvivalCurves',
                                   xlim           = NULL,
                                   ylim           = NULL,
                                   expand.yzero   = FALSE,
                                   legend.outside = FALSE) {
  stop('wrong arguments, see help for draw.kaplan')
})


#' draw.kaplan
#'
#' @inheritParams draw.kaplan
#'
#' @param chosen.btas numeric.
#' @param xdata data.frame.
#' @param ydata data.frame.
#'
#' @return object with logrank test and kaplan-meier survival plot
#' @export
#'
setMethod('draw.kaplan', signature(chosen.btas = 'numeric', xdata = 'data.frame', ydata = 'data.frame'), function(chosen.btas, xdata, ydata, probs, plot.title, xlim, ylim, expand.yzero, legend.outside) { draw.kaplan(list(chosen.btas), as.matrix(xdata), ydata, probs = probs, plot.title = plot.title, xlim = xlim, ylim = ylim, expand.yzero = expand.yzero, legend.outside = legend.outside)})

#' draw.kaplan
#'
#' @inheritParams draw.kaplan
#'
#' @param chosen.btas numeric.
#' @param xdata matrix.
#' @param ydata data.frame.
#'
#' @return object with logrank test and kaplan-meier survival plot
#' @export
#'
setMethod('draw.kaplan', signature(chosen.btas = 'numeric', xdata = 'matrix',     ydata = 'data.frame'), function(chosen.btas, xdata, ydata, probs, plot.title, xlim, ylim, expand.yzero, legend.outside) {draw.kaplan(list(chosen.btas), xdata, ydata, probs = probs, plot.title = plot.title, xlim = xlim, ylim = ylim, expand.yzero = expand.yzero, legend.outside = legend.outside)})

#' draw.kaplan
#'
#' @inheritParams draw.kaplan
#'
#' @param chosen.btas numeric.
#' @param xdata numeric.
#' @param ydata data.frame.
#'
#' @return object with logrank test and kaplan-meier survival plot
#' @export
#'
setMethod('draw.kaplan', signature(chosen.btas = 'numeric', xdata = 'numeric',    ydata = 'data.frame'), function(chosen.btas, xdata, ydata, probs, plot.title, xlim, ylim, expand.yzero, legend.outside) {draw.kaplan(list(chosen.btas), as.matrix(xdata), ydata, probs = probs, plot.title = plot.title, xlim = xlim, ylim = ylim, expand.yzero = expand.yzero, legend.outside = legend.outside)})

#' draw.kaplan
#'
#' @inheritParams draw.kaplan
#'
#' @param chosen.btas list.
#' @param xdata data.frame.
#' @param ydata data.frame.
#'
#' @return object with logrank test and kaplan-meier survival plot
#' @export
#'
setMethod('draw.kaplan', signature(chosen.btas = 'list',    xdata = 'data.frame', ydata = 'data.frame'), function(chosen.btas, xdata, ydata, probs, plot.title, xlim, ylim, expand.yzero, legend.outside) {draw.kaplan(chosen.btas, as.matrix(xdata), ydata, probs = probs, plot.title = plot.title, xlim = xlim, ylim = ylim, expand.yzero = expand.yzero, legend.outside = legend.outside)})

#' draw.kaplan
#'
#' @inheritParams draw.kaplan
#'
#' @param chosen.btas list.
#' @param xdata numeric.
#' @param ydata data.frame.
#'
#' @return object with logrank test and kaplan-meier survival plot
#' @export
#'
setMethod('draw.kaplan', signature(chosen.btas = 'list',    xdata = 'numeric',    ydata = 'data.frame'), function(chosen.btas, xdata, ydata, probs, plot.title, xlim, ylim, expand.yzero, legend.outside) { draw.kaplan(chosen.btas, as.matrix(xdata), ydata, probs = probs, plot.title = plot.title, xlim = xlim, ylim = ylim, expand.yzero = expand.yzero, legend.outside = legend.outside)})

#' draw.kaplan
#'
#' @inheritParams draw.kaplan
#'
#' @param chosen.btas list.
#' @param xdata matrix.
#' @param ydata data.frame.
#'
#' @return object with logrank test and kaplan-meier survival plot
#' @export
#'
setMethod('draw.kaplan', signature(chosen.btas = 'list', xdata = 'matrix', ydata = 'data.frame'),
          function(chosen.btas, xdata, ydata, probs, plot.title, xlim, ylim, expand.yzero, legend.outside) {

            if (nrow(xdata) != nrow(ydata)) {
              stop(sprintf('Rows in xdata (%d) and ydata (%d) must be the same', nrow(xdata), nrow(ydata)))
            } else if (!all(ncol(xdata) == sapply(chosen.btas, length))) {
              stop(sprintf('All or some of the chosen.btas (%s) have different number of variables from xdata (%d)', paste(sapply(chosen.btas, length), collapse = ', '), ncol(xdata)))
            }
            #
            # creates a matrix from list of chosen.btas
            chosen.btas.mat <- sapply(chosen.btas, function(e){as.vector(e)})
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
            futile.logger::flog.debug('prognostic.index', prognostic.index, capture = T)
            prognostic.index.df <- data.frame(time = c(), status = c(), group = c())
            # populate a data.frame with all patients (multiple rows per patients if has multiple btas)
            # already calculate high/low risk groups

            for (ix in 1:(dim(prognostic.index)[2])) {
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
            new.factor.str            <- as.vector(sapply(seq_along(chosen.btas), function(ix) {
              if (!is.null(names(chosen.btas)) && length(names(chosen.btas)) >= ix) {
                e <- names(chosen.btas)[ix]
                paste0(c('Low risk - ', 'High risk - '), e)
              } else {
                list('Low risk', 'High risk')
              }
            }))

            new.factor.str.l <- as.list(as.character(1:(2*length(chosen.btas))))
            names(new.factor.str.l) <- new.factor.str

            prognostic.index.df$group <- prognostic.index.df$group %>%
              list %>%
              c(new.factor.str.l) %>%
              do.call(forcats::fct_collapse, .)
            #
            if (length(levels(prognostic.index.df$group)) == 1) {
              stop('draw.kaplan(): There is only one group, cannot create kaplan-meir curve with low and high risk groups')
            }
            futile.logger::flog.debug('')
            futile.logger::flog.debug('prognostic.index.df', prognostic.index.df, capture = T)
            #
            # Generate the Kaplan-Meier survival object
            km        <- survival::survfit(survival::Surv(time, status) ~ group,  data = prognostic.index.df)
            futile.logger::flog.debug('')
            futile.logger::flog.debug('kaplan-meier object', km, capture = T)
            # Calculate the logrank test p-value
            surv.prob <- survival::survdiff(survival::Surv(time, status) ~ group,  data = prognostic.index.df)
            futile.logger::flog.debug('')
            futile.logger::flog.debug('surv.prob object', surv.prob, capture = T)
            p_value   <- 1 - stats::pchisq(surv.prob$chisq, df = 1)

            futile.logger::flog.debug('')
            futile.logger::flog.debug('pvalue: %g\n', p_value)
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
            if (legend.outside == T)
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
