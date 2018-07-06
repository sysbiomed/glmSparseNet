#' #' Concordance index comparison between a pair
#' #'
#' #' @param risk.a risk of individual A
#' #' @param risk.b risk of indiviudal B
#' #' @param time.a time of event of individual A
#' #' @param time.b time of event of individual B
#' #' @param event.a has event occurred in individual A
#' #' @param event.b has event occurred in individual B
#' #'
#' #' @return the pairwise comparison
#' #'
#' #' @examples
#' #' my.c.index.cmp(1, 2, 10, 30, TRUE, FALSE)
#' #' my.c.index.cmp(1, 2, 10, 5, TRUE, FALSE)
#' #' my.c.index.cmp(1, 2, 10, 5, TRUE, TRUE)
#' my.c.index.cmp <- function(risk.a, risk.b, time.a, time.b, event.a, event.b) {
#'   # discard condition
#'   if ((!event.a && !event.b) || # both censured
#'       (time.a < time.b && !event.a) || # lower time censored
#'       (time.a > time.b && !event.b)) {
#'     return(0)
#'   }
#'   # same time
#'   if (time.a == time.b) {
#'     # both non-censured
#'     if (event.a && event.b) {
#'       if (risk.a == risk.b) {
#'         return(1)
#'       }
#'     } else {
#'       #
#'       if ((risk.a < risk.b && !event.a) || # lower time censored
#'           (risk.a > risk.b && !event.b)) {
#'         return(1)
#'       }
#'     }
#'   } else {
#'     if ((time.a < time.b && risk.a > risk.b) ||
#'         (time.a > time.b && risk.a < risk.b)) {
#'       return(1)
#'     }
#'   }
#'   return(0.5)
#' }
#'
#' #' Calculates concordance index (c-index) for a given relative risk and survival data
#' #'
#' #' @param fitted.risk Vector of fitted risk that can be obtained from a Cox model
#' #' @param survival.data A survival::Surv object or a data object with time and status columns
#' #' @param n.cores number of cores to use
#' #'
#' #' @return the c-index
#' #'
#' #' @examples
#' #' # Create the simplest tests data set
#' #' library(survival)
#' #' test1 <- list(time=c(4,3,1,1,2,2,3),
#' #'               status=c(1,1,1,0,1,1,0),
#' #'               x=c(0,2,1,1,1,0,0),
#' #'               sex=c(0,0,0,0,1,1,1))
#' #' # Fit a stratified model
#' #' fit1 <- coxph(Surv(time, status) ~ x + strata(sex), test1)
#' #' fitted.risk <- predict(fit1, newx = x, type = 'risk')
#' #' c.index(fitted.risk, Surv(test1$time, test1$status))
#' c.index <- function(fitted.risk, survival.data, n.cores = 1) {
#'   survConcordance.fit()
#'   all.pairs <- combn(nrow(survival.data), 2)
#'   time  <- survival.data[, 'time']
#'   event <- survival.data[, 'status']
#'
#'   c.index.pairwise <- unlist(parallel::mclapply(seq(ncol(all.pairs)), function(ix) {
#'     ix.a <- all.pairs[1,ix]
#'     ix.b <- all.pairs[2,ix]
#'     return(my.c.index.cmp(fitted.risk[ix.a], fitted.risk[ix.b], time[ix.a], time[ix.b], event[ix.a], event[ix.b]))
#'   }, mc.cores = n.cores))
#'
#'   c.res <- sum(c.index.pairwise) / sum(c.index.pairwise != 0)
#'
#'   return(c.res)
#' }
