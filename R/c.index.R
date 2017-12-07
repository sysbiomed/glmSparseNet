#' Title
#'
#' @param val1.risk
#' @param val2.risk
#' @param val1.time
#' @param val2.time
#' @param val1.dead
#' @param val2.dead
#'
#' @return
#' @export
#'
#' @examples
my.c.index.cmp <- function(val1.risk, val2.risk, val1.time, val2.time, val1.dead, val2.dead) {
  # discard condition
  if ((!val1.dead && !val2.dead) || # both censured
      (val1.time < val2.time && !val1.dead) || # lower time censored
      (val1.time > val2.time && !val2.dead)) {
    return(0)
  }
  # same time
  if (val1.time == val2.time) {
    # both non-censured
    if (val1.dead && val1.dead) {
      if (val1.risk == val2.risk) {
        return(1)
      }
    } else {
      #
      if ((val1.risk < val2.risk && !val1.dead) || # lower time censored
          (val1.risk > val2.risk && !val2.dead)) {
        return(1)
      }
    }
  } else {
    if ((val1.time < val2.time && val1.risk > val2.risk) ||
        (val1.time > val2.time && val1.risk < val2.risk)) {
      return(1)
    }
  }
  return(0.5)
}
