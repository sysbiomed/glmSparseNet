context("run.cache")

base.dir(file.path(tempdir(), 'base.dir'))

cache0 <- file.path(tempdir(), 'run-cache')
cache1 <- file.path(tempdir(),'run-cache-changed1')
cache2 <- file.path(tempdir(), 'run-cache-changed2')

# Function to make sure we have correct platform
get_os <- tryCatch({
  get_os.fun <- function() {
    sysinf <- Sys.info()
    if (!is.null(sysinf)){
      os <- sysinf['sysname']
      if (os == 'Darwin')
        os <- "osx"
    } else { ## mystery machine
      os <- .Platform$OS.type
      if (grepl("^darwin", R.version$os))
        os <- "osx"
      if (grepl("linux-gnu", R.version$os))
        os <- "linux"
    }
    tolower(os)
  }
  get_os.fun()
})

test_that('folder can be created in tempdir', {
  result <- create.directory.for.cache(tempdir(), 'abcd')
  expect_true(dir.exists(result$parent.dir))
})

test_that('digest cache is consistent', {
  word <- '1234567'
  expect_equal(digest.cache(word), digest::digest(word, algo = 'sha256'))
  # taken manually at 2018.04.27
  expect_equal(
    digest.cache(word),
    '300a4687518d6e58377f814df9eb8a40f5befd3634de48c0fe893e47e127dbb3'
  )
})

test_that('tempdir is correct', {
  expect_equal(glmSparseNet:::tempdir.cache(), file.path(getwd(), 'run-cache'))
})

test_that("run.cache fails with arguments", {
  expect_error(
    run.cache(
      1, 1, 2, 3, 4, 5,
      base.dir = tempdir(), force.recalc = TRUE, show.message = TRUE
    )
  )
})

test_that("run.cache base.dir in folder that does not have access", {
  if (grepl('windows', get_os, ignore.case = TRUE)) {
    # CRAN automated tests allow to write in c:/Windows
    # expect_warning(
    #   run.cache(
    #     sum, 1, 2, 3, 4, 5,
    #     show.message = FALSE, base.dir = 'c:/Windows'
    #   ),
    #   'Could not create cache folder inside base.dir'
    # )
  } else if (grepl('darwin', get_os, ignore.case = TRUE)) {
    # Do nothing, the same test for linux fails
  } else if (grepl('linux', get_os, ignore.case = TRUE)) {
    expect_warning(
      run.cache(
        sum, 1, 2, 3, 4, 5,
        show.message = FALSE, base.dir = '/'
      ),
      'Could not create cache folder inside base.dir'
    )
  }
})

test_that("run.cache base.dir in folder that does not have access", {
  if (grepl('windows', get_os, ignore.case = TRUE)) {
    # CRAN automated tests allow to write in c:/Windows
    # expect_warning(
    #   run.cache(
    #     sum, 1, 2, 3, 4, 5,
    #     show.message = FALSE, base.dir = file.path('c:', 'windows', 'caca')),
    #   'Could not create cache base folder'
    # )
  } else if (grepl('darwin', get_os, ignore.case = TRUE)) {
    # Do nothing, the same test for linux fails
  } else if (grepl('linux', get_os, ignore.case = TRUE)) {
    expect_warning(
      run.cache(
        sum, 1, 2, 3, 4, 5,
        show.message = FALSE, base.dir = '/daca'
      ),
      'Could not create cache base folder'
    )
  } else {
    # do nothing (only perform tests on platforms above)
  }
})

test_that("run.cache base.dir in folder that does have access", {
  expect_equal(
    run.cache(
      sum, 1, 2, 3, 4, 5,
      base.dir = tempdir(),
      cache.digest = list(digest.cache(1)),
      show.message = FALSE
    ),
    15
  )
  
  expect_equal(
    run.cache(
      c, 1, 2, 3, 4, 5,
      base.dir = tempdir(),
      cache.digest = list(digest.cache(1)),
      show.message = FALSE
    ),
    c(1, 2, 3, 4, 5)
  )
})

test_that("Test slight differences in code", {
  # main code to compare
  fun.1 <- function(val1) {
    return(val1^2)
  }
  
  expect_identical(
    glmSparseNet:::build.function.digest(fun.1),
    glmSparseNet:::build.function.digest(fun.1)
  )
  
  # main code to compare
  fun.1.one.space <- function( val1) {
    return(val1^2)
  }
  
  expect_failure(
    expect_identical(
      glmSparseNet:::build.function.digest(fun.1),
      glmSparseNet:::build.function.digest(fun.1.one.space)
    )
  )
  
  # changes in spaces
  fun.1.spaces <- function(val1) { return(val1^2) }
  
  expect_failure(
    expect_identical(
      glmSparseNet:::build.function.digest(fun.1),
      glmSparseNet:::build.function.digest(fun.1.spaces)
    )
  )
  
  # same as fun.1 but defined in a different name
  fun.2 <- function(val1) {
    return(val1^2)
  }
  
  expect_identical(
    glmSparseNet:::build.function.digest(fun.1),
    glmSparseNet:::build.function.digest(fun.2)
  )
  
  # small difference in argument, but same body
  fun.2.slight.diff <- function(val2) {
    return(val1^2)
  }
  
  expect_failure(
    expect_identical(
      glmSparseNet:::build.function.digest(fun.1),
      glmSparseNet:::build.function.digest(fun.2.slight.diff)
    )
  )
  
  # using different variable
  fun.2.diff <- function(val2) {
    return(val2^2)
  }
  
  expect_failure(
    expect_identical(
      glmSparseNet:::build.function.digest(fun.1),
      glmSparseNet:::build.function.digest(fun.2.diff)
    )
  )
  
  # adds a new argument (usused in body)
  fun.2.diff.arg <- function(val1, val2 = FALSE) {
    return(val1^2)
  }
  
  expect_failure(
    expect_identical(
      glmSparseNet:::build.function.digest(fun.1),
      glmSparseNet:::build.function.digest(fun.2.diff.arg)
    )
  )
})

# Primitives have a very similar code
test_that("Two primitives give different results", {
  unique.tmp.dir <- file.path(tempdir(), 'two_primitives-run.cache')
  
  run.cache(sum, 1, 2, 3, 4, base.dir = unique.tmp.dir)
  run.cache(c, 1, 2, 3, 4, base.dir = unique.tmp.dir)
  
  expect_failure(
    expect_identical(
      run.cache(sum, 1, 2, 3, 4, base.dir = unique.tmp.dir),
      run.cache(c, 1, 2, 3, 4, base.dir = unique.tmp.dir)
    )
  )
})

# This tests the uniqueness of many different functions to see
# if the code is correct
test_that("builds different hash for different functions", {
  list.of.fun <- c(
    c, run.cache, expect_equal, expect_identical,
    tempdir, ISOdate, Sys.time, Sys.Date, Sys.timezone,
    abline, abs, aggregate, all, any, apply,
    apropos, attach, attr, attributes, as.Date, as.double,
    as.factor, as.name, axis, barplot, boxplot, call, casefold,
    cat, cbind, ceiling, charmatch, chartr, colMeans,
    colnames, colSums, complete.cases, cumsum,
    cut, dbeta, dbinom, dcauchy, dchisq, density,
    deparse, detach, dexp, df, dgamma,
    dgeom, dhyper, diff, difftime, dim, dir, dist, dlnorm,
    dlogis, dnbinom, dnorm, do.call, download.file, dpois, droplevels,
    dsignrank, dt, dunif, dweibull, dwilcox, ecdf, eval,
    exists, expression, find, floor, format, get, get0, getwd,
    gregexpr, grep, grepl, gsub, heatmap, hist,
    ifelse, integrate, IQR, is.double, is.na, is.name, is.nan, is.null,
    is.unsorted, jitter, julian, lapply, layout, length, list.dirs,
    load, log, log2, log10, lowess,
    mapply, match, max, mad, mean, median, merge, message,
    mget, min, months, na.omit, names, nchar, ncol, nrow, object.size, optim,
    optimize, order, outer, packageVersion, pairs, par, parse, paste,
    paste0, pbeta, pbinom, pcauchy, pchisq, pexp, pf, pgamma, pgeom, phyper,
    plnorm, plogis, plot, pmatch, pmax, pmin, pnbinom, pnorm, polygon,
    ppois, pretty, print, psignrank, pt, ptukey, punif,
    pweibull, pwilcox, qbeta, qbinom, qcauchy, qchisq, qexp, qf,
    qgamma, qgeom, qhyper, qlnorm, qlogis, qnbinom, qnorm, qpois, qqnorm,
    qsignrank, qt, qtukey, quantile, quarters, qunif, qweibull, qwilcox,
    R.Version, rank, rbeta, rbind, rbinom, rcauchy, rchisq,
    readline, readLines, readRDS, regexpr, regexec, remove,
    rep, replace, return, rev, rexp, rf, rgamma, rgeom,
    rhyper, rlnorm, rlogis, rnbinom, rnorm, round, row.names, rowMeans,
    rowSums, rpois, rsignrank, rt, runif, rweibull, rwilcox, sample,
    sapply, save, save.image, saveRDS, scale, scan, sd,
    segments, seq, set.seed, setdiff,
    setwd, shapiro.test, sign, signif, sink, solve, sort, sort.int,
    sort.list, split, sprintf, sqrt, stop,
    strftime, strptime, strsplit, structure, sub, substr, substring, sum,
    summary, sweep, switch, t, tapply, text, tolower, toupper, transform,
    trimws, trunc, tryCatch, type.convert, union, unique, unlist, unsplit,
    vapply, var, warning, weekdays, weighted.mean, which, with, within,
    write, xtfrm
  )
  
  fun.from.packages <- c(
    dplyr::all_equal, dplyr::anti_join, dplyr::arrange,
    dplyr::as.tbl, dplyr::between, dplyr::bind_cols, dplyr::bind_rows,
    dplyr::case_when, dplyr::coalesce, dplyr::combine, dplyr::cumall,
    dplyr::cumany, dplyr::cume_dist, dplyr::cummean, dplyr::dense_rank,
    dplyr::distinct, dplyr::filter, dplyr::first, dplyr::full_join,
    dplyr::if_else, dplyr::inner_join, dplyr::is.tbl, dplyr::lag,
    dplyr::last, dplyr::lead, dplyr::left_join, dplyr::min_rank,
    dplyr::mutate, dplyr::na_if, dplyr::near, dplyr::nth, dplyr::ntile,
    dplyr::percent_rank, dplyr::pull, dplyr::recode, dplyr::recode_factor,
    dplyr::rename, dplyr::right_join, dplyr::row_number, dplyr::sample_frac,
    dplyr::sample_n, dplyr::select, dplyr::semi_join, dplyr::slice,
    dplyr::top_frac, dplyr::top_n, dplyr::transmute, ggplot2::geom_boxplot,
    ggplot2::geom_histogram, ggplot2::geom_line, ggplot2::scale_fill_brewer,
    ggplot2::stat_qq_line, grid::unit, reshape2::melt
  )
  
  all.funs <- c(list.of.fun, fun.from.packages)
  
  list.of.fun.digest <- c(all.funs) %>%
    sapply(glmSparseNet:::build.function.digest)
  
  for (digest.ix in unique(list.of.fun.digest[duplicated(list.of.fun.digest)])) {
    print(all.funs[list.of.fun.digest == digest.ix])
    cat("----------------")
  }
  
  
  expect_identical(
    length(unique(list.of.fun.digest)),
    length(list.of.fun) + length(fun.from.packages))
})

# See if the add.to.hash argument really changes the signature
test_that("run.cache add to hash", {
  expect_message(
    run.cache(
      sum, 1, 2, 3, 4, 5,
      base.dir = tempdir(),
      force.recalc = TRUE,
      show.message = TRUE,
      add.to.hash = 'something'
    ),
    'Saving in cache'
  )
  expect_message(
    run.cache(
      sum, 1, 2, 3, 4, 5,
      base.dir = tempdir(),
      force.recalc = TRUE,
      show.message = TRUE,
      add.to.hash = 'other'
    ),
    'Saving in cache'
  )
  
  one <- capture_messages(
    run.cache(
      sum, 1, 2, 3, 4, 5,
      base.dir = tempdir(),
      force.recalc = FALSE,
      show.message = TRUE,
      add.to.hash = 'something'
    )
  )
  two <- capture_messages(
    run.cache(
      sum, 1, 2, 3, 4, 5,
      base.dir = tempdir(),
      force.recalc = FALSE,
      show.message = TRUE,
      add.to.hash = 'other'
    )
  )
  expect_false(all(one == two))
})

test_that("run.cache with seed", {
  expect_message(
    run.cache(
      rnorm, 1,
      seed = 10,
      base.dir = tempdir(),
      force.recalc = TRUE,
      show.message = TRUE
    ),
    'Saving in cache'
  )
  expect_message(
    run.cache(
      rnorm, 1,
      seed = 11,
      base.dir = tempdir(),
      force.recalc = TRUE,
      show.message = TRUE
    ),
    'Saving in cache'
  )
  expect_message(
    rnorm10 <- run.cache(
      rnorm, 1,
      seed = 10,
      base.dir = tempdir(),
      force.recalc = FALSE,
      show.message = TRUE
    ),
    'Loading from cache'
  )
  expect_message(
    rnorm11 <- run.cache(
      rnorm, 1,
      seed = 11,
      base.dir = tempdir(),
      force.recalc = FALSE,
      show.message = TRUE
    ),
    'Loading from cache'
  )
  
  expect_false(rnorm10 == rnorm11)
})

# test_that("run.cache saves to local directory", {
#   output <- capture_output(
#     run.cache(
#       sum, 1, 2, 3, 4, 5,
#       base.dir = tempdir(),
#       force.recalc = TRUE,
#       show.message = TRUE
#     )
#   )
#   expect_true(grepl(file.path('.', 'run-cache'), output))
# })

test_that("run.cache uses cache", {
  run.cache(
    sum, 1, 2, 3, 4, 5,
    base.dir = tempdir(),
    force.recalc = TRUE, show.message = FALSE
  )
  expect_message(
    run.cache(
      sum, 1, 2, 3, 4, 5,
      base.dir = tempdir(), force.recalc = FALSE, show.message = TRUE
    ),
    'Loading from cache'
  )
})

test_that("run.cache show.message option works", {
  show.message(TRUE)
  expect_message(
    run.cache(
      sum, 1, 2, 3, 4, 5, base.dir = tempdir(), force.recalc = TRUE
    ),
    'Saving in cache'
  )
  
  expect_message(
    run.cache(
      sum, 1, 2, 3, 4, 5,
      base.dir = tempdir(), force.recalc = TRUE, show.message = FALSE
    ),
    NA
  )
  
  show.message(FALSE)
  expect_message(
    run.cache(
      sum, 1, 2, 3, 4, 5, base.dir = tempdir(), force.recalc = TRUE
    ),
    NA
  )
  
  expect_message(
    run.cache(
      sum, 1, 2, 3, 4, 5,
      base.dir = tempdir(), force.recalc = TRUE, show.message = TRUE
    ),
    'Saving in cache'
  )
})

test_that("run.cache base.dir option works", {
  if (.Platform$OS.type == 'windows') {
    cache0.os <- gsub('\\\\', '\\\\\\\\', cache0)
    cache1.os <- gsub('\\\\', '\\\\\\\\', cache0)
    cache2.os <- gsub('\\\\', '\\\\\\\\', cache0)
  } else {
    cache0.os <- cache0
    cache1.os <- cache1
    cache2.os <- cache2
  }
  
  expect_message(
    run.cache(
      sum, 1, 2, 3, 4, 5, 9,
      base.dir = cache0, force.recalc = FALSE, show.message = TRUE
    ),
    cache0.os
  )
  
  expect_message(
    run.cache(
      sum, 1, 2, 3, 4, 5, 8,
      base.dir = cache1, force.recalc = FALSE, show.message = TRUE
    ),
    cache1.os
  )
  
  expect_message(
    run.cache(
      sum, 1, 2, 3, 4, 5, 9,
      base.dir = cache0, force.recalc = FALSE, show.message = TRUE
    ),
    cache0.os
  )
  
  base.dir(cache2)
  expect_message(
    run.cache(
      sum, 1, 2, 3, 4, 5,
      force.recalc = FALSE, show.message = TRUE),
    cache2.os
  )
})

