context("run.cache")

withr::local_tempdir(pattern = "base.dir") |>
  .baseDir()

test_that("folder can be created in tempdir", {
  result <- .createDirectoryForCache(withr::local_tempdir(), "abcd")
  expect_true(dir.exists(result$parent_dir))
})

test_that("digest cache is consistent", {
  word <- "1234567"
  expect_equal(.digestCache(word), rlang::hash(word))
  # taken manually at 2018.04.27
  expect_equal(
    .digestCache(word),
    "cd165630c0265b736b679ae63f597218"
  )
})

test_that("tempdir is correct", {
  expect_equal(glmSparseNet:::.tempdirCache(), file.path(getwd(), "run-cache"))
})

test_that("run_cache fails with arguments", {
  expect_error(
    .runCache(
      1, 1, 2, 3, 4, 5,
      # run_cache arguments
      base_dir = withr::with_tempdir(),
      force_recalc = TRUE,
      show_message = TRUE
    )
  )
})

test_that("run_cache base.dir in folder that does not have access", {
  if (grepl("windows", getOs, ignore.case = TRUE)) {
    # CRAN automated tests allow to write in c:/Windows
    # expect_warning(
    #   .runCache(
    #     sum, 1, 2, 3, 4, 5,
    #     show.message = FALSE, base.dir = 'c:/Windows'
    #   ),
    #   'Could not create cache folder inside base.dir'
    # )
  } else if (grepl("darwin", getOs, ignore.case = TRUE)) {
    # Do nothing, the same test for linux fails
  } else if (grepl("linux", getOs, ignore.case = TRUE)) {
    expect_warning(
      .runCache(
        sum, 1, 2, 3, 4, 5,
        # run_cache arguments
        show_message = FALSE, base_dir = "/"
      ),
      "Could not create cache folder inside base.dir"
    )
  }
})

test_that("run.cache base.dir in folder that does not have access", {
  if (grepl("windows", getOs, ignore.case = TRUE)) {
    # CRAN automated tests allow to write in c:/Windows
    # expect_warning(
    #   .runCache(
    #     sum, 1, 2, 3, 4, 5,
    #     show.message = FALSE, base.dir = file.path('c:', 'windows', 'caca')),
    #   'Could not create cache base folder'
    # )
  } else if (grepl("darwin", getOs, ignore.case = TRUE)) {
    # Do nothing, the same test for linux fails
  } else if (grepl("linux", getOs, ignore.case = TRUE)) {
    expect_warning(
      .runCache(
        sum, 1, 2, 3, 4, 5,
        # run_cache arguments
        show_message = FALSE, base_dir = "/daca"
      ),
      "Could not create cache base folder"
    )
  } else {
    # do nothing (only perform tests on platforms above)
  }
})

test_that("run_cache base.dir in folder that does have access", {
  expect_equal(
    .runCache(
      sum, 1, 2, 3, 4, 5,
      # run_cache arguments
      base_dir = withr::local_tempdir(),
      cache_digest = list(.digestCache(1)),
      show_message = FALSE
    ),
    15
  )

  expect_equal(
    .runCache(
      c, 1, 2, 3, 4, 5,
      # run_cache arguments
      base_dir = withr::local_tempdir(),
      cache_digest = list(.digestCache(1)),
      show_message = FALSE
    ),
    c(1, 2, 3, 4, 5)
  )
})

test_that("run_cache test slight differences in code", {
  # main code to compare
  fun1 <- function(val1) {
    return(val1^2)
  }

  expect_identical(
    glmSparseNet:::.buildFunctionDigest(fun1),
    glmSparseNet:::.buildFunctionDigest(fun1)
  )

  # main code to compare
  # nolint start: spaces_inside_linter
  # styler: off
  fun1OneSpace <- function(val1) {
    return( val1^2)
  }
  # styler: on
  # nolint end: spaces_inside_linter

  expect_failure(
    expect_identical(
      glmSparseNet:::.buildFunctionDigest(fun1),
      glmSparseNet:::.buildFunctionDigest(fun1OneSpace)
    )
  )

  # changes in spaces
  # nolint start: spaces_inside_linter
  # styler: off
  fun1Spaces <- function(val1) {
    return(val1^2 )
  }
  # styler: on
  # nolint end: spaces_inside_linter

  expect_failure(
    expect_identical(
      glmSparseNet:::.buildFunctionDigest(fun1),
      glmSparseNet:::.buildFunctionDigest(fun1Spaces)
    )
  )

  # same as fun1 but defined in a different name
  fun2 <- function(val1) {
    return(val1^2)
  }

  expect_identical(
    glmSparseNet:::.buildFunctionDigest(fun1),
    glmSparseNet:::.buildFunctionDigest(fun2)
  )

  # small difference in argument, but same body
  fun2SlightDiff <- function(val2) {
    return(val1^2)
  }

  expect_failure(
    expect_identical(
      glmSparseNet:::.buildFunctionDigest(fun1),
      glmSparseNet:::.buildFunctionDigest(fun2SlightDiff)
    )
  )

  # using different variable
  fun2Diff <- function(val2) {
    return(val2^2)
  }

  expect_failure(
    expect_identical(
      glmSparseNet:::.buildFunctionDigest(fun1),
      glmSparseNet:::.buildFunctionDigest(fun2Diff)
    )
  )

  # adds a new argument (usused in body)
  fun2DiffArg <- function(val1, val2 = FALSE) {
    return(val1^2)
  }

  expect_failure(
    expect_identical(
      glmSparseNet:::.buildFunctionDigest(fun1),
      glmSparseNet:::.buildFunctionDigest(fun2DiffArg)
    )
  )
})

# Primitives have a very similar code
test_that("run_cache: Two primitives give different results", {
  uniqueTmpDir <- withr::local_tempdir(pattern = "two_primitives-run_cache")

  .runCache(sum, 1, 2, 3, 4, base_dir = uniqueTmpDir)
  .runCache(c, 1, 2, 3, 4, base_dir = uniqueTmpDir)

  expect_failure(
    expect_identical(
      .runCache(sum, 1, 2, 3, 4, base_dir = uniqueTmpDir),
      .runCache(c, 1, 2, 3, 4, base_dir = uniqueTmpDir)
    )
  )
})

# This tests the uniqueness of many different functions to see
# if the code is correct
test_that("builds different hash for different functions", {
  listOfFun <- c(
    c, .runCache, expect_equal, expect_identical,
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

  funFromPackages <- c(
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

  allFuns <- c(listOfFun, funFromPackages)

  funDigest <- sapply(allFuns, glmSparseNet:::.buildFunctionDigest)

  for (digest_ix in unique(fun_digest[duplicated(fun_digest)])) {
    print(allFuns[fun_digest == digest_ix])
    futile.logger::flog.info("----------------")
  }

  expect_identical(
    length(unique(funDigest)),
    length(allFuns)
  )
})

# See if the add.to.hash argument really changes the signature
test_that("run.cache add to hash", {
  expect_message(
    .runCache(
      sum, 1, 2, 3, 4, 5,
      # run_cache arguments
      base_dir = withr::local_tempdir(),
      force_recalc = TRUE,
      show_message = TRUE,
      add_to_hash = "something"
    ),
    "Saving in cache"
  )
  expect_message(
    .runCache(
      sum, 1, 2, 3, 4, 5,
      # run_cache arguments
      base_dir = withr::local_tempdir(),
      force_recalc = TRUE,
      show_message = TRUE,
      add_to_hash = "other"
    ),
    "Saving in cache"
  )

  one <- capture_messages(
    .runCache(
      sum, 1, 2, 3, 4, 5,
      # run_cache arguments
      base_dir = withr::local_tempdir(),
      force_recalc = FALSE,
      show_message = TRUE,
      add_to_hash = "something"
    )
  )
  two <- capture_messages(
    .runCache(
      sum, 1, 2, 3, 4, 5,
      base_dir = withr::local_tempdir(),
      force_recalc = FALSE,
      show_message = TRUE,
      add_to_hash = "other"
    )
  )
  expect_false(all(one == two))
})

test_that("run.cache with seed", {
  baseDir <- withr::local_tempdir()

  expect_message(
    .runCache(
      rnorm, 1,
      # run_cache arguments
      seed = 10,
      base_dir = baseDir,
      force_recalc = TRUE,
      show_message = TRUE
    ),
    "Saving in cache"
  )
  expect_message(
    .runCache(
      rnorm, 1,
      seed = 11,
      # run_cache arguments
      base_dir = baseDir,
      force_recalc = TRUE,
      show_message = TRUE
    ),
    "Saving in cache"
  )
  expect_message(
    rnorm10 <- .runCache(
      rnorm, 1,
      # run_cache arguments
      seed = 10,
      base_dir = baseDir,
      force_recalc = FALSE,
      show_message = TRUE
    ),
    "Loading from cache"
  )
  expect_message(
    rnorm11 <- .runCache(
      rnorm, 1,
      # run_cache arguments
      seed = 11,
      base_dir = baseDir,
      force_recalc = FALSE,
      show_message = TRUE
    ),
    "Loading from cache"
  )

  expect_false(rnorm10 == rnorm11)
})

# nolint start: commented_code_linter.
# test_that("run.cache saves to local directory", {
#   output <- capture_output(
#     .runCache(
#       sum, 1, 2, 3, 4, 5,
#       base.dir = withr::local_tempdir(),
#       force.recalc = TRUE,
#       show.message = TRUE
#     )
#   )
#   expect_true(grepl(file.path('.', 'run-cache'), output))
# })
# nolint end: commented_code_linter.

test_that("run.cache uses cache", {
  baseDir <- withr::local_tempdir()
  .runCache(
    sum, 1, 2, 3, 4, 5,
    # run_cache arguments
    base_dir = baseDir,
    force_recalc = TRUE,
    show_message = FALSE
  )
  expect_message(
    .runCache(
      sum, 1, 2, 3, 4, 5,
      # run_cache arguments
      base_dir = baseDir,
      force_recalc = FALSE,
      show_message = TRUE
    ),
    "Loading from cache"
  )
})

test_that("run.cache show.message option works", {
  baseDir <- withr::local_tempdir()

  .showMessage(TRUE)
  expect_message(
    .runCache(
      sum, 1, 2, 3, 4, 5,
      # run_cache arguments
      base_dir = baseDir,
      force_recalc = TRUE
    ),
    "Saving in cache"
  )

  expect_message(
    .runCache(
      sum, 1, 2, 3, 4, 5,
      # run_cache arguments
      base_dir = baseDir, force_recalc = TRUE, show_message = FALSE
    ),
    NA
  )

  .showMessage(FALSE)
  expect_message(
    .runCache(
      sum, 1, 2, 3, 4, 5,
      # run_cache arguments
      base_dir = baseDir, force_recalc = TRUE
    ),
    NA
  )

  expect_message(
    .runCache(
      sum, 1, 2, 3, 4, 5,
      base_dir = baseDir, force_recalc = TRUE, show_message = TRUE
    ),
    "Saving in cache"
  )
})

test_that("run.cache base.dir option works", {
  cache0 <- file.path(withr::local_tempdir(), "run-cache")
  cache1 <- file.path(withr::local_tempdir(), "run-cache-changed1")
  cache2 <- file.path(withr::local_tempdir(), "run-cache-changed2")

  if (.Platform$OS.type == "windows") {
    cache0Os <- gsub("\\\\", "\\\\\\\\", cache0)
    cache1Os <- gsub("\\\\", "\\\\\\\\", cache0)
    cache2Os <- gsub("\\\\", "\\\\\\\\", cache0)
  } else {
    cache0Os <- cache0
    cache1Os <- cache1
    cache2Os <- cache2
  }

  expect_message(
    .runCache(
      sum, 1, 2, 3, 4, 5, 9,
      # run_cache arguments
      base_dir = cache0, force_recalc = FALSE, show_message = TRUE
    ),
    cache0Os
  )

  expect_message(
    .runCache(
      sum, 1, 2, 3, 4, 5, 8,
      # run_cache arguments
      base_dir = cache1, force_recalc = FALSE, show_message = TRUE
    ),
    cache1Os
  )

  expect_message(
    .runCache(
      sum, 1, 2, 3, 4, 5, 9,
      # run_cache arguments
      base_dir = cache0, force_recalc = FALSE, show_message = TRUE
    ),
    cache0Os
  )

  .baseDir(cache2)
  expect_message(
    .runCache(
      sum, 1, 2, 3, 4, 5,
      # run_cache arguments
      force_recalc = FALSE, show_message = TRUE
    ),
    cache2Os
  )
})
