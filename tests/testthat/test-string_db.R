test_that("stringDBhomoSapiens: Will parse a valid (fake) dataset", {
  testthat::local_mocked_bindings(
    download.file = function(url, destfile, method, ...) {
      # write a file with some data
      readr::write_delim(prepare_mock_interactions(), destfile, delim = " ")
    },
    .package = "utils"
  )

  interactions <- stringDBhomoSapiens(scoreThreshold = 150)

  interactions |>
    expect_s3_class(c("tbl", "tbl_df", "data.frame"))

  interactions$combined_score |>
    expect_length(4) |>
    vapply(expect_gte, numeric(1L), 150)
})

test_that("buildStringNetwork: Can build network", {
  testthat::local_mocked_bindings(
    download.file = function(url, destfile, method, ...) {
      # write a file with some data
      readr::write_delim(prepare_mock_interactions(), destfile, delim = " ")
    },
    .package = "utils"
  )

  interactions <- stringDBhomoSapiens(scoreThreshold = 150)

  # test deprecated parameters
  buildStringNetwork(string.tbl = interactions) |>
    lifecycle::expect_deprecated("stringTbl")
  buildStringNetwork(interactions, use.names = "protein") |>
    lifecycle::expect_deprecated("useNames")

  network <- buildStringNetwork(interactions)

  network |>
    expect_s4_class(c("dgCMatrix")) |>
    expect_length(64)

  (as.vector(network) != 0) |>
    sum() |>
    expect_equal(4)
})
