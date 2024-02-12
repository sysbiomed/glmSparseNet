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

test_that("buildStringNetwork: Can build network with external and ensembl", {
  testthat::local_mocked_bindings(
    protein2EnsemblGeneNames = function(ensemblProteins, ...) {
      data.frame(
        ensembl_peptide_id = ensemblProteins,
        ensembl_gene_id    = paste0("ens_", ensemblProteins),
        external_gene_name = paste0("gene_", ensemblProteins),
        stringsAsFactors   = FALSE
      )
    },
    geneNames = function(ensemblGenes, ...) {
      data.frame(
        ensembl_gene_id = ensemblGenes,
        external_gene_name = paste0("gene_", ensemblGenes),
        stringsAsFactors = FALSE
      )
    }
  )

  interactions <- stringDBhomoSapiens(scoreThreshold = 150)
  (network1 <- buildStringNetwork(interactions, useNames = "external")) |>
    colnames() |>
    expect_match("gene_")

  (network2 <- buildStringNetwork(interactions, useNames = "ensembl")) |>
    colnames() |>
    expect_match("ens_")

  network1 |>
    expect_s4_class(c("dgCMatrix")) |>
    expect_length(64)

  network2 |>
    expect_s4_class(c("dgCMatrix")) |>
    expect_length(64)

  (as.vector(network1) != 0) |>
    sum() |>
    expect_equal(4)

  (as.vector(network2) != 0) |>
    sum() |>
    expect_equal(4)
})

test_that("calculateCombinedScore: removeText will return different score", {
  testthat::local_mocked_bindings(
    download.file = function(url, destfile, method, ...) {
      # write a file with some data
      readr::write_delim(prepare_mock_interactions(), destfile, delim = " ")
    },
    .package = "utils"
  )

  score1 <- stringDBhomoSapiens(scoreThreshold = 150, removeText = TRUE) |>
    magrittr::extract2("combined_score")
  score2 <- stringDBhomoSapiens(scoreThreshold = 150, removeText = FALSE) |>
    magrittr::extract2("combined_score")

  expect_failure(
    expect_equal(score1, score2, tolerance = 1e-6)
  )
})
