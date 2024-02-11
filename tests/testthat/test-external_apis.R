context("external_apis")

test_that("gene name query", {
  if (Sys.getenv("ONLINE_EXTERNAL_API") != "true") {
    local_mocked_bindings(
      useEnsembl = function(...) list(),
      getBM = function(...) {
        dplyr::tribble(
          ~external_gene_name, ~ensembl_gene_id,
          "MOB1A", "ENSG00000114978",
          "TP53", "ENSG00000141510",
          "SPIC", "ENSG00000166211",
          "PTEN", "ENSG00000171862",
          "RFLNB", "ENSG00000183688"
        )
      },
      .package = "biomaRt"
    )
  }

  target <- data.frame(
    ensembl = c(
      "ENSG00000114978",
      "ENSG00000166211",
      "ENSG00000183688",
      "ENSG00000171862",
      "ENSG00000141510"
    ),
    gene.name = c(
      "MOB1A",
      "SPIC",
      "RFLNB",
      "PTEN",
      "TP53"
    ),
    stringsAsFactors = FALSE,
    row.names = 1
  )

  res <- geneNames(rownames(target))

  res$external_gene_name |>
    expect_equal(target[res$ensembl_gene_id, "gene.name"])
})

test_that("protein query", {
  if (Sys.getenv("ONLINE_EXTERNAL_API") != "true") {
    local_mocked_bindings(
      useEnsembl = function(...) list(),
      getBM = function(...) {
        dplyr::tribble(
          ~ensembl_peptide_id, ~ensembl_gene_id,
          "ENSP00000269305", "ENSG00000141510",
          "ENSP00000331915", "ENSG00000183688",
          "ENSP00000361021", "ENSG00000171862",
          "ENSP00000379364", "ENSG00000114978"
        )
      },
      .package = "biomaRt"
    )
  }

  target <- data.frame(
    ensembl = c(
      "ENSP00000379364",
      "ENSP00000299272",
      "ENSP00000331915",
      "ENSP00000361021",
      "ENSP00000269305"
    ),
    gene.name = c(
      "ENSG00000114978",
      "ENSG00000166211",
      "ENSG00000183688",
      "ENSG00000171862",
      "ENSG00000141510"
    ),
    stringsAsFactors = FALSE,
    row.names = 1
  )

  res <- protein2EnsemblGeneNames(rownames(target))
  expect_equal(res$ensembl_gene_id, target[res$ensembl_peptide_id, "gene.name"])
})
