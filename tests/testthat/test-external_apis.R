context("external_apis")

test_that("geneNames: gene name query", {
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

  geneNames(ensembl.genes = rownames(target)) |>
    lifecycle::expect_deprecated("ensemblGenes")
  geneNames(rownames(target), use.cache = TRUE) |>
    lifecycle::expect_deprecated("useCache")

  res <- geneNames(rownames(target))

  res$external_gene_name |>
    expect_equal(target[res$ensembl_gene_id, "gene.name"])
})

test_that("ensemblGeneNames: ensembl gene names query", {
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
    row.names = 2
  )

  ensemblGeneNames(gene.id = rownames(target)) |>
    lifecycle::expect_deprecated("geneId")
  ensemblGeneNames(rownames(target), use.cache = TRUE) |>
    lifecycle::expect_deprecated("useCache")

  res <- ensemblGeneNames(rownames(target))

  res$ensembl_gene_id |>
    expect_equal(target[res$external_gene_name, "ensembl"])
})

test_that("protein2EnsemblGeneNames: protein query", {
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

  protein2EnsemblGeneNames(ensembl.proteins = rownames(target)) |>
    lifecycle::expect_deprecated("ensemblProteins")
  protein2EnsemblGeneNames(rownames(target), use.cache = TRUE) |>
    lifecycle::expect_deprecated("useCache")

  res <- protein2EnsemblGeneNames(rownames(target))
  expect_equal(res$ensembl_gene_id, target[res$ensembl_peptide_id, "gene.name"])
})

# test .curlWorkaround
test_that("curl workaround", {
  dummy <- function(...) stop("Expected error +1")

  aa <- .curlWorkaround(dummy()) |>
    expect_warning("calling the function with ssl_verifypeer to FALSE") |>
    expect_error("Expected error \\+1")
})

test_that("(all): connection fails", {
  local_mocked_bindings(
    useEnsembl = function(...) list(),
    getBM = function(...) {
      stop("Error no connection")
    },
    .package = "biomaRt"
  )

  genes <- c("MOB1A", "SPIC", "RFLNB", "PTEN", "TP53")

  res <- geneNames(genes) |>
    expect_warning("calling the function with ssl_verifypeer to FALSE")

  res$ensembl_gene_id |>
    expect_equal(res$external_gene_name)

  res <- ensemblGeneNames(genes) |>
    expect_warning("calling the function with ssl_verifypeer to FALSE")

  res$ensembl_gene_id |>
    expect_equal(res$external_gene_name)

  res <- protein2EnsemblGeneNames(genes) |>
    expect_warning("calling the function with ssl_verifypeer to FALSE")

  res$ensembl_gene_id |>
    expect_equal(res$external_gene_name)
})

test_that(".biomartLoad: handles error gracefully", {
  local_mocked_bindings(
    useEnsembl = function(...) list(),
    getBM = function(useCache = TRUE, ...) {
      if (isFALSE(useCache)) "success" else NULL
    },
    .package = "biomaRt"
  )

  result <- .biomartLoad(list(), list(), list(), useCache = TRUE, FALSE) |>
    expect_warning("calling the function with ssl_verifypeer to FALSE") |>
    expect_equal("success")
})

