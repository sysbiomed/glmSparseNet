#' Calculate combined score for STRINGdb interactions
#'
#' Please note that all the interactions have duplicates as it's
#'  a two way interaction (score(ProteinA-Protein) == score(ProteinB, PorteinA))
#'
#' To better understand how the score is calculated, please see:
#'  https://string-db.org/help/faq/#how-are-the-scores-computed
#'
#' @param all.interactions table with score of all interactions
#' @param score_threshold threshold to keep interactions
#' @param remove.text remove text-based interactions
#'
#' @return table with combined score
.calculateCombinedScore <- function(
    allInteractions,
    scoreThreshold,
    removeText) {
  # We manually compute using the guide in stringdb's faq
  prior <- 0.041

  computePrior <- function(score, prior) {
    score[score < prior] <- prior
    (score - prior) / (1 - prior)
  }

  # normalize between 0 and 1
  mat <- Matrix::as.matrix(
    dplyr::select(allInteractions, -dplyr::starts_with("protein"))
  ) / 1000
  non_homology.ixs <- which(colnames(mat) != "homology")

  # compute prior away
  mat[, non_homology.ixs] <- computePrior(mat[, non_homology.ixs], prior)

  # then, combine the direct and transferred scores for each category:
  combined <- c(
    "neighborhood", "coexpression", "experiments",
    "database", "textmining"
  )

  for (ix in combined) {
    mat[, ix] <- 1 -
      (1 - mat[, ix]) *
        (1 - mat[, paste0(ix, "_transferred", sep = "")])
  }

  # now, do the homology correction on cooccurrence and textmining:
  for (ix in c("cooccurence", "textmining")) {
    mat[, ix] <- mat[, ix] * (1 - mat[, "homology"])
  }

  # Select only calculated columns
  mat <- mat[, c(
    "neighborhood", "fusion", "cooccurence", "coexpression",
    "experiments", "database", "textmining"
  )]

  # next, do the 1 - multiplication
  combinedScore <- (1 - mat[, "neighborhood"]) *
    (1 - mat[, "fusion"]) *
    (1 - mat[, "cooccurence"]) *
    (1 - mat[, "coexpression"]) *
    (1 - mat[, "experiments"])

  if (isFALSE(removeText)) {
    combinedScore <- combinedScore *
      (1 - mat[, "database"]) *
      (1 - mat[, "textmining"])
  }

  # and lastly, do the 1 - conversion again, and put back the prior
  #  *exactly once*
  combinedScore <- prior + (1 - prior) * (1 - combinedScore)

  allInteractions |>
    dplyr::select(from = "protein1", to = "protein2") |>
    dplyr::mutate(combined_score = floor(combinedScore * 1000)) |>
    #
    # Refilter combined score
    dplyr::filter(.data$combined_score >= scoreThreshold)
}

#' Download protein-protein interactions from STRING DB
#'
#' @param version version of the database to use
#' @param score_threshold remove scores below threshold
#' @param remove.text remove text mining-based scores
#'
#' @return a data.frame with rows representing an interaction between two
#' proteins, and columns
#' the count of scores above the given score_threshold
#'
#' @export
#' @examples
#' \donttest{
#' stringDBhomoSapiens(scoreThreshold = 800)
#' }
stringDBhomoSapiens <- function(
    version = "11.0",
    scoreThreshold = 0,
    removeText = TRUE) {
  # nolint start: object_usage_linter
  species <- 9606 # Homo sapiens
  links <- "links.full" # what data to retrieve

  # Using multiple glue to keep line length short
  urlDomain <- "stringdb-static.org"
  urlPath <- "download/protein.{links}.v{version}" |> glue::glue()
  urlFile <- "{species}.protein.{links}.v{version}.txt.gz" |> glue::glue()
  url <- "https://{urlDomain}/{urlPath}/{urlFile}" |> glue::glue()
  # nolint end: object_usage_linter

  # download string data from string-db.org
  #
  # We were using the package before, but the full dataset was
  #  no longer available. So had to do it manually
  allInteractions <- downloadFileLocal(url, oD = tempdir()) |>
    # read file into table
    readr::read_delim(delim = " ") |>
    # remove combined score, as we are calculating ourselves
    dplyr::select(-!!as.name("combined_score"))

  .calculateCombinedScore(allInteractions, scoreThreshold, removeText)
}

#' Build gene network from peptide ids
#'
#' This can reduce the dimension of the original network, as there may not be a
#' mapping
#' between peptide and gene id
#'
#' @param stringTbl `data.frame` or `tibble` with colnames and rownames as
#' ensembl peptide id _(same order)_.
#' @param useNames `character(1)` that defaults to use protein names
#' _('protein'), other options are 'ensembl' for ensembl gene id or 'external'
#' for external gene names.
#' @param string.tbl `r lifecycle::badge("deprecated")`
#' @param use.names `r lifecycle::badge("deprecated")`
#'
#' @return a new matrix with gene ids instead of peptide ids. The size of matrix
#'  can be different as
#' there may not be a mapping or a peptide mapping can have multiple genes.
#' @export
#' @seealso stringDBhomoSapiens
#' @examples
#' \donttest{
#' interactions <- stringDBhomoSapiens(scoreThreshold = 100)
#' string_network <- buildStringNetwork(interactions)
#'
#' # number of edges
#' sum(string_network != 0)
#' }
buildStringNetwork <- function(
    stringMatrix,
    useNames = c("protein", "ensembl", "external"),
    string.tbl = deprecated(),
    use.names = deprecated()
  ) {
  # Lifecycle management: to remove after 1.23.0
  if (lifecycle::is_present(string.tbl)) {
    .deprecatedDotParam("buildStringNetwork", "string.tbl")
    stringMatrix <- string.tbl
  }
  if (lifecycle::is_present(use.names)) {
    .deprecatedDotParam("buildStringNetwork", "use.names")
    useNames <- use.names
  }
  # Lifecycle management: end

  useNames <- match.arg(useNames)

  # remove 9606. prefix
  stringMatrix$from <- gsub("9606\\.", "", stringMatrix$from)
  stringMatrix$to <- gsub("9606\\.", "", stringMatrix$to)

  # get sorted list of proteins
  mergedProt <- sort(unique(c(stringMatrix$from, stringMatrix$to)))

  # if useNames is not default, then replace proteins with genes (either
  #   ensembl_id or gene_name)
  if (useNames == "ensembl" || useNames == "external") {
    protMap <- protein2EnsemblGeneNames(mergedProt)
    rownames(protMap) <- protMap$ensembl_peptide_id

    # use external gene names
    if (useNames == "external") {
      extGenes <- protMap$ensembl_gene_id |>
        unique() |>
        geneNames()
      rownames(extGenes) <- extGenes$ensembl_gene_id

      protMap$ensembl_gene_id <- extGenes[
        protMap$ensembl_gene_id,
        "external_gene_name"
      ]
    }

    # keep only proteins that have mapping to gene
    newString <- stringMatrix |>
      dplyr::filter(
        !!(as.name("from")) %in% protMap$ensembl_peptide_id &
          !!(as.name("to")) %in% protMap$ensembl_peptide_id
      )

    # empty gene ids default to previous code
    protMap <- protMap |>
      dplyr::mutate(
        ensembl_gene_id =
          dplyr::if_else(!!(as.name("ensembl_gene_id")) == "",
            !!(as.name("ensembl_peptide_id")),
            !!(as.name("ensembl_gene_id"))
          )
      )

    # replace protein with genes
    newString$from <- as.vector(protMap[
      newString$from,
      "ensembl_gene_id"
    ])
    newString$to <- as.vector(protMap[newString$to, "ensembl_gene_id"])

    # discard all interaction between gene and himself
    newString <- newString[newString$from != newString$to, ]

    # update list of protein index with genes
    mergedProt <- sort(unique(c(newString$from, newString$to)))
  } else {
    # if default then just pass the argument as newString
    newString <- stringMatrix
  }

  #
  # Build sparse matrix

  newString$from <- readr::parse_factor(newString$from, mergedProt)
  newString$to <- readr::parse_factor(newString$to, mergedProt)

  i <- as.numeric(newString$from)
  j <- as.numeric(newString$to)

  # Create new sparse matrix with p x p dimensions (p = genes)
  Matrix::sparseMatrix(
    i = i,
    j = j,
    x = newString$combined_score,
    dims = array(length(mergedProt), 2),
    dimnames = list(
      levels(newString$from),
      levels(newString$to)
    )
  )
}
