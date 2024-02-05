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
calculate.combined.score <- function(all.interactions,
                                     score_threshold,
                                     remove.text) {
  # We manually compute using the guide in stringdb's faq
  prior <- 0.041

  compute.prior <- function(score, prior) {
    score[score < prior] <- prior
    return((score - prior) / (1 - prior))
  }

  # normalize between 0 and 1
  mat <- as.matrix(all.interactions |>
    dplyr::select(-dplyr::starts_with("protein"))) / 1000
  non_homology.ixs <- which(colnames(mat) != "homology")

  # compute prior away
  mat[, non_homology.ixs] <- compute.prior(mat[, non_homology.ixs], prior)

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
  combined_score <-
    (1 - mat[, "neighborhood"]) *
      (1 - mat[, "fusion"]) *
      (1 - mat[, "cooccurence"]) *
      (1 - mat[, "coexpression"]) *
      (1 - mat[, "experiments"])

  if (!remove.text) {
    combined_score <-
      combined_score *
        (1 - mat[, "database"]) *
        (1 - mat[, "textmining"])
  }

  # and lastly, do the 1 - conversion again, and put back the prior
  #  *exactly once*
  combined_score <- prior + (1 - prior) * (1 - combined_score)

  return(all.interactions |>
    dplyr::select(
      from = !!as.name("protein1"),
      to = !!as.name("protein2")
    ) |>
    dplyr::mutate(combined_score = floor(combined_score * 1000)) |>
    #
    # Refilter combined score
    dplyr::filter(!!(as.name("combined_score")) >= score_threshold))
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
#' stringDBhomoSapiens(score_threshold = 800)
#' }
stringDBhomoSapiens <- function(version = "11.0",
                                score_threshold = 0,
                                remove.text = TRUE) {
  species <- 9606 # Homo sapiens
  links <- "links.full" # what data to retrieve

  # Using multiple glue to keep line length short
  url.domain <- "stringdb-static.org"
  url.path <- "download/protein.{links}.v{version}" |> glue::glue()
  url.file <- "{species}.protein.{links}.v{version}.txt.gz" |> glue::glue()
  url <- "https://{url.domain}/{url.path}/{url.file}" |> glue::glue()

  # download string data from string-db.org
  #
  # We were using the package before, but the full dataset was
  #  no longer available. So had to do it manually
  all.interactions <- downloadFileLocal(url, oD = tempdir()) |>
    # read file into table
    readr::read_delim(delim = " ") |>
    # remove combined score, as we are calculating ourselves
    dplyr::select(-!!as.name("combined_score"))

  return(calculate.combined.score(
    all.interactions,
    score_threshold,
    remove.text
  ))
}

#' Build gene network from peptide ids
#'
#' This can reduce the dimension of the original network, as there may not be a
#' mapping
#' between peptide and gene id
#'
#' @param string.tbl matrix with colnames and rownames as ensembl peptide id
#' (same order)
#' @param use.names default is to use protein names ('protein'), other options
#' are 'ensembl' for ensembl
#' gene id or 'external' for external gene names
#'
#' @return a new matrix with gene ids instead of peptide ids. The size of matrix
#'  can be different as
#' there may not be a mapping or a peptide mapping can have multiple genes.
#' @export
#' @seealso stringDBhomoSapiens
#' @examples
#' \donttest{
#' all.interactions.700 <- stringDBhomoSapiens(score_threshold = 700)
#' string.network <- buildStringNetwork(all.interactions.700,
#'   use.names = "external"
#' )
#' # number of edges
#' sum(string.network != 0)
#' }
buildStringNetwork <- function(string.tbl, use.names = "protein") {
  # remove 9606. prefix
  string.tbl$from <- gsub("9606\\.", "", string.tbl$from)
  string.tbl$to <- gsub("9606\\.", "", string.tbl$to)

  # get sorted list of proteins
  merged.prot <- sort(unique(c(string.tbl$from, string.tbl$to)))

  # if use.names is not default, then replace proteins with genes (either
  #   ensembl_id or gene_name)
  if (use.names == "ensembl" || use.names == "external") {
    prot.map <- protein2EnsemblGeneNames(merged.prot)
    rownames(prot.map) <- prot.map$ensembl_peptide_id

    # use external gene names
    if (use.names == "external") {
      ext.genes <- prot.map$ensembl_gene_id |>
        unique() |>
        geneNames()
      rownames(ext.genes) <- ext.genes$ensembl_gene_id

      prot.map$ensembl_gene_id <- ext.genes[
        prot.map$ensembl_gene_id,
        "external_gene_name"
      ]
    }

    # keep only proteins that have mapping to gene
    new.string <- string.tbl |>
      dplyr::filter(!!(as.name("from")) %in%
        prot.map$ensembl_peptide_id &
        !!(as.name("to")) %in%
          prot.map$ensembl_peptide_id)

    # empty gene ids default to previous code
    prot.map <- prot.map |>
      dplyr::mutate(
        ensembl_gene_id =
          dplyr::if_else(!!(as.name("ensembl_gene_id")) == "",
            !!(as.name("ensembl_peptide_id")),
            !!(as.name("ensembl_gene_id"))
          )
      )

    # replace protein with genes
    new.string$from <- as.vector(prot.map[
      new.string$from,
      "ensembl_gene_id"
    ])
    new.string$to <- as.vector(prot.map[new.string$to, "ensembl_gene_id"])

    # discard all interaction between gene and himself
    new.string <- new.string[new.string$from != new.string$to, ]

    # update list of protein index with genes
    merged.prot <- sort(unique(c(new.string$from, new.string$to)))
  } else {
    # if default then just pass the argument as new.string
    new.string <- string.tbl
  }

  #
  # Build sparse matrix

  new.string$from <- readr::parse_factor(new.string$from, merged.prot)
  new.string$to <- readr::parse_factor(new.string$to, merged.prot)

  i <- as.numeric(new.string$from)
  j <- as.numeric(new.string$to)

  # Create new sparse matrix with p x p dimensions (p = genes)
  new.mat <- Matrix::sparseMatrix(
    i = i,
    j = j,
    x = new.string$combined_score,
    dims = array(length(merged.prot), 2),
    dimnames = list(
      levels(new.string$from),
      levels(new.string$to)
    )
  )

  # return the new matrix
  return(new.mat)
}
