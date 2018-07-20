#' Download protein-protein interactions from STRING DB (for a specific species)
#'
#' @param version version of the database to use
#' @param score_threshold remove scores below threshold
#' @param remove.text remove text mining-based scores
#'
#' @return a data.frame with rows representing an interaction between two proteins, and columns
#' the count of scores above the given score_threshold
#'
#' @export
string.db.homo.sapiens <- function(version = '10', score_threshold = 0, remove.text = TRUE) {

  . <- NULL

  STRINGdb::get_STRING_species(version = version, species_name=NULL) %>%
    dplyr::arrange(rlang::UQ(as.name('official_name'))) %>%
    dplyr::filter(rlang::UQ(as.name('official_name')) == 'homo_sapiens')

  # downloading Homo sapiens
  string_db <- STRINGdb::STRINGdb$new(version         = version,
                                      species         = 9606,
                                      score_threshold = 0)

  # Load to memory the database (by calling a method)
  tp53 <- string_db$mp( "tp53" )

  # get all interactions
  all.interactions <- string_db$get_interactions(string_db$proteins$protein_external_id)

  # remove text.based columns
  if (remove.text) {
    col.ixs <- colnames(all.interactions) %>%
    {
      !. %in% .[grep('text', .)]
    }
  } else {
    col.ixs <- array(TRUE, ncol(all.interactions))
  }

  # remove text if flag is TRUE
  all.interactions <- all.interactions[, col.ixs]

  col.ixs <- (!colnames(all.interactions) %in% 'combined_score')

  # columns without from and to
  col.vals.ixs <- col.ixs & (!colnames(all.interactions) %in% c('from', 'to'))

  mat <- as.matrix(all.interactions[,col.vals.ixs]) / 1000

  #
  # Calculate new combined score
  #  https://string-db.org/help/faq/#how-are-the-scores-computed

  p <- 0.041
  combined.score <- mat %>%
    #
    # Removing prior per channel
    apply( 2, function(ix) {
      res <- (ix - p) / (1 - p)
      res[ix == 0] <- 0
      return(res)
    }) %>% {
      #
      # Normalize Co-Occurence and Text mining scores
      #  https://string-db.org/help/faq/#how-are-the-scores-computed

      for (ix in c('cooccurence', 'textmining', 'textmining_transferred')) {
        if (ix %in% colnames(.)) {
          .[, ix] <- .[, ix] * (1 - .[, 'homology'])
        }
      }
      .[,!colnames(.) %in% c('homology')]
    } %>% {
      #
      # Combine the scores of the channels
      #
      # 1 - factor(1 - S_i)
      res <- (1 - .[,1])
      for (ix in seq(ncol(.))[-1]) {
        res <- res * (1 - .[,ix])
      }
      1 - res
    } %>% {
      #
      # Add prior (once)
      . + p * (1 - .)
    } %>% {
      #
      # Reconvert to integer
      floor(. * 1000)
    }

  #
  # Refilter combined score

  all.interactions$combined_score.new <- combined.score
  interactions <- all.interactions %>%
    dplyr::filter(rlang::UQ(as.name('combined_score.new')) >= score_threshold)

  #
  # Build sparse matrix

  merged.prot <- sort(unique(c(interactions$from, interactions$to)))

  interactions$from <- readr::parse_factor(interactions$from, merged.prot)
  interactions$to   <- readr::parse_factor(interactions$to, merged.prot)

  levels(interactions$from) <- levels(interactions$from) %>% { gsub('9606\\.', '', .) }
  levels(interactions$to)   <- levels(interactions$to) %>% { gsub('9606\\.', '', .) }

  i <- as.numeric(interactions$from)
  j <- as.numeric(interactions$to)

  # Create new sparse matrix with p x p dimensions (p = genes)
  new.mat <- Matrix::sparseMatrix(i        = i,
                                  j        = j,
                                  x        = interactions$combined_score.new,
                                  dims     = array(length(merged.prot), 2),
                                  dimnames = list(levels(interactions$from),
                                                  levels(interactions$to)))

  return(list(network = new.mat, interactions = interactions))
}

#' Build gene network from peptide ids
#'
#' This can reduce the dimension of the original network, as there may not be a mapping
#' between peptide and gene id
#'
#' @param protein.network matrix with colnames and rownames as ensembl peptide id (same order)
#' @param use.external.names use external gene names instead of ensembl gene id
#'
#' @return a new matrix with gene ids instead of peptide ids. The size of matrix can be different as
#' there may not be a mapping or a peptide mapping can have multiple genes.
#' @export
#' @seealso string.db.homo.sapiens
build.string.gene.network <- function(protein.network, use.external.names = FALSE) {

  # get mapping from ensembl protein-gene
  gene.tbl <- protein.to.ensembl.gene.names(colnames(protein.network))

  # create new matrix with columns as genes
  new.mat <- protein.network[gene.tbl$ensembl_peptide_id, gene.tbl$ensembl_peptide_id]

  message(sprintf('Proteins mapped to genes: %d out of %d in STRING protein-protein interactions (%d discarded)',
                  nrow(new.mat),
                  nrow(protein.network),
                  sum(!colnames(protein.network) %in% gene.tbl$ensembl_peptide_id)))

  # change name of rows and columns to gene (either ensembl gene id or gene name)
  if (use.external.names) {
    colnames(new.mat) <- gene.tbl$external_gene_name
    rownames(new.mat) <- gene.tbl$external_gene_name
  } else {
    colnames(new.mat) <- gene.tbl$ensembl_gene_id
    rownames(new.mat) <- gene.tbl$ensembl_gene_id
  }

  # return the new matrix
  return(new.mat)
}
