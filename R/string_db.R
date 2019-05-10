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
#' \dontrun{
#'     stringDBhomoSapiens(score_threshold = 800)
#' }
stringDBhomoSapiens <- function(version = '10', score_threshold = 0,
                                remove.text = TRUE) {

    . <- NULL

    STRINGdb::get_STRING_species(version = version, species_name=NULL) %>%
        dplyr::arrange(!!(as.name('official_name'))) %>%
        dplyr::filter(!!(as.name('official_name')) == 'homo_sapiens')

    # downloading Homo sapiens
    string_db <- STRINGdb::STRINGdb$new(version         = version,
                                        species         = 9606,
                                        score_threshold = 0)

    # Load to memory the database (by calling a method)
    tp53 <- string_db$mp( "tp53" )

    # get all interactions
    all.interactions <- string_db$proteins$protein_external_id %>%
        string_db$get_interactions()

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

            for (ix in c('cooccurence', 'textmining',
                         'textmining_transferred')) {
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

    all.interactions$combined_score <- combined.score
    interactions <- all.interactions %>%
        dplyr::filter(!!(as.name('combined_score')) >= score_threshold)

    return(interactions)
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
#' \dontrun{
#'   all.interactions.700 <- stringDBhomoSapiens(score_threshold = 700)
#'   string.network       <- buildStringNetwork(all.interactions.700,
#'                                               use.names = 'external')
#'   # number of edges
#'   sum(network != 0)
#' }
buildStringNetwork <- function(string.tbl, use.names = 'protein') {

    # remove 9606. prefix
    string.tbl$from <- gsub('9606\\.', '', string.tbl$from)
    string.tbl$to   <- gsub('9606\\.', '', string.tbl$to)

    # get sorted list of proteins
    merged.prot <- sort(unique(c(string.tbl$from, string.tbl$to)))

    # if use.names is not default, then replace proteins with genes (either
    #   ensembl_id or gene_name)
    if (use.names == 'ensembl' || use.names == 'external') {
        prot.map           <- protein2EnsemblGeneNames(merged.prot)
        rownames(prot.map) <- prot.map$ensembl_peptide_id

        # use external gene names
        if (use.names == 'external') {
            ext.genes <- prot.map$ensembl_gene_id %>% unique %>% geneNames
            rownames(ext.genes) <- ext.genes$ensembl_gene_id

            prot.map$ensembl_gene_id <- ext.genes[prot.map$ensembl_gene_id,
                                                  'external_gene_name']
        }

        # keep only proteins that have mapping to gene
        new.string <- string.tbl %>%
           dplyr::filter(!!(as.name('from')) %in%
                             prot.map$ensembl_peptide_id &
                         !!(as.name('to')) %in%
                             prot.map$ensembl_peptide_id)

        # replace protein with genes
        new.string$from <- as.vector(prot.map[new.string$from,
                                              'ensembl_gene_id'])
        new.string$to   <- as.vector(prot.map[new.string$to,'ensembl_gene_id'])

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
    new.string$to   <- readr::parse_factor(new.string$to, merged.prot)

    i <- as.numeric(new.string$from)
    j <- as.numeric(new.string$to)

    # Create new sparse matrix with p x p dimensions (p = genes)
    new.mat <- Matrix::sparseMatrix(i        = i,
                                    j        = j,
                                    x        = new.string$combined_score,
                                    dims     = array(length(merged.prot), 2),
                                    dimnames = list(levels(new.string$from),
                                                    levels(new.string$to)))

    # return the new matrix
    return(new.mat)
}
