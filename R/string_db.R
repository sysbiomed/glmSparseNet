#' Download protein-protein interactions from STRING DB (for a specific species)
#'
#' @param version version of the database to use
#' @param scrore_threshold remove
#'
#' @return a data.frame with rows representing an interaction between two proteins, and columns
#' the count of scores above the given score_threshold
#'
#' @export
string.db.homo.sapiens <- function(version = '10', score_threshold = 0, remove.text = TRUE) {
  STRINGdb::get_STRING_species(version = version, species_name=NULL) %>%
    dplyr::arrange(official_name) %>%
    dplyr::filter(official_name == 'homo_sapiens')

  # downloading Homo sapiens
  string_db <- STRINGdb$new(version         = version,
                            species         = 9606,
                            score_threshold = score_threshold)

  # Load to memory the database (by calling a method)
  tp53 <- string_db$mp( "tp53" )
  atm  <- string_db$mp( "atm" )

  # get all interactions
  all.interactions <- as.tbl(string_db$get_interactions(string_db$proteins$protein_external_id))

  # remove text.based columns
  if (remove.text) {
    col.ixs <- colnames(all.interactions) %>%
    {
      !. %in% .[grep('text', .)]
    }
  } else {
    col.ixs <- seq(ncol(all.interactions))
  }

  return(all.interactions[, col.ixs])
}
