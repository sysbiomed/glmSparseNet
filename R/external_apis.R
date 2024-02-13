#' Workaround for bug with curl when fetching specific ensembl mirror
#'
#' Should be solved in issue #39, will test to remove it.
#'
#' @param expr expression
#'
#' @return result of expression
#'
#' @examples
#' \donttest{
#' glmSparseNet:::.curlWorkaround({
#'     biomaRt::useEnsembl(
#'         biomart = "genes",
#'         dataset = "hsapiens_gene_ensembl"
#'     )
#' })
#' }
.curlWorkaround <- function(expr) {
    result <- tryCatch(expr, error = function(err) err)

    if (inherits(result, "error") || is.null(result)) {
        warning(
            "There was an problem, calling the function with ",
            "ssl_verifypeer to FALSE", "\n\n\t: ", result$message
        )

        result <- httr::with_config(
            config = httr::config(
                ssl_verifypeer = 0L,
                ssl_verifyhost = 0L,
                verbose = 1L
            ),
            {
                expr
            },
            override = FALSE
        )
    }

    return(result)
}

#' Common call to biomaRt to avoid repetitive code
#'
#' @seealso geneNames
#' @seealso ensemblGeneNames
#' @seealso protein2EnsemblGeneNames
#' @seealso biomaRt::getBM()
#' @seealso biomaRt::useEnsembl()
#'
#' @param attributes Attributes you want to retrieve. A possible list of
#' attributes can be retrieved using the function biomaRt::listAttributes.
#' @param filters Filters (one or more) that should be used in the query.
#' A possible list of filters can be retrieved using the function
#' biomaRt::listFilters.
#' @param values Values of the filter, e.g. vector of affy IDs. If multiple
#' filters are specified then the argument should be a list of vectors of
#' which the position of each vector corresponds to the position of the filters
#' in the filters argument
#' @param useCache Boolean indicating if biomaRt cache should be used
#' @param verbose When using biomaRt in webservice mode and setting verbose to
#' TRUE, the XML query to the webservice will be printed.
#'
#' @return data.frame with attributes as columns and values translated to them
#'
#' @examples
#' glmSparseNet:::.biomartLoad(
#'     attributes = c("external_gene_name", "ensembl_gene_id"),
#'     filters = "external_gene_name",
#'     values = c("MOB1A", "RFLNB", "SPIC", "TP53"),
#'     useCache = TRUE,
#'     verbose = FALSE
#' )
.biomartLoad <- function(attributes, filters, values, useCache, verbose) {
    # local function that's used twice due to bug with curl

    mart <- .curlWorkaround(
        .runCache(
            biomaRt::useEnsembl,
            biomart = "genes",
            dataset = "hsapiens_gene_ensembl",
            host = "https://www.ensembl.org",
            verbose = verbose,
            # run_cache arguments
            cachePrefix = "biomart.useEnsembl",
            showMessage = FALSE
        )
    )

    results <- tryCatch(
        .curlWorkaround(
            biomaRt::getBM(
                attributes = attributes,
                filters = filters,
                values = values,
                useCache = useCache,
                verbose = verbose,
                mart = mart
            )
        ),
        error = function(error) {
            if (useCache) {
                warning(
                    "There was a problem getting the genes,",
                    " trying without a cache.",
                    "\n\t",
                    error
                )
            } else {
                rlang::abort(
                    paste0(
                        "There was a problem with biomaRt::getBM()",
                        "\n\t",
                        error
                    )
                )
            }
            warning(error)
        }
    )

    if ((inherits(results, "error") || is.null(results)) && useCache) {
        # retrying without cache
        return(
            .biomartLoad(
                attributes = attributes,
                filters = filters,
                values = values,
                useCache = FALSE,
                verbose = verbose
            )
        )
    }
    results
}


#' Retrieve gene names from biomaRt
#'
#' @param ensemblGenes character vector with gene names in ensembl_id format
#' @param useCache Boolean indicating if biomaRt cache should be used
#' @param verbose When using biomaRt in webservice mode and setting verbose to
#' TRUE, the XML query to the webservice will be printed.
#' @param ensembl.genes `r lifecycle::badge("deprecated")`
#' @param use.cache `r lifecycle::badge("deprecated")`
#'
#' @return a dataframe with external gene names, ensembl_id
#' @export
#'
#' @examples
#' geneNames(c("ENSG00000114978", "ENSG00000166211", "ENSG00000183688"))
geneNames <- function(
        ensemblGenes,
        useCache = TRUE,
        verbose = FALSE,
        # Deprecated arguments with dots in name
        ensembl.genes = deprecated(), # nolint: object_name_linter.
        use.cache = deprecated()) { # nolint: object_name_linter.
    # Lifecycle management: to remove after 1.23.0
    if (lifecycle::is_present(ensembl.genes)) {
        .deprecatedDotParam("geneNames", "ensembl.genes")
        ensemblGenes <- ensembl.genes
    }
    if (lifecycle::is_present(use.cache)) {
        .deprecatedDotParam("geneNames", "use.cache")
        useCache <- use.cache
    }
    # Lifecycle management: end

    tryCatch(
        {
            results <- .biomartLoad(
                attributes = c("external_gene_name", "ensembl_gene_id"),
                filters = "ensembl_gene_id",
                values = ensemblGenes,
                useCache = useCache,
                verbose = verbose
            )

            #
            # Check if any genes does not have an external_gene_name
            #  and add them with same ensembl_id
            data.frame(
                external_gene_name = ensemblGenes[
                    !ensemblGenes %in% results$ensembl_gene_id
                ],
                stringsAsFactors = FALSE
            ) |>
                dplyr::mutate(ensembl_gene_id = .data$external_gene_name) |>
                rbind(results) |>
                dplyr::arrange("external_gene_name")
        },
        error = function(msg) {
            warning(sprintf("Problem when finding gene names:\n\t%s", msg))
            data.frame(
                ensembl_gene_id = ensemblGenes,
                external_gene_name = ensemblGenes,
                stringsAsFactors = FALSE
            )
        }
    )
}

#' Retrieve ensembl gene names from biomaRt
#'
#' @param geneId character vector with gene names
#' @param useCache Boolean indicating if biomaRt cache should be used
#' @param verbose When using biomaRt in webservice mode and setting verbose to
#' TRUE, the XML query to the webservice will be printed.
#' @param gene.id `r lifecycle::badge("deprecated")`
#' @param use.cache `r lifecycle::badge("deprecated")`
#'
#' @return a dataframe with external gene names, ensembl_id
#' @export
#'
#' @examples
#' ensemblGeneNames(c("MOB1A", "RFLNB", "SPIC", "TP53"))
ensemblGeneNames <- function(
        geneId,
        useCache = TRUE,
        verbose = FALSE,
        # Deprecated arguments with dots in name
        gene.id = deprecated(), # nolint: object_name_linter.
        use.cache = deprecated()) { # nolint: object_name_linter.
    # Lifecycle management: to remove after 1.23.0
    if (lifecycle::is_present(gene.id)) {
        .deprecatedDotParam("ensemblGeneNames", "gene.id")
        geneId <- gene.id
    }
    if (lifecycle::is_present(use.cache)) {
        .deprecatedDotParam("ensemblGeneNames", "use.cache")
        useCache <- use.cache
    }
    # Lifecycle management: end

    tryCatch(
        {
            results <- .biomartLoad(
                attributes = c("external_gene_name", "ensembl_gene_id"),
                filters = "external_gene_name",
                values = geneId,
                useCache = useCache,
                verbose = verbose
            )

            #
            # Check if any genes does not have an external_gene_name
            #  and add them with same ensembl_id

            data.frame(
                external_gene_name = geneId[
                    !geneId %in% results$external_gene_name
                ],
                stringsAsFactors = FALSE
            ) |>
                dplyr::mutate(ensembl_gene_id = .data$external_gene_name) |>
                rbind(results) |>
                dplyr::arrange("external_gene_name")
        },
        error = function(msg) {
            warning(sprintf("Problem when finding gene names:\n\t%s", msg))
            data.frame(
                ensembl_gene_id = geneId,
                external_gene_name = geneId,
                stringsAsFactors = FALSE
            )
        }
    )
}

#' Retrieve ensembl gene ids from proteins
#'
#' @param ensemblProteins character vector with gene names in
#' ensembl_peptide_id format
#' @param useCache Boolean indicating if biomaRt cache should be used
#' @param verbose When using biomaRt in webservice mode and setting verbose to
#' TRUE, the XML query to the webservice will be printed.
#' @param ensembl.proteins `r lifecycle::badge("deprecated")`
#' @param use.cache `r lifecycle::badge("deprecated")`
#'
#' @return a dataframe with external gene names, ensembl_peptide_id
#' @export
#'
#' @examples
#' protein2EnsemblGeneNames(c(
#'     "ENSP00000235382",
#'     "ENSP00000233944",
#'     "ENSP00000216911"
#' ))
protein2EnsemblGeneNames <- function(
        ensemblProteins,
        useCache = TRUE,
        verbose = FALSE,
        ensembl.proteins = deprecated(), # nolint: object_name_linter.
        use.cache = deprecated()) { # nolint: object_name_linter.
    # Lifecycle management: to remove after 1.23.0
    if (lifecycle::is_present(ensembl.proteins)) {
        .deprecatedDotParam("protein2EnsemblGeneNames", "ensembl.proteins")
        ensemblProteins <- ensembl.proteins
    }
    if (lifecycle::is_present(use.cache)) {
        .deprecatedDotParam("protein2EnsemblGeneNames", "use.cache")
        useCache <- use.cache
    }
    # Lifecycle management: end

    tryCatch(
        {
            .biomartLoad(
                attributes = c("ensembl_peptide_id", "ensembl_gene_id"),
                filters = "ensembl_peptide_id",
                values = ensemblProteins,
                useCache = useCache,
                verbose = verbose
            ) |>
                dplyr::arrange("ensembl_peptide_id")
        },
        error = function(msg) {
            warning(sprintf("Problem when finding gene names:\n\t%s", msg))
            data.frame(
                ensembl_peptide_id = ensemblProteins,
                ensembl_gene_id    = ensemblProteins,
                external_gene_name = ensemblProteins,
                stringsAsFactors   = FALSE
            )
        }
    )
}
