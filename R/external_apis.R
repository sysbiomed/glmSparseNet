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
#' glmSparseNet:::curl.workaround({
#'   biomaRt::useEnsembl(
#'     biomart = "genes",
#'     dataset = "hsapiens_gene_ensembl"
#'   )
#' })
#' }
curl.workaround <- function(expr) {
  result <- tryCatch(
    {
      expr
    },
    error = function(err) {
      err
    }
  )

  if (inherits(result, "error") || is.null(result)) {
    warning(
      "There was an problem, calling the function with ",
      "ssl_verifypeer to FALSE", "\n\n\t error: ", result$message
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
#' @param use.cache Boolean indicating if biomaRt cache should be used
#' @param verbose When using biomaRt in webservice mode and setting verbose to
#' TRUE, the XML query to the webservice will be printed.
#'
#' @return data.frame with attributes as columns and values translated to them
#'
#' @examples
#' glmSparseNet:::biomart.load(
#'   attributes = c("external_gene_name", "ensembl_gene_id"),
#'   filters = "external_gene_name",
#'   values = c("MOB1A", "RFLNB", "SPIC", "TP53"),
#'   use.cache = TRUE,
#'   verbose = FALSE
#' )
biomart.load <- function(
    attributes, filters, values, use.cache, verbose) {
  # local function that's used twice due to bug with curl

  mart <- curl.workaround({
    run.cache(
      biomaRt::useEnsembl,
      biomart = "genes",
      dataset = "hsapiens_gene_ensembl",
      host = "https://www.ensembl.org",
      verbose = verbose,
      # run.cache arguments
      cache.prefix = "biomart.useEnsembl",
      show.message = FALSE
    )
  })

  #
  results <- tryCatch(
    {
      curl.workaround(
        biomaRt::getBM(
          attributes = attributes,
          filters = filters,
          values = values,
          useCache = use.cache,
          verbose = verbose,
          mart = mart
        )
      )
    },
    error = function(error) {
      if (use.cache) {
        warning(
          "There was a problem getting the genes,",
          " trying without a cache.",
          "\n\t",
          error
        )
      } else {
        stop("There was a problem with biomaRt::getBM()", "\n\t", error)
      }
      warning(error)
    }
  )

  if ((inherits(results, "error") || is.null(results)) && use.cache) {
    # retrying without cache
    return(
      biomart.load(
        attributes = attributes,
        filters = filters,
        values = values,
        use.cache = FALSE,
        verbose = verbose
      )
    )
  }
  return(results)
}


#' Retrieve gene names from biomaRt
#'
#' @param ensembl.genes character vector with gene names in ensembl_id format
#' @param use.cache Boolean indicating if biomaRt cache should be used
#' @param verbose When using biomaRt in webservice mode and setting verbose to
#' TRUE, the XML query to the webservice will be printed.
#'
#' @return a dataframe with external gene names, ensembl_id
#' @export
#'
#' @examples
#' geneNames(c("ENSG00000114978", "ENSG00000166211", "ENSG00000183688"))
geneNames <- function(ensembl.genes, use.cache = TRUE, verbose = FALSE) {
  tryCatch(
    {
      results <- biomart.load(
        attributes = c("external_gene_name", "ensembl_gene_id"),
        filters = "ensembl_gene_id",
        values = ensembl.genes,
        use.cache = use.cache,
        verbose = verbose
      )

      #
      # Check if any genes does not have an external_gene_name
      #  and add them with same ensembl_id
      data.frame(
        external_gene_name = ensembl.genes[
          !ensembl.genes %in% results$ensembl_gene_id
        ],
        stringsAsFactors = FALSE
      ) |>
        dplyr::mutate(ensembl_gene_id = .data$external_gene_name) |>
        rbind(results) |>
        dplyr::arrange("external_gene_name")
    },
    error = function(msg) {
      warning(sprintf("Error when finding gene names:\n\t%s", msg))
      data.frame(
        ensembl_gene_id = ensembl.genes,
        external_gene_name = ensembl.genes,
        stringsAsFactors = FALSE
      )
    }
  )
}

#' Retrieve ensembl gene names from biomaRt
#'
#' @param gene.id character vector with gene names
#' @param use.cache Boolean indicating if biomaRt cache should be used
#' @param verbose When using biomaRt in webservice mode and setting verbose to
#' TRUE, the XML query to the webservice will be printed.
#'
#' @return a dataframe with external gene names, ensembl_id
#' @export
#'
#' @examples
#' ensemblGeneNames(c("MOB1A", "RFLNB", "SPIC", "TP53"))
ensemblGeneNames <- function(gene.id, use.cache = TRUE, verbose = FALSE) {
  tryCatch(
    {
      results <- biomart.load(
        attributes = c("external_gene_name", "ensembl_gene_id"),
        filters = "external_gene_name",
        values = gene.id,
        use.cache = use.cache,
        verbose = verbose
      )

      #
      # Check if any genes does not have an external_gene_name
      #  and add them with same ensembl_id

      data.frame(
        external_gene_name = gene.id[!gene.id %in% results$external_gene_name],
        stringsAsFactors = FALSE
      ) |>
        dplyr::mutate(ensembl_gene_id = .data$external_gene_name) |>
        rbind(results) |>
        dplyr::arrange("external_gene_name")
    },
    error = function(msg) {
      warning(sprintf("Error when finding gene names:\n\t%s", msg))
      data.frame(
        ensembl_gene_id = gene.id,
        external_gene_name = gene.id,
        stringsAsFactors = FALSE
      )
    }
  )
}

#' Retrieve hallmarks of cancer count for genes
#'
#' `r lifecycle::badge("defunct")`
#' The API has been removed and this function is no longer available.
#'
#' @param genes gene names
#' @param metric see below
#' @param hierarchy see below
#' @param generate.plot flag to indicate if return object has a ggplot2 object
#' @param show.message flag to indicate if run.cache method shows messages
#'
#' @return data.frame with choosen metric and hierarchy
#' It also returns a vector with genes that do not have any
#' hallmarks.
#'
#' See http://chat.lionproject.net/api for more details on the
#' metric and hallmarks parameters
#'
#' To standardize the colors in the gradient you can use
#' scale_fill_gradientn(limits=c(0,1), colours=topo.colors(3)) to
#' limit between 0 and 1 for cprob and -1 and 1 for npmi
#'
#' @export
#'
#' @examples
#' hallmarks(c("MOB1A", "RFLNB", "SPIC"))
#' \donttest{
#' hallmarks(c("MOB1A", "RFLNB", "SPIC"), metric = "cprob")
#' }
hallmarks <- function(
    genes,
    metric = "count",
    hierarchy = "full",
    generate.plot = TRUE,
    show.message = FALSE) {
  lifecycle::deprecate_stop(
    "1.21.0", "hallmarks()",
    details = "API is no longer available"
  )
}


#' Retrieve ensembl gene ids from proteins
#'
#' @param ensembl.proteins character vector with gene names in
#' ensembl_peptide_id format
#' @param use.cache Boolean indicating if biomaRt cache should be used
#' @param verbose When using biomaRt in webservice mode and setting verbose to
#' TRUE, the XML query to the webservice will be printed.
#'
#' @return a dataframe with external gene names, ensembl_peptide_id
#' @export
#'
#' @examples
#' protein2EnsemblGeneNames(c(
#'   "ENSP00000235382",
#'   "ENSP00000233944",
#'   "ENSP00000216911"
#' ))
protein2EnsemblGeneNames <- function(
    ensembl.proteins, use.cache = TRUE, verbose = FALSE) {
  #
  tryCatch(
    {
      biomart.load(
        attributes = c("ensembl_peptide_id", "ensembl_gene_id"),
        filters = "ensembl_peptide_id",
        values = ensembl.proteins,
        use.cache = use.cache,
        verbose = verbose
      ) |>
        dplyr::arrange("ensembl_peptide_id")
    },
    error = function(msg) {
      warning(sprintf("Error when finding gene names:\n\t%s", msg))
      data.frame(
        ensembl_peptide_id = ensembl.proteins,
        ensembl_gene_id    = ensembl.proteins,
        external_gene_name = ensembl.proteins,
        stringsAsFactors   = FALSE
      )
    }
  )
}
