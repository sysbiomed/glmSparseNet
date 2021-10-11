#' Workaround for bug with curl when fetching specific ensembl mirror
#' 
#' Should be solved in issue #39, will test to remove it.
#'
#' @param expr expression
#'
#' @return result of expression
#' @export
#'
#' @examples
#' \donttest{
#' glmSparseNet:::curl.workaround({
#'     biomaRt::useEnsembl(
#'         biomart = "genes", 
#'         dataset = 'hsapiens_gene_ensembl')
#' })
#' }
curl.workaround <- function(expr) {
    result <- tryCatch(
        {expr}, 
        error = function(err) {
        err
    })
    
    if (inherits(result, 'error') || is.null(result)) {
        warning(
            "There was an problem, calling the function with ",
            "ssl_verifypeer to FALSE", "\n\n\t error: ", result$message)
        # httr::set_config(httr::config(
        #    ssl_verifypeer = 0L, 
        #    ssl_verifyhost = 0L, 
        #    verbose = 0L))
        result <- httr::with_config(
            config = httr::config(
                ssl_verifypeer = 0L, 
                ssl_verifyhost = 0L, 
                verbose = 1L
            ),
            {expr},
            override = FALSE
        )
        # httr::reset_config()
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
#'     attributes = c("external_gene_name","ensembl_gene_id"), 
#'     filters = "external_gene_name", 
#'     values = c('MOB1A','RFLNB', 'SPIC', 'TP53'),
#'     use.cache = TRUE,
#'     verbose = FALSE
#' )
biomart.load <- function(
    attributes, filters, values, use.cache, verbose
) {

    
    # local function that's used twice due to bug with curl
    
    
    mart <- curl.workaround({
        loose.rock::run.cache(
            biomaRt::useEnsembl,
            biomart = "genes",
            dataset = 'hsapiens_gene_ensembl',
            host = 'https://www.ensembl.org',
            verbose = verbose,
            # loose.rock::run.cache arguments
            cache.prefix = 'biomart.useEnsembl',
            show.message = FALSE
        )
    })
    
    #
    results <- tryCatch({
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
    }, error = function(error) {
        if (use.cache) {
            warning(
                'There was a problem getting the genes,',
                ' trying without a cache.', 
                '\n\t',
                error
            )
        } else {
            stop('There was a problem with biomaRt::getBM()', '\n\t', error)
        }
        warning(error)
    })
    
    if ((inherits(results, 'error') || is.null(results)) && use.cache) {
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
#' geneNames(c('ENSG00000114978','ENSG00000166211', 'ENSG00000183688'))
geneNames <- function(ensembl.genes, use.cache = TRUE, verbose = FALSE) {

    . <- NULL

    tryCatch({
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

        results <- ensembl.genes[!ensembl.genes %in%
                                     results$ensembl_gene_id] %>%
            {
                data.frame(
                    external_gene_name = .,
                    ensembl_gene_id    = .,
                    stringsAsFactors   = FALSE
                )
            } %>% rbind(results)

        return(
            dplyr::arrange(results, !!(as.name('external_gene_name')))
        )
    }, error = function(msg) {
        warning(sprintf('Error when finding gene names:\n\t%s', msg))
    })
    return(
        data.frame(
            ensembl_gene_id = ensembl.genes,
            external_gene_name = ensembl.genes,
            stringsAsFactors = FALSE
        )
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
#' \donttest{
#'     ensemblGeneNames(c('MOB1A','RFLNB', 'SPIC', 'TP53'))
#' }
ensemblGeneNames <- function(gene.id, use.cache = TRUE, verbose = FALSE) {

    . <- NULL

    tryCatch({
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

        results <- gene.id[!gene.id %in% results$external_gene_name] %>%
            {
                data.frame(
                    external_gene_name = .,
                    ensembl_gene_id    = .,
                    stringsAsFactors   = FALSE
                )
            } %>% rbind(results)

        return(
            dplyr::arrange(results, !!(as.name('external_gene_name')))
        )
    }, error = function(msg) {
        warning(sprintf('Error when finding gene names:\n\t%s', msg))
    })
    return(
        data.frame(
            ensembl_gene_id = gene.id, 
            external_gene_name = gene.id,
            stringsAsFactors = FALSE
        )
    )
}

#' Retrieve hallmarks of cancer count for genes
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
#' \donttest{
#'     hallmarks(c('MOB1A', 'RFLNB', 'SPIC'))
#'     hallmarks(c('MOB1A', 'RFLNB', 'SPIC'), metric = 'cprob')
#' }
hallmarks <- function(
    genes, 
    metric = 'count', 
    hierarchy = 'full',
    generate.plot = TRUE, 
    show.message = FALSE
) {
    #
    valid.measures <- c('count', 'cprob', 'pmi', 'npmi')
    if (!metric %in% valid.measures) {
        stop('measure argument is not valid, it must be one of the following: ',
              paste(valid.measures, collapse = ', ')
        )
    }


    all.genes <- sort(unique(genes))

    #
    # necessary due to https://github.com/cambridgeltl/chat/issues/6
    if (metric == 'cprob') {
        temp.res        <- hallmarks(all.genes, metric = 'count',
                                     hierarchy = 'full', show.message = FALSE,
                                     generate.plot = FALSE)
        good.ix         <- Matrix::rowSums(temp.res$hallmarks) != 0
        all.genes       <- sort(unique(rownames(temp.res$hallmarks[good.ix,])))
        df.no.hallmarks <- temp.res$no.hallmakrs
        #
        message('There is a bug in the Hallmarks\' API that requires the ',
                'function to wait around 5 additional seconds to finish.\n',
                'Sorry. bug report: ',
                'https://github.com/cambridgeltl/chat/issues/6\n')
        Sys.sleep(5.5)
    } else {
        df.no.hallmarks <- NULL
    }

    # build base url for call
    baseUrl <- 'https://chat.lionproject.net/chartdata?measure=%s&hallmarks=%s'
    baseUrl <- sprintf(baseUrl, metric, hierarchy)
    # add genes
    call.url <- sprintf('%s&q=%s', baseUrl, paste(all.genes, collapse = '&q='))

    lines <- NULL
    conn <- NULL
    lines <- tryCatch({
        
        # certificate has been left to expire
        result <- httr::with_config(
            config = httr::config(
                ssl_verifypeer = 0L, 
                ssl_verifyhost = 0L, 
                verbose = 0L
            ),
            {httr::RETRY("GET", url = call.url, times = 3, encode = "json")},
            override = FALSE
        )
        result <- httr::content(result) %>% strsplit('\n')
        result[[1]]
    }, error = function(err) {
        warning('Cannot call Hallmark API, please try again later.')
    })

    if (is.null(lines)) {
        return(list(hallmarks = NULL,
                    no.hallmakrs = NULL,
                    heatmap = NULL))
    }

    item_group <- cumsum(grepl(sprintf("^[A-Za-z0-9\\._,-]+\t%s", metric),
                               lines))
    all.items  <- list()
    col.names  <- c()

    for (ix in split(lines, item_group)){
        item.id <- gsub(sprintf("\t%s", metric),"", ix[1])
        # prepare results
        item.val  <- list()
        my.names  <- c('gene.name')
        my.values <- c(item.id)
        for (line in ix[-1]) {
            if (line == '') {
                next
            }
            my.split  <- strsplit(line, '\t')[[1]]
            my.names  <- c(my.names, my.split[1])
            my.values <- c(my.values, as.numeric(my.split[2]))
            col.names <- c(col.names, my.split[[1]])
        }
        names(my.values) <- my.names
        all.items[[item.id]] <- my.values
    }

    col.names <- unique(col.names)
    df <- data.frame()
    for (ix in all.items) {
        # convert to numeric
        new.ix <- as.numeric(ix[names(ix) != 'gene.name'])
        # set previous names
        names(new.ix) <- names(ix)[names(ix) != 'gene.name']
        # create temporary data frame with controlled column names
        temp.df <- data.frame(t(new.ix[col.names]))
        rownames(temp.df) <- ix['gene.name']
        df <- rbind(df, temp.df)
    }

    df.scaled <- t(scale(t(df)))
    na.ix     <- which(apply(df.scaled, 1, function(col) {
        return(all(is.nan(col)))
    }))
    df.scaled <- df # use counts

    if (is.null(df.no.hallmarks)) {
        df.no.hallmarks <- data.frame(
          gene.name = sort(rownames(df.scaled)[na.ix]),
          stringsAsFactors = FALSE)$gene.name
    }

    df.scaled <- cbind(gene.name = rownames(df.scaled), df.scaled)

    #
    # Generate heatmap
    if (generate.plot) {
        df.scaled$gene.name <- rownames(df.scaled)

        g1 <- reshape2::melt(df.scaled, id.vars = c('gene.name')) %>%
            dplyr::filter(!!(as.name('value')) > 0) %>%
            ggplot2::ggplot(ggplot2::aes_string('gene.name', 'variable',
                                                fill = 'value')) +
              ggplot2::geom_raster() +
              ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                                 hjust = 1)) +
              ggplot2::ggtitle('Hallmarks heatmap',
                  subtitle = stringr::str_wrap(sprintf(
                      'Selected genes without hallmarks (%d): %s',
                      length(df.no.hallmarks),
                      paste(df.no.hallmarks, collapse = ', ')),
                                               width = 50)) +
              ggplot2::xlab('External Gene Name') + ggplot2::ylab('') +
              ggplot2::scale_fill_gradientn(
                  colours = rev(grDevices::topo.colors(2)))

    } else {
        g1 = NULL
    }

    df.scaled$gene.name <- NULL

    return(list(hallmarks = df.scaled, no.hallmakrs = df.no.hallmarks,
                heatmap = g1))
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
#'     'ENSP00000235382',
#'     'ENSP00000233944',
#'     'ENSP00000216911'
#' ))
protein2EnsemblGeneNames <- function(
    ensembl.proteins, use.cache = TRUE, verbose = FALSE
) {
    #
    tryCatch({
        results <- biomart.load(
            attributes = c("ensembl_peptide_id", "ensembl_gene_id"),
            filters = "ensembl_peptide_id",
            values = ensembl.proteins,
            use.cache = use.cache,
            verbose = verbose
        )
        
        #
        return(
            dplyr::arrange(results, !!(as.name('ensembl_peptide_id')))
        )
    }, error = function(msg) {
        warning(sprintf('Error when finding gene names:\n\t%s', msg))
    })
    return(
        data.frame(
            ensembl_peptide_id = ensembl.proteins,
            ensembl_gene_id    = ensembl.proteins,
            external_gene_name = ensembl.proteins,
            stringsAsFactors   = FALSE
        )
    )
}
