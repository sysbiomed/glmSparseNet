gene.names <- function(ensembl.genes) {
  tryCatch({
    marts <- biomaRt::listMarts()
    index <- grep("ensembl genes",marts$version, ignore.case = TRUE)
    mart <- biomaRt::useMart(marts$biomart[index])
    mart <- run.cache(biomaRt::useMart,marts$biomart[index], 'hsapiens_gene_ensembl',
                      cache.prefix = 'biomart')
    results <- biomaRt::getBM(attributes = c("external_gene_name", "ensembl_gene_id"),
                              filters = "ensembl_gene_id", values = ensembl.genes,
                              mart = mart)
    return(dplyr::arrange(results, external_gene_name))
  }, error = function(msg) {
    flog.warn('Error when finding gene names:\n\t%s', msg)
  })
  return(data.frame(ensembl_gene_id = ensembl.genes, external_gene_name = ensembl.genes, stringsAsFactors = FALSE))
}

hallmarks <- function(genes, measure = 'count', hallmarks = 'full') {
  base.url <- sprintf('http://chat.lionproject.net/chartdata?measure=%s&hallmarks=%s', measure, hallmarks)
  # base.url <- 'http://chat.lionproject.net/?measure=npmi&chart_type=doughnut&hallmarks=full'

  all.genes <- sort(unique(genes))

  call.url <- sprintf('%s&q=%s', base.url, paste(all.genes, collapse = '&q='))

  lines <- loose.rock::run.cache(readr::read_lines, url(call.url), cache.digest = list(digest.cache(call.url)))
  item_group <- cumsum(grepl("^[A-Za-z0-9\\._,-]+\tcount", lines))
  all.items <- list()
  col.names <- c()
  clean.rows <- lapply(split(lines, item_group), function(ix) {
    item.id <- gsub("\tcount","", ix[1])
    # prepare results
    item.val <- list()
    my.names <- c('gene.name')
    my.values <- c(item.id)
    for (line in ix[-1]) {
      if (line == '') {
        next
      }
      my.split <- strsplit(line, '\t')[[1]]
      # flog.info('  %s -- %s',my.split[1], my.split[2] )
      my.names  <- c(my.names, my.split[1])
      my.values <- c(my.values, as.numeric(my.split[2]))
      col.names <<- c(col.names, my.split[[1]])
    }
    names(my.values) <- my.names
    all.items[[item.id]] <- my.values
  })

  col.names <- unique(col.names)
  df <- data.frame()
  for (ix in clean.rows) {
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
  na.ix <- which(apply(df.scaled, 1, function(col) {
    return(all(is.nan(col)))
  }))
  df.scaled <- df # use counts

  df.no.hallmarks <- data.frame(gene.name = sort(rownames(df.scaled)[na.ix]), stringsAsFactors = FALSE)$gene.name
  return(list(hallmarks = df.scaled, no.hallmakrs = df.no.hallmarks))
}
