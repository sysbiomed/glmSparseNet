#' Prepare FPKM data from TCGA project (specific tissue)
#'
#' This function will load data and pre-process it to be used in
#'  survival models.
#'
#' It will:
#'  * load data
#'  * handle duplicate samples for same individal (default is to keep only first)
#'  * remove individuals with missing vital_status or both follow-up/death time span
#'  * remove individuals with follow-up/death time span == 0
#'  * remove genes from RNASeqData (xdata) with standard deviation == 0
#'
#' @param project tcga project that has a package avaliable see https://github.com/averissimo/tcga.data
#' @param tissue.type type of tissue, can be 'primary.solid.tumor', 'metastatic', etc... depending on project.
#'
#' @return a list with data ready to be used in survival analysis, the 'xdata.raw' and 'ydata.raw' elements
#' have the full dataset for the specific tissue and the 'xdata' and 'ydata' have been cleaned by handling
#' patients with multiple samples, removing individuals with event time <= 0, missing and genes that have
#' standard_deviation == 0. It also returns a sha256 checksum for each of the data
#' @export
#'
#' @examples
#' prepare.tcga.survival.data('brca', 'primary.solid.tumor', 'keep_first')
prepare.tcga.survival.data <- function(project = 'brca', tissue.type = 'primary.solid.tumor', handle.duplicates = 'keep_first',
                                       coding.genes = FALSE) {

  package.name <- paste0(project, '.data')

  if (!require(package.name, character.only = T)) {
    stop(sprintf('There is no package called \'%s\' installed, please go to https://github.com/averissimo/tcga.data/releases and install the corresponding release.'))
  }

  data("fpkm.per.tissue", package = package.name)
  futile.logger::flog.info('Loading data from %s package', package.name)
  futile.logger::flog.info('Types of tissue:\n * %s', paste(sprintf('%s (%d)', names(fpkm.per.tissue), sapply(fpkm.per.tissue, ncol)), collapse = '\n * '))

  #
  # Normalize

  xdata.raw <- apply(fpkm.per.tissue[[tissue.type]], 1, function(row) {
    if (max(abs(row)) == 0) {
      return(row)
    }
    return(row / max(abs(row)))
  })

  # remove genes that don't have any variability
  sd.xdata  <- sapply(seq(ncol(xdata.raw)), function(ix) { sd(xdata.raw[,ix]) })
  #
  futile.logger::flog.info('Non-expressed genes to be removed (from %d total genes) : %d', ncol(xdata.raw), sum(sd.xdata == 0))
  futile.logger::flog.info('  Remaining genes : %d', ncol(xdata.raw) - sum(sd.xdata == 0))
  xdata.raw <- xdata.raw[,sd.xdata != 0]

  if (coding.genes) {
    futile.logger::flog.info('Using only coding genes:')
    coding <- loose.rock::coding.genes()
    xdata.raw <- xdata.raw[,colnames(xdata.raw) %in% coding$ensembl_gene_id]
    futile.logger::flog.info('  * total coding genes: %d', length(coding$ensembl_gene_id))
    futile.logger::flog.info('  * coding genes in data: %d (new size of xdata)', ncol(xdata.raw))
  }

  if (handle.duplicates == 'keep_first') {
    xdata <- xdata.raw[!duplicated(strtrim(rownames(xdata.raw), 12)),]
  } else if (handle.duplicates == 'keep_all') {
    xdata <- xdata.raw
  }

  #
  # YDATA

  # load data
  data('clinical', package = package.name)

  # load only patients with valid bcr_patient_barcode (non NA)
  ix.clinical <- !is.na(clinical[[tissue.type]]$bcr_patient_barcode)

  # build ydata data.frame
  ydata <- data.frame(time   = clinical[[tissue.type]]$surv_event_time,
                      status = clinical[[tissue.type]]$vital_status)[ix.clinical,]

  # name each row with patient code
  rownames(ydata) <- clinical[[tissue.type]]$bcr_patient_barcode[ix.clinical]

  # removing patients with:
  #  * negative follow-up
  #  * missing follow-up time
  futile.logger::flog.info('Number of patients removed with: \n * followup time < 0:   %d\n * followup time is.na: %d',
                           sum(!is.na(ydata$time) & ydata$time <= 0), sum(is.na(ydata$time)))
  ydata        <- ydata[!is.na(ydata$time) & ydata$time > 0,]

  # status description:
  #  * == 1 for dead (event happening)
  #  * == 0 for alive (censored)
  ydata$status <- ydata$status != 'Alive'

  # Multiple samples for same individual
  #  rename by appending to name
  xdata <- xdata[strtrim(rownames(xdata), 12) %in% rownames(ydata),]
  if (length(strtrim(rownames(xdata), 12)) != length(unique(strtrim(rownames(xdata), 12)))) {
    flog.warn('There are multiple samples for the same individual.. using strategy: \'%s\'', handle.duplicates)
    #
    new.row.names   <- strtrim(rownames(xdata), 12)
    rownames(xdata) <- sapply(seq_along(new.row.names), function(ix) {
      ix.name <- new.row.names[ix]
      count.b4 <- sum(new.row.names[1:ix] == ix.name)
      return(sprintf('%s.%d', ix.name, count.b4))
    })
    ydata <- ydata[strtrim(rownames(xdata), 12),]
    rownames(ydata) <- rownames(xdata)
  } else {
    rownames(xdata) <- strtrim(rownames(xdata), 12)
  }

  xdata.digest <- loose.rock::digest.cache(xdata)
  xdata.raw.digest <- loose.rock::digest.cache(xdata.raw)

  ydata.digest <- loose.rock::digest.cache(ydata)

  return(list(xdata = xdata,
              xdata.digest = xdata.digest,
              ydata = ydata,
              ydata.digest = ydata.digest,
              xdata.raw = xdata.raw,
              xdata.raw.digest = xdata.raw.digest))

}
