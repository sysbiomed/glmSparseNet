#' Prepare FPKM data from TCGA project (specific tissue)
#'
#' This function will load data and pre-process it to be used in
#'  survival models.
#'
#' It will:
#'  * load data
#'  *
#'
#' @param project
#' @param tissue.type
#'
#' @return
#' @export
#'
#' @examples
#' prepare.tcga.survival.data('brca', 'primary.solid.tumor', 'keep_first')
prepare.tcga.survival.data <- function(project = 'brca', tissue.type = 'primary.solid.tumor', handle.duplicates = 'keep_first') {

  package.name <- paste0(project, '.data')

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
  flog.info('Non-expressed genes to be removed (from %d total genes) : %d', ncol(xdata.raw), sum(sd.xdata == 0))
  flog.info('  Remaining genes : %d', ncol(xdata.raw) - sum(sd.xdata == 0))
  xdata.raw <- xdata.raw[,sd.xdata != 0]

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
  flog.info('Number of patients removed with: \n * followup time < 0:   %d\n * followup time is.na: %d',
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

  xdata.digest <- verissimo::digest.cache(xdata)
  xdata.raw.digest <- verissimo::digest.cache(xdata.raw)

  ydata.digest <- verissimo::digest.cache(ydata)

  return(list(xdata = xdata,
              xdata.digest = xdata.digest,
              ydata = ydata,
              ydata.digest = ydata.digest,
              xdata.raw = xdata.raw,
              xdata.raw.digest = xdata.raw.digest))

}
