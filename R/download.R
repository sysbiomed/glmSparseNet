#' Download files to local temporary path
#'
#' In case of new call it uses the temporary cache instead of
#' downloading again.
#'
#' Inspired by STRINGdb Bioconductor package, but using curl
#' as file may be too big to handle.
#'
#' @param urlStr url of file to download
#' @param oD temporary directory to store file
#'
#' @return path to file
#'
#' @examples
#' glmSparseNet:::downloadFileLocal(
#'   "https://string-db.org/api/tsv-no-header/version"
#' )
downloadFileLocal <- function(urlStr, oD = tempdir()) {
  fileName <- utils::tail(strsplit(urlStr, "/")[[1]], 1)
  temp <- file.path(oD, fileName)
  if (!file.exists(temp) || file.info(temp)$size == 0) {
    message("STRINGdb download: This might take some time, please be patient.")
    method <- if (capabilities("libcurl")) {
      "curl"
    } else {
      "auto"
    }
    oldTimeout <- getOption("timeout")
    options(timeout = oldTimeout * 4) # add a bit as it can take a while

    utils::download.file(urlStr,
      destfile = temp, method = method,
      quiet = FALSE, timeout = oldTimeout * 4
    )

    options(timeout = oldTimeout)
  }
  if (file.info(temp)$size == 0) {
    unlink(temp)
    temp <- NULL
    futile.logger::flog.info(
      paste(
        "ERROR: failed to download ",
        fileName,
        ".\n",
        "Please check your internet connection and/or try again.\n",
        "Then, if you still display this error message please contact us.",
        sep = ""
      )
    )
  }
  return(temp)
}
