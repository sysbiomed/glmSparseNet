# Function to make sure we have correct platform
getOs <- tryCatch({
  getOsFun <- function() {
    sysinf <- Sys.info()
    if (!is.null(sysinf)) {
      os <- sysinf["sysname"]
      if (os == "Darwin") {
        os <- "osx"
      }
    } else { ## mystery machine
      os <- .Platform$OS.type
      if (grepl("^darwin", R.version$os)) {
        os <- "osx"
      }
      if (grepl("linux-gnu", R.version$os)) {
        os <- "linux"
      }
    }
    tolower(os)
  }
  getOsFun()
})
