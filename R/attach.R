#' @keywords internal
#' @noRd
.onAttach <- function(...) {
  tryCatch(
    {
      suppressPackageStartupMessages(
        library("ellmer")
      )
      packageStartupMessage("Attached 'ellmer'")
    },
    error = function(e) {
      packageStartupMessage("Please install the 'ellmer' package")
    }
  )
}