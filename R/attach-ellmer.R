#' @keywords internal
#' @importFrom utils packageDescription
#' @noRd
.onAttach <- function(libname, pkgname) {
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("Package 'ellmer' is required but not installed", call. = FALSE)
  }
  
  pkg_version <- utils::packageDescription(pkgname, fields = "Version")
  ellmer_version <- utils::packageDescription("ellmer", fields = "Version")
  
  packageStartupMessage(
    sprintf(
      "Loaded %s v%s with ellmer v%s", 
      pkgname,
      pkg_version,
      ellmer_version
    )
  )
}