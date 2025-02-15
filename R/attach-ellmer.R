#' @keywords internal
#' @importFrom utils packageDescription
#' @noRd
.onAttach <- function(libname, pkgname) {
  version <- utils::packageDescription(pkgname, fields = "Version")

  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("Package 'ellmer' is required but not installed", call. = FALSE)
  }

  suppressWarnings({
    library("ellmer", character.only = TRUE, warn.conflicts = FALSE)
  })

  packageStartupMessage(
    sprintf(
      "── Attaching packages ──────────────────── %s %s ──",
      pkgname,
      version
    )
  )

  ellmer_version <- utils::packageDescription("ellmer", fields = "Version")
  packageStartupMessage(
    sprintf("✔ ellmer %s", ellmer_version)
  )
}