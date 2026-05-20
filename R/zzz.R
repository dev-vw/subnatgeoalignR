.onLoad <- function(libname, pkgname) {
  defaults <- list(
    # Set defaults
    sng.ONE_TO_ONE_THRESHOLD = 0.85,
    sng.MANY_TO_ONE_THRESHOLD = 0.95,
    sng.CENSUS_CONTRIBUTION_THRESHOLD = 0.05,
    sng.BUFFER_DIST_IN_METERS = 0
  )

  # Don't overwrite anything the user already set in .Rprofile
  toset <- !(names(defaults) %in% names(options()))
  if (any(toset)) options(defaults[toset])
}
