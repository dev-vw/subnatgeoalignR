.onLoad <- function(libname, pkgname) {

  # Don't overwrite anything the user already set in .Rprofile
  toset <- !(names(.sng_defaults) %in% names(options()))
  if (any(toset)) options(.sng_defaults[toset])
}
