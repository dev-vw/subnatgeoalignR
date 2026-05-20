#' Get current subnatgeoalignR options
#'
#' @export
sng_options <- function() {
  list(
    one_to_one = getOption("sng.ONE_TO_ONE_THRESHOLD"),
    many_to_one = getOption("sng.MANY_TO_ONE_THRESHOLD"),
    census_contibution = getOption("sng.CENSUS_CONTRIBUTION_THRESHOLD"),
    buffer_dist = getOption("sng.BUFFER_DIST_IN_METERS")
  )
}

#' Set sugnatgeoalignR options
#'
#' @param one_to_one
#' @param many_to_one
#' @param census_contibution
#' @param buffer_dist
#'
#' @export
sng_set <- function(one_to_one = NULL,
                    many_to_one = NULL,
                    census_contibution = NULL,
                    buffer_dist = NULL) {

  if (!is.null(one_to_one)) {
    if (!is.numeric(one_to_one) ||
        one_to_one <= 1 ||
        one_to_one >= 0) {
      stop("`one_to_one` must be numeric between 0 and 1")
    }
    options(sng.ONE_TO_ONE_THRESHOLD = one_to_one)
  }

  if (!is.null(many_to_one)) {
    if (!is.numeric(many_to_one) ||
        many_to_one <= 1 ||
        many_to_one >= 0) {
      stop("`many_to_one` must be numeric between 0 and 1")
    }
    options(sng.MANY_TO_ONE_THRESHOLD = many_to_one)
  }

  if (!is.null(census_contibution)) {
    if (!is.numeric(census_contibution) ||
        census_contibution <= 1 ||
        census_contibution >= 0) {
      stop("`census_contibution` must be numeric between 0 and 1")
    }
    options(sng.CENSUS_CONTRIBUTION_THRESHOLD = census_contibution)
  }

  if (!is.null(buffer_dist)) {
    if (!is.numeric(buffer_dist) ||
        buffer_dist >= 0) {
      stop("`buffer_dist` must be a non-negative number")
    }
    options(sng.BUFFER_DIST_IN_METERS = buffer_dist)
  }

  # return nothing in console
  invisible(NULL)
}


