#' Package-included defaults
.sng_defaults <- list(
  sng.ONE_TO_ONE_THRESHOLD  = 0.85,
  sng.MANY_TO_ONE_THRESHOLD = 0.95,
  sng.CENSUS_CONTRIBUTION   = 0.05,
  sng.BUFFER_DIST           = 0
)

#' Get current subnatgeoalignR options
#'
#' @export
sng_options <- function() {
  list(
    one_to_one = getOption("sng.ONE_TO_ONE_THRESHOLD"),
    many_to_one = getOption("sng.MANY_TO_ONE_THRESHOLD"),
    census_contribution = getOption("sng.CENSUS_CONTRIBUTION"),
    buffer_dist = getOption("sng.BUFFER_DIST")
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
        one_to_one > 1 ||
        one_to_one < 0) {
      stop("`one_to_one` must be numeric between 0 and 1")
    }
    options(sng.ONE_TO_ONE_THRESHOLD = one_to_one)
  }

  if (!is.null(many_to_one)) {
    if (!is.numeric(many_to_one) ||
        many_to_one > 1 ||
        many_to_one < 0) {
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
        buffer_dist < 0) {
      stop("`buffer_dist` must be a non-negative number")
    }
    options(sng.BUFFER_DIST_IN_METERS = buffer_dist)
  }

  # return nothing in console
  invisible(NULL)
}

#' Reset all sng options to their defaults
#' @export
sng_reset <- function() {
  options(.sng_defaults)
  invisible(NULL)
}

#' Initial sf data processing for crosswalk generation
#'
#' @description (INTERNAL) Processes two sets of mutually comparable sf objects
#' for crosswalk comparison. This is the first step of the crosswalking heuristic.
#'
#' @param pepfar
#' @param census
#'
#' @import dplyr
#' @import sf
#'
#' @returns a list of dfs
#' @export
#'
#' @examples
process_data <- function(pepfar, census) {
  # ensure same CRS of two sets of shapefiles
  census <- sf::st_transform(census, st_crs(pepfar))

  # add row IDs if not already present
  pepfar <- pepfar %>% dplyr::mutate(pepfar_id = row_number())
  census <- census %>% dplyr::mutate(census_id = row_number())

  # polygon names needed for merging names at the final final_crosswalk formation
  pepfar_names <- pepfar %>% dplyr::select(name, pepfar_id)
  census_names <- census %>% dplyr::select(AREA_NAME, census_id)

  # apply buffer if needed
  if (get_option("buffer_dist") == 0) {
    pepfar <- sf::st_make_valid(pepfar)
  } else {
    pepfar <- sf::st_buffer(sf::st_make_valid(pepfar),
                            dist = get_option("buffer_dist"))
  }

  return(list(pepfar = pepfar, census = census,
              pepfar_names = pepfar_names, census_names = census_names))
}


