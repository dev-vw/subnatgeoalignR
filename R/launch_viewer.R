#' Launch the interactive geography alignment viewer
#'
#' Opens a Shiny app that displays the intersection layer on a dark basemap,
#' alongside PEPFAR and census outlines. Users can filter by PEPFAR unit,
#' toggle layers, and click polygons for attribute popups.
#'
#' @param intersection_layer An \code{sf} object produced by
#'   \code{\link{build_intersection_layer}}, with \code{intersection_geom} as
#'   the active geometry column.
#' @param pepfar_sf The original PEPFAR \code{sf} object (used for outline
#'   rendering).
#' @param census_sf The original census \code{sf} object (used for outline
#'   rendering).
#' @param pepfar_id_col Character. Name of the PEPFAR identifier column in
#'   \code{intersection_layer} and \code{pepfar_sf}.
#' @param census_id_col Character. Name of the census identifier column
#'   \emph{before} the \code{census_} prefix was added — i.e. the original
#'   column name in \code{census_sf}. The app will look for
#'   \code{paste0("census_", census_id_col)} in \code{intersection_layer}.
#' @param ... Additional arguments passed to \code{\link[shiny]{shinyApp}}.
#'
#' @return Invisibly returns the \code{shiny.appobj} (useful for embedding in
#'   RMarkdown / Quarto).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- build_intersection_layer(
#'   pepfar_sf  = pepfar_adm2,
#'   census_sf  = census_adm2,
#'   pepfar_id  = uid,
#'   census_id  = geo_code
#' )
#'
#' launch_viewer(
#'   intersection_layer = result,
#'   pepfar_sf          = pepfar_adm2,
#'   census_sf          = census_adm2,
#'   pepfar_id_col      = "uid",
#'   census_id_col      = "geo_code"
#' )
#' }
launch_viewer <- function(
    intersection_layer,
    pepfar_sf,
    census_sf,
    pepfar_id_col,
    census_id_col,
    ...
) {

  # Validate inputs
  if (!inherits(intersection_layer, "sf")) {
    rlang::abort("`intersection_layer` must be an sf object.")
  }
  if (!pepfar_id_col %in% names(intersection_layer)) {
    rlang::abort(
      paste0("Column '", pepfar_id_col, "' not found in `intersection_layer`.")
    )
  }

  census_col_full <- census_id_col
  if (!census_col_full %in% names(intersection_layer)) {
    rlang::abort(
      paste0(
        "Column '", census_col_full, "' not found in `intersection_layer`. ",
        "Did you mean a different `census_id_col`?"
      )
    )
  }

  app <- shiny::shinyApp(
    ui     = app_ui(),
    server = function(input, output, session) {
      app_server(
        input, output, session,
        intersection_layer = intersection_layer,
        pepfar_sf          = pepfar_sf,
        census_sf          = census_sf,
        pepfar_id_col      = pepfar_id_col,
        census_id_col      = census_id_col
      )
    },
    ...
  )

  shiny::runApp(app)
  invisible(app)
}
