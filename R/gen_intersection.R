# processed_lst <- process_data(pepfar, census)
# intersection_matrix <- sf::st_intersects(processed_lst$pepfar, processed_lst$census)
#
# get_intersection_geom <- function(x, y) {
#   if (is.na(y)) return(NA)
#
#   return(
#     st_intersection(
#     processed_lst$pepfar[x, ] %>% st_geometry(),
#     processed_lst$census[y, ] %>% st_geometry())
#   )
# }
#
# result <- processed_lst$pepfar %>%
#   left_join(
#     tibble(
#       pepfar_id = rep(
#         seq_along(intersection_matrix),
#         lengths(intersection_matrix)
#       ),
#       census_id = unlist(intersection_matrix)
#     ) %>%
#       st_drop_geometry(),
#     by = "pepfar_id"
#   ) %>%
#   # Join census attributes by row index
#   left_join(
#     processed_lst$census %>%
#       st_drop_geometry(),
#     by = "census_id"
#   ) %>%
#   mutate(
#     intersection_geom = purrr::map2(pepfar_id, census_id, get_intersection_geom)
#   ) %>%
#   mutate(
#     intersection_geom = purrr::map(intersection_geom,
#                                    ~ { if (length(.x) == 0) NA else .x })
#   ) %>%
#   st_as_sf() %>%
#   st_drop_geometry()
