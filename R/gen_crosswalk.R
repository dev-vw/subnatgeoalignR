#' Generate crosswalk table
#'
#' @description This is a wrapper function that takes two sets of polygons
#' and generates a table of one-to-one matches, many-to-1, and partial matches.
#'
#' @param pepfar a PEPFAR sf object for a country at a given ADM level
#' @param census a Census sf object for a country at a given ADM level
#'
#' @returns a df with a final crosswalk, with pepfar polygons as the
#' basis of comparison
#'
#' @export
#'
#' @examples
gen_crosswalk <- function(pepfar, census) {
  data_lst <- process_data(pepfar, census)
  contained_pairs_lst <- process_contained_pairs(data_lst$pepfar, data_lst$census)

  one_to_one <- process_1to1(contained_pairs_lst$pepfar, contained_pairs_lst$census)
  many_to_one <- process_manyto1(one_to_one,
                                 contained_pairs_lst$contained_census_ids,
                                 contained_pairs_lst$pepfar,
                                 contained_pairs_lst$census)

  contained_summary <- merge_contained_pairs(one_to_one,
                                             many_to_one,
                                             contained_pairs_lst$contained_df)

  final_crosswalk <- process_crosswalk(one_to_one,
                                       many_to_one,
                                       contained_summary,
                                       data_lst$pepfar_names,
                                       data_lst$census_names)

  return(final_crosswalk)
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

#' Find contained pairs between two sets of sf polygons
#'
#' @description Looks for the census polygons that are contained by pepfar polygons
#' and returns a df, `contained_df`, where each row is a census poly contained by
#' a pepfar poly. If a census poly takes up more than 50% of the area of a pepfar
#' poly, it is filtered out.
#'
#' @param pepfar
#' @param census
#'
#' @import dplyr
#' @import sf
#'
#' @returns a list of three dfs and one vectors
#'
#' @export
#'
#' @examples
process_contained_pairs <- function(pepfar, census) {

  # a list: look for the census polygons covered by pepfar
  contained_pairs <- sf::st_covered_by(census, pepfar, sparse = TRUE)

  # a df: create a df of contained pairs, where each row is a census polygon
  # contained by a pepfar polygon
  contained_df <-
    tibble(
      # populates columns where each row shows a census polygons contained by a
      # pepfar polygon; note that this accounts for pepfar polygons that contain
      # one or more census polys
      census_id = census$census_id[rep(seq_along(contained_pairs),
                                       lengths(contained_pairs))],
      pepfar_id = pepfar$pepfar_id[unlist(contained_pairs)]
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(census_area = as.numeric(st_area(census[census$census_id == census_id, ])),
                  pepfar_area = as.numeric(st_area(pepfar[pepfar$pepfar_id == pepfar_id, ])),
                  pct_of_pepfar = census_area / pepfar_area) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(is_contained = TRUE) %>%
    dplyr::filter(pct_of_pepfar < 0.5)

  contained_census_ids <- unique(contained_df$census_id)

  census <- census %>% dplyr::filter(!census_id %in% contained_census_ids)

  return(list(pepfar = pepfar,
              census = census,
              contained_df = contained_df,
              contained_census_ids = contained_census_ids))
}


#' Find one-to-one matches between two sets of sf polygons
#'
#' @description For all polygons in the pepfar polygon set, finds all one-to-one
#' matches by computing a Jaccard index (INTERNAL). Rows with a Jaccard index above
#' the `get_option("one_to_one")` threshold are kept and returned.
#'
#' @param pepfar
#' @param census
#'
#' @returns a df
#'
#' @examples
process_1to1 <- function(pepfar, census) {
  intersecting_pairs <- sf::st_intersects(pepfar, census)

  jaccard_results <- list()

  for (i in seq_len(nrow(pepfar))) {
    candidates <- intersecting_pairs[[i]]

    if (length(candidates) == 0) next

    for (j in candidates) {
      score <- compute_jacard(pepfar[i, ], census[j, ])

      jaccard_results[[length(jaccard_results) + 1]] <- tibble(
        pepfar_id = pepfar$pepfar_id[i],
        census_id = census$census_id[j],
        jaccard = score
      )
    }
  }

  jaccard_df <- dplyr::bind_rows(jaccard_results)

  one_to_one <- jaccard_df %>%
    dplyr::filter(jaccard >= get_option("one_to_one")) %>%
    dplyr::group_by(pepfar_id) %>% dplyr::slice_max(jaccard, n = 1) %>% dplyr::ungroup() %>%
    dplyr::group_by(census_id) %>% dplyr::slice_max(jaccard, n = 1) %>% dplyr::ungroup() %>%
    dplyr::mutate(crosswalk_id = paste0("cw_", row_number()),
                  match_type = "1-to-1")

  return(one_to_one)
}

#' Calculates Jaccard index
#'
#' @description The Jaccard index betwen two polygons is the proportion of the area
#' of intersection over the area of union.
#'
#' @param poly1
#' @param poly2
#'
#' @returns a numeric
#'
#' @examples
compute_jacard <- function(poly1, poly2) {
  inter <- sf::st_intersection(poly1, poly2)

  if (nrow(inter) == 0 || sf::st_is_empty(inter)) return(0)

  area_inter <- as.numeric(sf::st_area(inter))
  area_union <- as.numeric(sf::st_area(sf::st_union(poly1, poly2)))

  return(area_inter / area_union)
}

#' Find many-to-one matches between two sets of sf polygons
#'
#' @description For all polygons in the pepfar polygon set, finds the census polygons
#' that comprise them. Uses the thresholds, `get_option("many_to_one")` and
#' `get_option("census_contribution")`, to determine which polygons should be
#' included. (INTERNAL)
#'
#' @param one_to_one
#' @param contained_census_ids
#' @param pepfar
#' @param census
#'
#' @returns a df
#'
#' @examples
process_manyto1 <- function(one_to_one, contained_census_ids, pepfar, census) {
  matched_pepfar_ids <- unique(one_to_one$pepfar_id)
  matched_census_ids <- unique(c(one_to_one$census_id, contained_census_ids))

  unmatched_pepfar <- pepfar %>% dplyr::filter(!pepfar_id %in% matched_pepfar_ids)
  unmatched_census <- census %>% dplyr::filter(!census_id %in% matched_census_ids)

  intersecting_unmatched <- sf::st_intersects(unmatched_pepfar, unmatched_census)

  coverage_results <- list()

  for (i in seq_len(nrow(unmatched_pepfar))) {
    candidates <- intersecting_unmatched[[i]]

    if (length(candidates) == 0) next

    pepfar_area <- as.numeric(sf::st_area(unmatched_pepfar[i, ]))

    for (j in candidates) {
      inter <- sf::st_intersection(unmatched_pepfar[i, ], unmatched_census[j, ])

      area_inter <- as.numeric(sf::st_area(inter))
      area_census <- as.numeric(sf::st_area(unmatched_census[j, ]))

      coverage_results[[length(coverage_results) + 1]] <- tibble(
        pepfar_id = unmatched_pepfar$pepfar_id[i],
        census_id = unmatched_census$census_id[j],
        area_intersect = area_inter,
        pct_of_pepfar = area_inter / pepfar_area,
        pct_of_census = area_inter / area_census
      )
    }
  }

  coverage_df <- dplyr::bind_rows(coverage_results)

  coverage_df <- coverage_df %>%
    dplyr::filter(pct_of_census >= get_option("census_contribution"))

  many_to_one <- coverage_df %>%
    dplyr::group_by(pepfar_id) %>%
    dplyr::arrange(desc(pct_of_pepfar), .by_group = TRUE) %>%
    dplyr::mutate(cumulative_coverage = cumsum(pct_of_pepfar)) %>%
    # keep census polys needed to reach threshold; filters out those that contribute
    # less than the many_to_one threshold, cumulatively
    dplyr::filter(lag(cumulative_coverage, default = 0) < get_option("many_to_one")) %>%
    dplyr::summarise(
      census_ids_contributing = list(as.integer(census_id)),
      pcts_of_pepfar_contributing = list(as.numeric(pct_of_pepfar)),
      total_coverage = sum(pct_of_pepfar),
      n_census_polys = n(),
      match_type = dplyr::case_when(
        n() == 1 ~ "partial",
        n() >- 1 ~ "many-to-1"
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(crosswalk_id = paste0("cw_",
                                        max(as.integer(stringr::str_extract(one_to_one$crosswalk_id, "\\d+"))) + row_number()))

  return(many_to_one)
}

#' Assigns an existing crosswalk ID to polygons in `contained_df`.
#'
#' @description (INTERNAL)
#'
#' @param one_to_one
#' @param many_to_one
#' @param contained_df
#'
#' @returns a df
#'
#' @examples
merge_contained_pairs <- function(one_to_one, many_to_one, contained_df) {
  crosswalk_lookup <- dplyr::bind_rows(
    one_to_one %>% dplyr::select(pepfar_id, crosswalk_id),
    many_to_one %>% dplyr::select(pepfar_id, crosswalk_id)
  ) %>% dplyr::arrange(pepfar_id)

  contained_summary <- contained_df %>%
    dplyr::group_by(pepfar_id) %>%
    dplyr::summarise(census_ids_contained = list(census_id),
                     pcts_of_pepfar_contained = list(pct_of_pepfar),
                     .groups = "drop") %>%
    dplyr::left_join(crosswalk_lookup, by = "pepfar_id")

  return(contained_summary)
}

#' Creates the final crosswalk table.
#'
#' @description `one_to_one` is first flattened to match the dimensions of
#' `many_to_one`. `one_to_one` and `many_to_one` are then combined (row-binded)
#' and several new columns are created to record pertinent crosswalk information.
#'
#'
#' @param one_to_one
#' @param many_to_one
#' @param contained_summary
#' @param pepfar_names
#' @param census_names
#'
#' @returns a df
#'
#' @examples
process_crosswalk <- function(one_to_one, many_to_one,
                              contained_summary,
                              pepfar_names, census_names) {

  one_to_one_flat <- one_to_one %>%
    dplyr::mutate(
      census_ids_contributing = purrr::map(census_id, ~as.integer(list(.x))),
      pcts_of_pepfar_contributing = purrr::map(jaccard, ~.x),
      total_coverage = jaccard,
      n_census_polys = 1) %>%
    dplyr::select(pepfar_id,
                  census_ids_contributing, total_coverage,
                  pcts_of_pepfar_contributing,
                  n_census_polys,
                  match_type,
                  crosswalk_id)

  final_crosswalk <- dplyr::bind_rows(one_to_one_flat, many_to_one) %>%
    dplyr::left_join(contained_summary %>%
                       dplyr::select(pepfar_id,
                                     census_ids_contained,
                                     pcts_of_pepfar_contained), by = "pepfar_id") %>%
    dplyr::mutate(
      census_ids_contributing = purrr::map2(
        census_ids_contributing,
        census_ids_contained,
        function(x, y) {
          if (all(is.na(y))) {
            x
          } else {
            c(x, y)
          }
        }
      ),
      pcts_of_pepfar = purrr::map2(
        pcts_of_pepfar_contributing,
        pcts_of_pepfar_contained,
        function(x, y) {
          if (all(is.na(y))) {
            x
          } else {
            c(x, y)
          }
        }
      ),
      n_census_polys = purrr::map_int(census_ids_contributing, length),
      match_type = dplyr::case_when(
        n_census_polys == 1 & match_type != "partial" ~ "one-to-one",
        n_census_polys > 1 ~ "many-to-one",
        .default = "partial"
      )
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(total_coverage = sum(unlist(pcts_of_pepfar))) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(census_ids_contained, pcts_of_pepfar_contained, pcts_of_pepfar_contributing)) %>%
    dplyr::arrange(pepfar_id)

  final_crosswalk <- final_crosswalk %>%
    tidyr::unnest(cols = c(census_ids_contributing, pcts_of_pepfar))

  # adds the pepfar and census area names to the table
  final_crosswalk <- dplyr::left_join(final_crosswalk, st_drop_geometry(pepfar_names), by = "pepfar_id") %>%
    dplyr::left_join(st_drop_geometry(census_names), by = c("census_ids_contributing" = "census_id")) %>%
    dplyr::rename(pepfar_name = name,
                  census_name = AREA_NAME) %>%
    dplyr::arrange(pepfar_id)

  return(final_crosswalk)
}


