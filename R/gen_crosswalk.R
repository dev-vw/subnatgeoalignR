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

# helper functions --------------------------------------------------------
process_data <- function(pepfar, census) {
  # ensure same CRS
  census <- st_transform(census, st_crs(pepfar))

  # add row IDs if not already present
  pepfar <- pepfar %>% mutate(pepfar_id = row_number())
  census <- census %>% mutate(census_id = row_number())

  # data frames needed for merging names at the final final_crosswalk formation
  pepfar_names <- pepfar %>% select(name, pepfar_id)
  census_names <- census %>% select(AREA_NAME, census_id)

  # apply buffer if needed
  if (BUFFER_DIST_IN_METERS == 0) {
    pepfar <- st_make_valid(pepfar)
  } else {
    pepfar <- st_buffer(st_make_valid(pepfar), dist = BUFFER_DIST_IN_METERS)
  }

  return(list(pepfar = pepfar, census = census,
              pepfar_names = pepfar_names, census_names = census_names))
}

process_contained_pairs <- function(pepfar, census) {
  contained_pairs <- st_covered_by(census, pepfar, sparse = TRUE)

  contained_df <-
    tibble(
      census_id = census$census_id[rep(seq_along(contained_pairs),
                                       lengths(contained_pairs))],
      pepfar_id = pepfar$pepfar_id[unlist(contained_pairs)]
    ) %>%
    rowwise() %>%
    mutate(census_area = as.numeric(st_area(census[census$census_id == census_id, ])),
           pepfar_area = as.numeric(st_area(pepfar[pepfar$pepfar_id == pepfar_id, ])),
           pct_of_pepfar = census_area / pepfar_area) %>%
    ungroup() %>%
    mutate(is_contained = TRUE) %>%
    filter(pct_of_pepfar < 0.5)

  contained_census_ids <- unique(contained_df$census_id)

  census <- census %>% filter(!census_id %in% contained_census_ids)

  return(list(pepfar = pepfar,
              census = census,
              contained_df = contained_df,
              contained_census_ids = contained_census_ids))
}

process_1to1 <- function(pepfar, census) {
  intersecting_pairs <- st_intersects(pepfar, census)

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

  jaccard_df <- bind_rows(jaccard_results)

  one_to_one <- jaccard_df %>%
    filter(jaccard >= ONE_TO_ONE_THRESHOLD) %>%
    group_by(pepfar_id) %>% slice_max(jaccard, n = 1) %>% ungroup() %>%
    group_by(census_id) %>% slice_max(jaccard, n = 1) %>% ungroup() %>%
    mutate(crosswalk_id = paste0("cw_", row_number()),
           match_type = "1-to-1")

  return(one_to_one)
}

compute_jacard <- function(poly1, poly2) {
  inter <- st_intersection(poly1, poly2)

  if (nrow(inter) == 0 || st_is_empty(inter)) return(0)

  area_inter <- as.numeric(st_area(inter))
  area_union <- as.numeric(st_area(st_union(poly1, poly2)))

  return(area_inter / area_union)
}

process_manyto1 <- function(one_to_one, contained_census_ids, pepfar, census) {
  matched_pepfar_ids <- unique(one_to_one$pepfar_id)
  matched_census_ids <- unique(c(one_to_one$census_id, contained_census_ids))

  unmatched_pepfar <- pepfar %>% filter(!pepfar_id %in% matched_pepfar_ids)
  unmatched_census <- census %>% filter(!census_id %in% matched_census_ids)

  intersecting_unmatched <- st_intersects(unmatched_pepfar, unmatched_census)

  coverage_results <- list()

  for (i in seq_len(nrow(unmatched_pepfar))) {
    candidates <- intersecting_unmatched[[i]]

    if (length(candidates) == 0) next

    pepfar_area <- as.numeric(st_area(unmatched_pepfar[i, ]))

    for (j in candidates) {
      inter <- st_intersection(unmatched_pepfar[i, ], unmatched_census[j, ])

      area_inter <- as.numeric(st_area(inter))
      area_census <- as.numeric(st_area(unmatched_census[j, ]))

      coverage_results[[length(coverage_results) + 1]] <- tibble(
        pepfar_id = unmatched_pepfar$pepfar_id[i],
        census_id = unmatched_census$census_id[j],
        area_intersect = area_inter,
        pct_of_pepfar = area_inter / pepfar_area,
        pct_of_census = area_inter / area_census
      )
    }
  }

  coverage_df <- bind_rows(coverage_results)

  coverage_df <- coverage_df %>%
    filter(pct_of_census >= CENSUS_CONTRIBUTION_THRESHOLD)

  many_to_one <- coverage_df %>%
    group_by(pepfar_id) %>%
    arrange(desc(pct_of_pepfar), .by_group = TRUE) %>%
    mutate(cumulative_coverage = cumsum(pct_of_pepfar)) %>%
    # Keep census polys needed to reach threshold; removes those that contribute
    # very little
    filter(lag(cumulative_coverage, default = 0) < MANY_TO_ONE_THRESHOLD) %>%
    summarise(
      census_ids_contributing = list(as.integer(census_id)),
      pcts_of_pepfar_contributing = list(as.numeric(pct_of_pepfar)),
      total_coverage = sum(pct_of_pepfar),
      n_census_polys = n(),
      match_type = case_when(
        n() == 1 ~ "partial",
        n() >- 1 ~ "many-to-1"
      )
    ) %>%
    ungroup() %>%
    mutate(crosswalk_id = paste0("cw_",
                                 max(as.integer(str_extract(one_to_one$crosswalk_id, "\\d+"))) + row_number()))

  return(many_to_one)
}

# Step 4 ------------------------------------------------------------------
merge_contained_pairs <- function(one_to_one, many_to_one, contained_df) {
  crosswalk_lookup <- bind_rows(
    one_to_one %>% select(pepfar_id, crosswalk_id),
    many_to_one %>% select(pepfar_id, crosswalk_id)
  ) %>% arrange(pepfar_id)

  contained_summary <- contained_df %>%
    group_by(pepfar_id) %>%
    summarise(census_ids_contained = list(census_id),
              pcts_of_pepfar_contained = list(pct_of_pepfar),
              .groups = "drop") %>%
    left_join(crosswalk_lookup, by = "pepfar_id")

  return(contained_summary)
}

process_crosswalk <- function(one_to_one, many_to_one, contained_summary, pepfar_names, census_names) {
  one_to_one_flat <- one_to_one %>%
    mutate(
      census_ids_contributing = map(census_id, ~as.integer(list(.x))),
      pcts_of_pepfar_contributing = map(jaccard, ~.x),
      total_coverage = jaccard,
      n_census_polys = 1) %>%
    select(pepfar_id, census_ids_contributing, total_coverage, pcts_of_pepfar_contributing, n_census_polys, match_type, crosswalk_id)

  final_crosswalk <- bind_rows(one_to_one_flat, many_to_one) %>%
    left_join(contained_summary %>% select(pepfar_id, census_ids_contained, pcts_of_pepfar_contained),
              by = "pepfar_id") %>%
    mutate(
      census_ids_contributing = map2(
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
      pcts_of_pepfar = map2(
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
      n_census_polys = map_int(census_ids_contributing, length),
      match_type = case_when(
        n_census_polys == 1 & match_type != "partial" ~ "one-to-one",
        n_census_polys > 1 ~ "many-to-one",
        .default = "partial"
      )
    ) %>% rowwise() %>%
    mutate(total_coverage = sum(unlist(pcts_of_pepfar))) %>% ungroup() %>%
    select(-c(census_ids_contained, pcts_of_pepfar_contained, pcts_of_pepfar_contributing)) %>% arrange(pepfar_id)

  final_crosswalk <- final_crosswalk %>% unnest(cols = c(census_ids_contributing, pcts_of_pepfar))

  final_crosswalk <- left_join(final_crosswalk, st_drop_geometry(pepfar_names), by = "pepfar_id") %>%
    left_join(st_drop_geometry(census_names), by = c("census_ids_contributing" = "census_id")) %>%
    rename(pepfar_name = name,
           census_name = AREA_NAME) %>% arrange(pepfar_id)

  return(final_crosswalk)
}


