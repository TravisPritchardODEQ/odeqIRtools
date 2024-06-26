#' Assess_delist_ws
#'
#' This script will assign category 2 to GNIS's that meet a delisting condtion. If Delist_eligability == 1 and the
#' prev_GNIS_category %in% c('5',  '4A'), final_GNIS_cat will = 2. If  Delist_eligability != 1 and the
#' prev_GNIS_category %in% c('5',  '4A'), the final_GNIS_cat will equal the preivous category.
#'
#'@param df dataframe of IR categories just after joining previous assessments
#'@export






assess_delist <- function(df, type = NULL){

  if (type == 'WS'){

  delist_assessment <- df |>
    mutate(Delist_eligability = case_when( prev_GNIS_category %in% c('5',  '4A', '5C') & IR_category_GNIS_24 == '2' & Delist_eligability ==1 ~ "Delist Eligible",
                                           prev_GNIS_category %in% c('5',  '4A', '5C') & IR_category_GNIS_24 == '2' & (Delist_eligability < 1 | is.na(Delist_eligability)) ~ 'Insuffcient data to delist')) |>
    mutate(final_GNIS_cat = case_when(Delist_eligability == "Delist Eligible" ~ '2',
                                      TRUE ~ final_GNIS_cat),
           Rationale_GNIS = case_when(Delist_eligability %in% c("Delist Eligible",'Insuffcient data to delist')  ~paste0(Delist_eligability, "- ", Rationale_GNIS),
                                      TRUE ~ Rationale_GNIS)) |>
    mutate(final_GNIS_cat = factor(final_GNIS_cat,levels=c('Unassessed', '3D',"3", "3B","3C", "2", "5",'5C', '4A', '4B', '4C'), ordered=TRUE)) |>
    mutate(status_change = case_when(final_GNIS_cat == prev_GNIS_category & IR_category_GNIS_24 == 'Unassessed' ~ "No change in status- No new assessment",
                                     final_GNIS_cat == prev_GNIS_category ~ "No change in status- Assessed",
                                     final_GNIS_cat == '2' & prev_GNIS_category %in% c('5','4A','4B', '4C', '5C') ~ "Delist",
                                     prev_GNIS_category == 'Unassessed' ~ "New Assessment",
                                     prev_GNIS_category == '2' & final_GNIS_cat  %in% c('5','4A','4B', '4C', '5C') ~ "Attain to Impaired",
                                     prev_GNIS_category %in% c('3D','3','3B', '3C') & final_GNIS_cat %in% c('5','4A','4B', '4C', '5C') ~ "Insufficient to Impaired",
                                     prev_GNIS_category %in% c('3D','3','3B', '3C') & final_GNIS_cat %in% c('2') ~ "Insufficient to Attain",
                                     prev_GNIS_category %in% c('3D') & final_GNIS_cat %in% c('3') ~ "3D to Insufficient",
                                     prev_GNIS_category %in% c('4A') & final_GNIS_cat %in% c('5', '5C') ~ "4A to Category 5",
                                     TRUE ~ paste0(prev_GNIS_category, ' to ', final_GNIS_cat )
    )) |>
    dplyr::relocate(final_GNIS_cat, .after = period) |>
    dplyr::relocate(Rationale_GNIS, .after = final_GNIS_cat)
  } else {
    delist_assessment <- df |>
      mutate(Delist_eligability = case_when( prev_category %in% c('5',  '4A', '5C') & IR_category == '2' & Delist_eligability ==1 ~ "Delist Eligible",
                                             prev_category %in% c('5',  '4A', '5C') & IR_category == '2' & (Delist_eligability < 1 | is.na(Delist_eligability)) ~ 'Insuffcient data to delist')) |>
      mutate(final_AU_cat = case_when(Delist_eligability == "Delist Eligible" ~ '2',
                                      TRUE ~ final_AU_cat),
             Rationale = case_when(Delist_eligability %in% c("Delist Eligible",'Insuffcient data to delist')  ~paste0(Delist_eligability, "- ", Rationale),
                                   TRUE ~ Rationale)) |>
      mutate(final_AU_cat = factor(final_AU_cat,levels=c('Unassessed', '3D',"3", "3B","3C", "2", "5",'5C', '4A', '4B', '4C'), ordered=TRUE)) |>
      mutate(status_change = case_when(final_AU_cat == prev_category & IR_category == 'Unassessed' ~ "No change in status- No new assessment",
                                       final_AU_cat == prev_category ~ "No change in status- Assessed",
                                       final_AU_cat == '2' & prev_category %in% c('5','4A','4B', '4C', '5C') ~ "Delist",
                                       prev_category == 'Unassessed' ~ "New Assessment",
                                       prev_category == '2' & final_AU_cat  %in% c('5','4A','4B', '4C', '5C') ~ "Attain to Impaired",
                                       prev_category %in% c('3D','3','3B', '3C') & final_AU_cat %in% c('5','4A','4B', '4C', '5C') ~ "Insufficient to Impaired",
                                       prev_category %in% c('3D','3','3B', '3C') & final_AU_cat %in% c('2') ~ "Insufficient to Attain",
                                       prev_category %in% c('3D') & final_AU_cat %in% c('3') ~ "3D to Insufficient",
                                       prev_category %in% c('4A') & final_AU_cat %in% c('5', '5C') ~ "4A to Category 5",
                                       TRUE ~ paste0(prev_category, 'to ', final_AU_cat)
      )) |>
      dplyr::relocate(final_AU_cat, .after = period) |>
      dplyr::relocate(Rationale, .after = final_AU_cat)

  }

  return(delist_assessment)

}
