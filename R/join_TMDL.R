#' join_TMDL
#'
#' Join assessments to TMDL actions. used to determine categery 4A and to display action IDs in display
#'
#' @param df dataframe to join. Shoud;d either be final AU decsion table or final GNIS decision table, after
#'            all delist etc completed
#' @param type should be 'AU' or 'GNIS' depending on what you ware matching
#' @returns Returns same datafrane as df, but with action ID and TMDL parameters added
#' @import odeqtmdl
#' @export
#'
#'
#'
join_TMDL <- function(df, type){


  tmdl_actual_periods <- tibble::tribble(
                                              ~Period, ~Actual_period,
                                              "year_round", 'year_round',
                                              'spawn', 'spawn',
                                               "Both",   "year_round",
                                               "Both",     "spawn",
                           "Mixed (Both, year_round)",   "year_round",
                           "Mixed (Both, year_round)",     "spawn"
                           )



  tmdl_names <- odeqtmdl::tmdl_actions |>
    dplyr::select(action_id, TMDL_name) |>
    dplyr::distinct()


  tmdl_ws <- odeqtmdl::tmdl_au |>
    filter(!AU_ID == "99") |>
    filter(action_id == "OR_TMDL_20240806b") %>%
    pull(AU_ID) |>
    unique()


  if(type == 'AU'){





    # prep TMDL info --------------------------------------------------------------------------------------------------

#filter out <= 1 % TMDL coverage- This reduces inclusion of dangles
    tmdl_au0 <- odeqtmdl::tmdl_au |>
      #dplyr::filter(TMDL_AU_Percent > 1) |>
      dplyr::left_join(tmdl_actual_periods, relationship = "many-to-many") |>
      dplyr::rename(TMDL_Period = Period,
                    period = Actual_period) |>
      dplyr::left_join(tmdl_names)


    TMDLs <- tmdl_au0 %>%
      dplyr::filter(TMDL_scope == "TMDL") |>
      dplyr::left_join(odeqtmdl::tmdl_parameters[, c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant", "TMDL_status")],
                       by = c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant")) %>%
      dplyr::mutate(TMDL_status = case_when(AU_ID %in% tmdl_ws & action_id == "30674" & Pollu_ID == 132 ~ "Not Active",
                                     TRUE ~ TMDL_status)) |>
      dplyr::filter(TMDL_status == 'Active') |>
      dplyr::select(AU_ID, TMDL_name, action_id, Pollu_ID, period, TMDL_Period, TMDL_pollutant, TMDL_status) |>
      dplyr::group_by(AU_ID, Pollu_ID, period) |>
      dplyr::summarise(TMDLs =  stringr::str_c(unique(TMDL_name), collapse = "; "),
                       action_ids =  stringr::str_c(unique(action_id), collapse = "; "),
                       TMDL_pollutants = stringr::str_c(unique(TMDL_pollutant), collapse = "; "),
                       TMDL_Periods = stringr::str_c(unique(TMDL_Period), collapse = "; "),
                       TMDL_Periods = stringr::str_c(unique(TMDL_Period), collapse = "; ")) |>
      dplyr::mutate(Pollu_ID = as.character(Pollu_ID))

    AU_display_TMDL <- df |>
      dplyr::left_join(TMDLs) |>
      dplyr::mutate(final_AU_cat = dplyr::case_when(final_AU_cat == '5' & !is.na(action_ids) ~ '4A',
                                                    .default = final_AU_cat))


    return(AU_display_TMDL)
  } else {


    tmdl_au_gnis0 <- odeqtmdl::tmdl_au_gnis |>
      dplyr::left_join(tmdl_actual_periods, relationship = "many-to-many") |>
      dplyr::rename(TMDL_Period = Period,
                    period = Actual_period)|>
      dplyr::left_join(tmdl_names)


    TMDL_GNIS <- tmdl_au_gnis0 |>
      dplyr::filter(TMDL_scope == "TMDL") |>
      dplyr::mutate(AU_GNIS_Name = stringr::str_split_i(AU_GNIS, ";", 2)) |>
      dplyr::left_join(odeqtmdl::tmdl_parameters[, c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant", "TMDL_status")],
                       by = c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant")) %>%
      dplyr::mutate(TMDL_status = case_when(AU_ID %in% tmdl_ws & action_id == "30674" & Pollu_ID == 132 ~ "Not Active",
                                            TRUE ~ TMDL_status)) |>
      dplyr::filter(TMDL_status == 'Active') |>
      dplyr::select(AU_ID, AU_GNIS_Name, TMDL_name, action_id, Pollu_ID, period, TMDL_Period, TMDL_pollutant, TMDL_status) |>
      dplyr::group_by(AU_ID, AU_GNIS_Name, Pollu_ID, period) |>
      dplyr::summarise(TMDLs =  stringr::str_c(unique(TMDL_name), collapse = "; "),
                       action_ids =  stringr::str_c(str_unique(action_id), collapse = "; "),
                       TMDL_pollutants = stringr::str_c(unique(TMDL_pollutant), collapse = "; "),
                       TMDL_Periods = stringr::str_c(str_unique(TMDL_Period), collapse = "; ")) |>
      dplyr::mutate(Pollu_ID = as.character(Pollu_ID))


    WS_GNIS_rollup_delist_TMDL <- df |>
      dplyr::left_join(TMDL_GNIS) |>
      dplyr::mutate(final_GNIS_cat = dplyr::case_when(final_GNIS_cat == '5' & !is.na(action_ids) ~ '4A',
                                                      .default = final_GNIS_cat))

    return(WS_GNIS_rollup_delist_TMDL)


  }


}
