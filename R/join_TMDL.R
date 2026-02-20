#' join_TMDL
#'
#' Join assessments to TMDL actions. used to determine categery 4A and to display action IDs in display
#'
#' @param df dataframe to join. Shoud;d either be final AU decsion table or final GNIS decision table, after
#'            all delist etc completed
#' @param type should be 'AU' or 'GNIS' depending on what you ware matching
#' @returns Returns same datafrane as df, but with action ID and TMDL parameters added (TMDLs, action_ids, TMDL_pollutants, TMDL_Periods)
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




  IR.sql <-  DBI::dbConnect(odbc::odbc(), 'IR_Dev')


  LU_BU_Assessment <-  dplyr::tbl(IR.sql, 'LU_BU_Assessment') |>
    dplyr::collect() |>
    dplyr::mutate(Pollu_ID = as.numeric(Pollu_ID))

  DBI::dbDisconnect(IR.sql)


  tmdl_ben_use <- odeqtmdl::tmdl_ben_use |>
    dplyr::left_join(LU_BU_Assessment, relationship = "many-to-many")




  if(type == 'AU'){





    # prep TMDL info --------------------------------------------------------------------------------------------------

#filter out <= 1 % TMDL coverage- This reduces inclusion of dangles

    TMDL_AU <- odeqtmdl::tmdl_au |>
      dplyr::filter(TMDL_scope == "TMDL",
                    TMDL_status == 'Active') |>
      dplyr::left_join(tmdl_ben_use, relationship = "many-to-many") |>
      dplyr::group_by(AU_ID, TMDL_parameter, TMDL_pollutant, action_id, Pollu_ID, wqstd_code, Period) |>
      dplyr::left_join(tmdl_actual_periods, relationship = "many-to-many") |>
      dplyr::rename(TMDL_Period = Period,
                    period = Actual_period) |>
      dplyr::left_join(tmdl_names) |>
      dplyr::group_by(AU_ID, Pollu_ID,wqstd_code, period) |>
      dplyr::summarise(TMDLs =  stringr::str_c(unique(TMDL_name), collapse = "; "),
                       action_ids =  stringr::str_c(unique(action_id), collapse = "; "),
                       TMDL_pollutants = stringr::str_c(unique(TMDL_pollutant), collapse = "; "),
                       TMDL_Periods = stringr::str_c(unique(TMDL_Period), collapse = "; ") )

    AU_display_TMDL <- df |>
      dplyr::left_join(TMDL_AU) |>
      dplyr::mutate(final_AU_cat = dplyr::case_when(final_AU_cat == '5' & !is.na(action_ids) ~ '4A',
                                                    final_AU_cat == '4A' & is.na(action_ids) ~ '5',
                                                    TRUE ~ final_AU_cat))


    return(AU_display_TMDL)
  } else {


    TMDL_GNIS <- odeqtmdl::tmdl_au_gnis |>
      dplyr::filter(TMDL_scope == "TMDL",
                    TMDL_status == 'Active') |>
      dplyr::left_join(tmdl_ben_use, relationship = "many-to-many") |>
      #dplyr::group_by(AU_ID, TMDL_parameter, TMDL_pollutant, action_id, Pollu_ID, wqstd_code, Period) |>
      dplyr::left_join(tmdl_actual_periods, relationship = "many-to-many") |>
      dplyr::rename(TMDL_Period = Period,
                    period = Actual_period) |>
      dplyr::left_join(tmdl_names) |>
      dplyr::group_by(AU_ID,AU_GNIS_Name, Pollu_ID,wqstd_code, period) |>
      dplyr::summarise(TMDLs =  stringr::str_c(unique(TMDL_name), collapse = "; "),
                       action_ids =  stringr::str_c(unique(action_id), collapse = "; "),
                       TMDL_pollutants = stringr::str_c(unique(TMDL_pollutant), collapse = "; "),
                       TMDL_Periods = stringr::str_c(unique(TMDL_Period), collapse = "; ") )


    WS_GNIS_rollup_delist_TMDL <- df |>
      dplyr::left_join(TMDL_GNIS) |>
      dplyr::mutate(final_GNIS_cat = dplyr::case_when(final_GNIS_cat == '5' & !is.na(action_ids) ~ '4A',
                                                      final_GNIS_cat == '4A' & is.na(action_ids) ~ '5',
                                                      TRUE ~ final_GNIS_cat))

    return(WS_GNIS_rollup_delist_TMDL)


  }


}
