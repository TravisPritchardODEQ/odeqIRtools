#' join_AUs
#'
#' This function will join a dataframe by HUC12 number to HUC number and names for HUC 4 - 12. This function is used
#' to transfrom HUC12 in the geodatabase layer to additional HUC information to be used in tool sorting. The huc info
#' is stored in the HUC_info dataset
#'
#' @param df dataframe to join
#' @param AU_ID_col Field that identifies AU_ID
#' @export





join_AU_info <- function(df, AU_ID_col = 'AU_ID'){


  df_join <- df |>
    left_join(AU_info, by = join_by({{AU_ID_col}} == AU_ID)) |>
    relocate(AU_Name, .after = {{AU_ID_col}}) |>
    relocate(AU_UseCode, .after = AU_Name) |>
    relocate(HUC12, .after = AU_UseCode)

  return(df_join)
}





