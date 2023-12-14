#' join_hucs
#'
#' This function will join a dataframe by HUC12 number to HUC number and names for HUC 4 - 12. This function is used
#' to transfrom HUC12 in the geodatabase layer to additional HUC information to be used in tool sorting. The huc info
#' is stored in the HUC_info dataset
#'
#' @param df dataframe to join
#' @param hu12_col Field that the original HUC12 number is identified
#' @export





join_hucs <- function(df, hu12_col = 'HUC12'){


  df_join <- df |>
    left_join(HUC_info, by = join_by({{hu12_col}} == HUC12))

  return(df_join)
}
