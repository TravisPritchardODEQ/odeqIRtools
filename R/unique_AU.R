#' unique_AU
#'
#' This function extarcts the unique element from an AU_ID. It will extract the last element defines by the '_' symbol.
#'
#' @param AU_ID AU_ID string to extract unique code portion from
#' @return THe unique portion of an AU_ID as string
#' @example unique_AU('OR_WS_170501100706_05_102965')
#' @export



unique_AU <- function(AU_ID){

  stringr::str_extract(AU_ID, "(?<=_)[^_]*$")


}
