#' Aluminum default DOC Lookup
#'
#' This function looks up default DOC values used in calculating Al criteria. The function sends lat/long values via
#' a query to a Oregon DEQ map server to determine deafult DOC values. DOC values are based on georegion as described in
#' ##insert link to interpretation doc when finalized##. If -9999 is returned, station is outside of georeferenced area.
#'
#' @param lat Latitude of station
#' @param long Longitude of station
#' @return default DOC value. If -9999 is returned, station is outside of georeferenced area.
#' @export
#' @examples
#' \dontrun{
#'Al_default_DOC(lat = 45.80369 , long = -123.9551)
#'
#'aluminum_data_mlocs <- aluminum_data %>%
#'  select(MLocID, Lat_DD, Long_DD) %>%
#'  distinct() %>%
#'  rowwise() %>%
#'  mutate(def_DOC = Al_default_DOC(Lat_DD, Long_DD))
#'}


Al_default_DOC <- function(lat, long){

  path_region <- paste0("https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/OR_Ecoregions_Aluminum/MapServer/0/query?geometry=",
                        long,"%2C+",
                        lat,
                        "&geometryType=esriGeometryPoint&inSR=4269&spatialRel=esriSpatialRelWithin&outFields=Def_DOC+&returnGeometry=false&returnTrueCurves=false&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&queryByDistance=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=geojson")

  request_region <- httr::GET(url = path_region)

  response_region <- httr::content(request_region, as = "text", encoding = "UTF-8")

  region_df<- geojsonsf::geojson_sf(response_region) %>%
    sf::st_drop_geometry()

  if(is.null(region_df[1,1])) {
    print(paste("Lat/Long (",lat, ",", long,") out of area"))
    return(NA_real_)
  } else {

    return(region_df[1,1])
  }

}






