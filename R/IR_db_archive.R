#' archive_IRdb
#'
#' Archives previous cycle's IR database
#' This function archives the finished IR cycle in the IR database. This function should be run at the end of the IR
#' Cycle before any data is imported into a new IR cycle.
#'
#'
#' @param IR_cycle IR cycle to be archived. Should be a 4 digit year. YYYY
#' @param db IR database connection name
#' @return Returns nothing. Creates new tables in IR database
#' @export
#'

archive_IRdb <- function(IR_cycle, db){
  # Error checking. Make sure
  assertthat::assert_that(assertthat::is.number(IR_cycle), floor(log10(IR_cycle)) + 1 == 4,
                          dplyr::between(IR_cycle, 2022, 2050),
                          msg = "IR_cycle must be a year in YYYY format.")

  con <- DBI::dbConnect(odbc::odbc(), db)

  DBI::dbSendQuery(con,glue::glue_sql("EXEC archive_cycle @ir_year = {IR_cycle}", .con = con))


}
