#' DO saturation percent calculator
#'
#' This function will calculate DO saturation percentage
#' based on DO mg/L values, temperature in C, and elevation in ft
#' This function is based on the equation found in The Dissolved
#' Oxygen Water Quality Standard Implementation Guidence.
#' This function differs from the oxySol function in the wql package
#' because it calculates the percentage directly and incorporates elevation,
#' as opposed to pressure
#'
#' @param DO DO value in mg/L
#' @param TempC Temperature value in C
#' @param elevft Elevation value in feet
#' @export
#' @examples
#' DOSat.calc()

DOSat_calc <- function(DO, TempC, elevft) {
  DO / (exp(-139.34411 + ((1.575701*10^5)/(TempC+273.15)) -
              ((6.642308 * 10^7)/((TempC+273.15)^2)) +
              ((1.243800 * 10^10)/((TempC+273.15)^3)) -
              ((8.621949 * 10^11)/((TempC+273.15)^4))) *
          (1 - (0.0001148 * elevft/3.281 ))) * 100
}
