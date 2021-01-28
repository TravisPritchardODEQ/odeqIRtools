#' Censor_Data
#'
#' This function deals with censored data as described in the white paper "Use of Censored Data" from 5/11/2018
#' When the QL > criteria value, 1/2 the value of the criteria is substituted for any sample reported as censored
#' When the QL < Criteria value, 1/2 the QL value will be substituted
#' Samples greater than the Max QL, use QL value
#' Input is a dataframe
#' Adds a column named Result_cen for the value to use
#' @param df dataframe to modify.
#' @param result_col column in dataframe with the result to be compared against. Defaults to IRResultNWQSunit
#' @param qualifier_col column in dataframe for the qualifier. Defaults to Result_Operator
#' @param criteria_col column in dataframe with criteria value. Defaults to crit
#' @export
#'

censor_data <- function(df,
                          result_col= IRResultNWQSunit,
                          qualifier_col = Result_Operator,
                          criteria_col = crit) {

  res_type <- dplyr::pull(df, {{result_col}})
  crit_type <- dplyr::pull(df, {{criteria_col}})

  if(!is.numeric(res_type)) {
    stop("result_col needs to be numeric type")
  }


  if(!is.numeric(crit_type)) {
    stop("criteria_col needs to be numeric type")
  }


  # Perform censored data modifications
  Results_censored <- df %>%
    dplyr::mutate( Result_cen = dplyr::case_when({{ qualifier_col }} == "=" ~ {{ result_col }},
                                                   {{ qualifier_col }} == ">" ~ {{ result_col }},
                                                  {{ qualifier_col }} == "<" & {{ result_col }} >= {{ criteria_col }} ~ 0.5 * {{ criteria_col }},
                                                  {{ qualifier_col }} == "<" &
                                                    {{ result_col }} < {{ criteria_col }} ~ 0.5 * {{ result_col }}
                                                 ))

  return(Results_censored)
}




