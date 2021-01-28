#' binomial_excursions
#'
#' This function returns the number of excursions required to list as impaired. Using type = "Toxics" represents 90%
#' confidence that <= 5% samples exceed criterion value. type = "Conventionals" represnts 90% confidnce that <= 10%
#' samples exceed criterion value.
#' @param n number of results.
#' @param type "Toxics" or "Conventionals" for parameter type.
#' @return Value to list if number of excursions exceed.
#' @export
#' @examples
#' binomial_excursions(30, type = "Toxics")

binomial_excursions <- function(n,  type = c("Toxics", "Conventionals")){

  if(length(type) > 1){
    stop("type must be specified: 'Toxics' or 'Conventionals'")
  }

  if(!type %in% c("Toxics", "Conventionals") | is.null(type)){
    stop("type must be specified: 'Toxics' or 'Conventionals'")
  }

  if(type == "Conventionals"){

  x = ifelse(n <= 11, 2, qbinom(0.90, n, 0.10, lower.tail = TRUE)+1 )
  return(x)
  }


  if(type == "Toxics"){

    x = ifelse(n <= 18, 2, qbinom(0.90, n, 0.05, lower.tail = TRUE)+1)
    return(x)
  }

}




