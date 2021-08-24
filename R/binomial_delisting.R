#' binomial_delisting
#'
#' This function returns the number of excursions required to delist. Using type = "Toxics" represents 90%
#' confidence that <= 5% samples exceed criterion value. type = "Conventionals" represnts 90% confidnce that <= 10%
#' samples exceed criterion value.
#' @param n number of results.
#' @param type "Toxics" or "Conventionals" for parameter type.
#' @return Value to delist if number of excursions is below.
#' @export
#' @examples
#' binomial_delisting(30, type = "Toxics")

binomial_delisting<- function(n,  type = c("Toxics", "Conventionals")){

  if(length(type) > 1){
    stop("type must be specified: 'Toxics' or 'Conventionals'")
  }

  if(!type %in% c("Toxics", "Conventionals") | is.null(type)){
    stop("type must be specified: 'Toxics' or 'Conventionals'")
  }

  if(type == "Conventionals"){

    x = ifelse(n < 15, NA_real_, qbinom(0.90, n, 0.10, lower.tail = TRUE ) -1 )
    return(x)
  }


  if(type == "Toxics"){

    x = ifelse(n < 18, NA_real_,  qbinom(0.90, n, 0.05, lower.tail = TRUE)-1)
    return(x)
  }

}




