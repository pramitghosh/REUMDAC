#' Queries the EUMETSAT Discover API
#' 
#' Discover EUMETSAT Data Store Earth Observation data at the collection level.
#' 
#' @param hits_only Return details of the hits only
#'
#' @return If `hits_only` is FALSE, the entire response is returned as an R list, otherwise only the details of the hits.
#' @export
#' 
#' @import httr
#' @examples
#' discover()
#' discover(hits_only = TRUE)
#'
discover = function(hits_only = FALSE)
{
  # curl -X 'GET' \
  # 'https://api.eumetsat.int/product-navigator/0.0.1/csw/record/_search' \
  # -H 'accept: application/json'
  
  token = GET("https://api.eumetsat.int/product-navigator/0.0.1/csw/record/_search",
                    add_headers(accept = "application/json"))
  
  if(token$status_code != 200)
    return(NULL)
  
  if(hits_only)
    return(content(token)$hits$hits)
  
  return(content(token))
}
