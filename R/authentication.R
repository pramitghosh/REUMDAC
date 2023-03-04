#' Authenticate access using EUMETSAT's API
#'
#' Consumer Key and Consumer Secret are available to authenticated users through 
#' EUMETSAT's EO Portal: https://api.eumetsat.int/api-key/.
#'
#' @param consumer_key EUMETSAT Consumer Key
#' @param consumer_secret EUMETSAT Consumer Secret
#'
#' @return API Access Token as a String
#' @export
#'
#' @importFrom base64enc base64encode
#' @import httr
#' 
#' @examples
#' if(FALSE)
#' {
#'   authenticate("consumer_key_goes_here", "consumer_secret_goes_here")
#' }
#' 
authenticate = function(consumer_key, consumer_secret)
{
  str = base64enc::base64encode(charToRaw(paste(consumer_key, consumer_secret, sep = ":")))
  endpoint = "https://api.eumetsat.int/token"
  token = httr::POST(endpoint, httr::add_headers(Authorization = paste("Basic", str, sep = " ")), body = list(grant_type = "client_credentials"), encode = "form")
  
  if(token$status_code != 200)
    return(NULL)
  
  return(httr::content(token)$access_token)
}
