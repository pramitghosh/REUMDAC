#' Authenticate access using EUMETSAT's API
#'
#' Consumer Key and Consumer Secret are available to authenticated users through 
#' EUMETSAT's EO Portal: https://api.eumetsat.int/api-key/.
#'
#' @param consumer_key EUMETSAT Consumer Key (default NULL; attempts to read from file)
#' @param consumer_secret EUMETSAT Consumer Secret (default NULL; attempts to read from file)
#' @param save_creds Whether credentials are to be stored in a file for later use (default TRUE)
#' @param return_header Returns a httr::add_headers object with access token (default FALSE)
#'
#' @return API Access Token as a String or a httr header object
#' @export
#'
#' @importFrom base64enc base64encode
#' @import httr
#' @importFrom fs path_home
#' @importFrom utils read.csv
#' 
#' @examples
#' if(FALSE)
#' {
#'   authenticate("consumer_key_goes_here", "consumer_secret_goes_here")
#' }
#' 
authenticator = function(consumer_key = NULL, consumer_secret = NULL, save_creds = TRUE, return_header = FALSE)
{
  path = file.path(fs::path_home(), '.reumdac', 'credentials')
  
  if(is.null(consumer_key) || is.null(consumer_secret))
  {
    print('Credentials not provided; attempting to read from file')
    cred_vals = utils::read.csv(path, header = FALSE)
    # print(cred_vals)
    consumer_key = as.character(cred_vals[1])
    consumer_secret = as.character(cred_vals[2])
  }
  
  str = base64enc::base64encode(charToRaw(paste(consumer_key, consumer_secret, sep = ":")))
  
  endpoint = "https://api.eumetsat.int/token"
  token = httr::POST(endpoint, httr::add_headers(Authorization = paste("Basic", str, sep = " ")), body = list(grant_type = "client_credentials"), encode = "form")
  
  if(token$status_code != 200)
  {
    print('Authentication was unsuccessful!')
    return(NULL)
  }
  else
    print('Authentication successful!')
  
  if(save_creds == TRUE && file.exists(path) == FALSE)
  {
    print('No credentials found on file; saving new credentials')
    dir.create(file.path(fs::path_home(), '.reumdac'))#, mode = '0400')
    file.create(path)
    writeLines(paste(consumer_key, consumer_secret, sep = ','), file.path(fs::path_home(), '.reumdac', 'credentials'), sep = '')
  }
  
  if(return_header == TRUE)
  {
    return(httr::add_headers(Authorization = paste("Bearer ", httr::content(token)$access_token, sep = " ")))
  }
  else
    return(httr::content(token)$access_token)
}
