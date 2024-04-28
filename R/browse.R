#' Returns the footprint for a GEO product
#' 
#' GEO products do not have an individual footprint. Instead, this resource represents a GEO footprint for the respective sensor mode and sub satellite longitude.
#'
#' @param sensorMode Sensor Mode; one of: {ALTHRV, REDSCN, NOMSCN}
#' @param subSatelliteLongitude Sub Satellite longitude in degrees (defaults to 0 degrees)
#' @param returnPolygon TRUE if a polygon should be returned; FALSE to return the response
#' @param plotPolygon if the polygon is to be plotted
#' @param authenticated whether to send authenticated API requests (defaults to TRUE)
#'
#' @return If returnPolygon is TRUE then a sf polygon else the response as an R list
#' 
#' @import httr
#' @import sf
#' @import rnaturalearthdata
#' @importFrom rnaturalearth ne_countries
#' @export
#'
#' @examples
#' footprint("ALTHRV", -50, authenticated = FALSE)
#' footprint("NOMSCN", returnPolygon = FALSE, plotPolygon = TRUE, authenticated = FALSE)
#' 
footprint = function(sensorMode, subSatelliteLongitude = 0, returnPolygon = TRUE, plotPolygon = FALSE, authenticated = TRUE)
{
  # curl -X 'GET' \
  # 'https://api.eumetsat.int/data/browse/footprints/{sensorMode}/{subSatelliteLongitude}' \
  # -H 'accept: application/geo+json'
  
  baseURL = "https://api.eumetsat.int/"
  path = paste("data/browse/footprints", sensorMode, subSatelliteLongitude, sep = "/")
  
  if(authenticated == TRUE)
    response = httr::GET(baseURL,
                path = path,
                add_headers(accept = "application/json"),
                authenticator(return_header = TRUE))
  else
    response = httr::GET(baseURL,
                         path = path,
                         add_headers(accept = "application/json"))
  
  if(response$status_code != 200)
    return(NULL)
  
  coordinates = content(response)$coordinates
  
  coordinates = unlist(coordinates, recursive = FALSE)
  coordinates = lapply(coordinates, FUN = function(X) as.matrix(unlist(X)))
  coordinates = lapply(coordinates, FUN = function(X) t(as.matrix(X)))
  coordinates = matrix(unlist(coordinates), byrow = TRUE, ncol = 2)
  
  sf_polygon = st_sf(st_sfc(st_polygon(list(coordinates)), crs = 4326))
  
  if(plotPolygon == TRUE)
  {
    sf_use_s2(TRUE)
    crs_string = paste("+proj=laea +lat_0=0 +lon_0", subSatelliteLongitude, sep = '=')
    laea = st_crs(crs_string) # Lambert equal area
    
    world = st_transform(rnaturalearth::ne_countries(scale = "medium", returnclass = "sf"), crs = 4326)
    world_s2 = st_transform(world, laea)
    sf_polygon_s2 = st_transform(sf_polygon, laea)
    
    bb = st_bbox(sf_polygon_s2)
    
    plot(st_geometry(world_s2), col = "grey", xlim = c(bb[1], bb[3]), ylim = c(bb[2], bb[4]), axes = FALSE)
    plot(st_geometry(sf_polygon_s2), border = "red", add = TRUE)
  }
  
  if(returnPolygon == TRUE)
  {
    return(sf_polygon)
  }
  
  return(content(response))
}


#' Queries the collections endpoint
#' 
#' Queries the appropriate `collections/` endpoint depending on the arguments passed to it.
#'
#' @param collID Collection ID
#' @param authenticated whether to send authenticated API requests (defaults to TRUE)
#'
#' @return A list with information about collection(s) and the number of products available
#' @export
#'
#' @examples
#' collections(authenticated = FALSE)
collections = function(collID = NULL, authenticated = TRUE)
{
  if(is.null(collID))
  {
    # curl -X 'GET' \
    # 'https://api.eumetsat.int/data/browse/1.0.0/collections?format=json' \
    # -H 'accept: text/html'
    
    baseURL = "https://api.eumetsat.int/"
    path = paste("data/browse/collections")
    
    if(authenticated == TRUE)
      response = httr::GET(baseURL,
                           path = path,
                           query = list(format = "json"),
                           httr::add_headers(accept = "application/json"),
                           authenticator(return_header = TRUE))
    else
      response = httr::GET(baseURL,
                           path = path,
                           query = list(format = "json"),
                           httr::add_headers(accept = "application/json"))
    
    print(paste('Found ', httr::content(response)$numberOfProducts, ' products.'))
    return(httr::content(response))
  }
}

