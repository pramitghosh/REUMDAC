#' Returns the footprint for a GEO product
#' 
#' GEO products do not have an individual footprint. Instead, this resource represents a GEO footprint for the respective sensor mode and sub satellite longitude.
#'
#' @param sensorMode Sensor Mode; one of: {ALTHRV, REDSCN, NOMSCN}
#' @param subSatelliteLongitude Sub Satellite longitude in degrees
#' @param returnPolygon TRUE if a polygon should be returned; FALSE to return the response
#' @param plotPolygon if the polygon is to be plotted
#'
#' @return If returnPolygon is TRUE then a sf polygon else the response as an R list
#' 
#' @import httr
#' @import sf
#' @importFrom rnaturalearth ne_countries
#' @export
#'
#' @examples
#' footprint("ALTHRV", -50)
#' footprint("NOMSCN", 0, returnPolygon = FALSE, plotPolygon = TRUE)
#' 
footprint = function(sensorMode, subSatelliteLongitude, returnPolygon = TRUE, plotPolygon = FALSE)
{
  # curl -X 'GET' \
  # 'https://api.eumetsat.int/data/browse/footprints/{sensorMode}/{subSatelliteLongitude}' \
  # -H 'accept: application/geo+json'
  
  baseURL = "https://api.eumetsat.int/"
  path = paste("data/browse/footprints", sensorMode, subSatelliteLongitude, sep = "/")
  response = GET(baseURL,
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
    if(requireNamespace("rnaturalearth", quietly = TRUE))
    {
      world = st_transform(rnaturalearth::ne_countries(scale = "medium", returnclass = "sf"), crs = 4326)
      plot(st_geometry(world), axes = TRUE)
      plot(st_geometry(sf_polygon), border = "red", add = TRUE)
    } else print("Namespace for 'rnaturalearth' not found; aborting plotting!")
  }
  
  if(returnPolygon == TRUE)
  {
    return(sf_polygon)
  }
  
  return(content(response))
}
