#' Convert GSOD station list to sp::SpatialPolygonsDataFrame
#' 
#' @export gsodDf2Sp
gsodDf2Sp <- function(data, 
                      ...) {

  # Required packages
  lib <- c("rgdal", "sp")
  sapply(lib, function(x) stopifnot(require(x, character.only = TRUE)))
  
  # Subset data by valid coordinates and convert to SpatialPointsDataFrame
  data.lonlat <- subset(data, !is.na(LON) & !is.na(LAT))
  
  coordinates(data.lonlat) <- ~ LON + LAT
  proj4string(data.lonlat) <- CRS("+init=epsg:4326")
  
  return(data.lonlat)
}