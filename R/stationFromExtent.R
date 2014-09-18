#' Identify stations within user-defined extent from map
#' 
#' @export stationFromExtent
stationFromExtent <- function(mapRegion = "world",
                              bb = NULL, 
                             ...) {
 
  # Manually draw bounding box if not supplied
  if (is.null(bb)) {

    # Display plain map of desired extent
    mapGriddedData(mapRegion = mapRegion, addLegend = FALSE, plotData = FALSE, 
                   borderCol = "black")
    
    # Draw extent on map
    bb <- drawExtent()
  }
  
  # Crop available stations by drawn extent and return
  stations <- gsodReformat(data = gsodstations, df2sp = TRUE)
  stations <- crop(stations, bb)
  
  return(stations)
}
  