#' Identify nearby GSOD stations based on a given geographic location
#' 
#' @export stationFromCoords
stationFromCoords <- function(x, 
                              y = NULL, 
                              width = 50, 
                              ...) {
  
  # Calculate distance from point of interest to supplied stations
  stations <- gsodReformat(gsodstations, df2sp = TRUE)
  coords <- coordinates(stations)
  x.to.stations <- sapply(seq(nrow(coordinates(stations))), function(i) {
    as.numeric(geodist(Nfrom = y, Efrom = x, 
                       Nto = coords[i, 2], Eto = coords[i, 1]))
  })
  # Add calculated distances to stations
  stations$DIST <- round(x.to.stations, digits = 3)
  
  # Identify and return GSOD stations that lie within the given buffer width
  index <- which(x.to.stations <= width)
  
  return(stations[index, ])
}