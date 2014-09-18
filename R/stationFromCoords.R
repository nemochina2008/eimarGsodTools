#' Identify nearby GSOD stations based on a given geographic location
#' 
#' @export stationFromCoords
stationFromCoords <- function(x, 
                            y = NULL, 
                            locations, 
                            width = 50, 
                            id = FALSE, 
                            ...) {
  
  # Calculate distance from point of interest to supplied locations
  coords <- coordinates(locations)
  x.to.locations <- sapply(seq(nrow(coordinates(locations))), function(i) {
    as.numeric(geodist(Nfrom = y, Efrom = x, 
                       Nto = coords[i, 2], Eto = coords[i, 1]))
  })
  # Add calculated distances to locations
  locations$DIST <- round(x.to.locations, digits = 3)
  
  # Identify and return GSOD stations (or referring station ID) that lie within 
  # the given buffer width
  index <- which(x.to.locations <= width)
  
  if (id) {
    return(locations$USAF[index])
  } else {
    return(locations[index, ])
  }
}