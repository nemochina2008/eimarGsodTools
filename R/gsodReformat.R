#' Reformat elevation and coordinates in GSOD stations file
#' 
#' @export gsodReformat
gsodReformat <- function(data, 
                         elevation = TRUE, 
                         coords = TRUE, 
                         ...) {

  # Reformat elevation (optional)
  if (elevation)
    data$ELEV..1M. <- data$ELEV..1M. / 10
  
  # Reformat coordinates and eliminate inconsistent coordinates (optional)
  if (coords) {
    for (i in c("LON", "LAT")) {
      data[, i] <- data[, i] / 1000
      data[data[, i] < ifelse(i == "LON", -180, -90) & 
                     !is.na(data[, i]), i] <- NA
      data[data[, i] > ifelse(i == "LON", 180, 90) & 
                     !is.na(data[, i]), i] <- NA
    }
  }
  
  # Return reformatted data
  return(data)
}
