#' Replace statistical outliers with NA
#' 
#' @export outlier2na
outlier2na <- function(data, 
                       prm = "TEMP", 
                       save.output = F,
                       ...) {
  
  # Loop through all provided parameters
  for (j in prm) {
    # Extract time series of current parameter from ki.data object
    vec <- data@Parameter[[j]]
    # Identify outliers
    outliers <- tsOutliers(vec, index = T)
    # Replace identified outliers with NA
    vec[outliers] <- NA
    # Insert adjusted time series into referring slot of ki.data object
    data@Parameter[[j]] <- vec
  }
  
  # Save (optional) and return spike-filtered data set
  if (save.output)
    write.csv(data.frame("Datetime" = data@Datetime, 
                         "Timezone" = data@Timezone, 
                         "Aggregationtime" = data@Aggregationtime, 
                         "PlotId" = data@PlotId$Shortname, 
                         "EpPlotId" = data@EpPlotId, 
                         "StationId" = data@StationId$Shortname, 
                         "Processlevel" = data@Processlevel, 
                         "Qualityflag" = data@Qualityflag, 
                         "TEMP" = data@Parameter$TEMP, 
                         "MAX" = data@Parameter$MAX, 
                         "MIN" = data@Parameter$MIN), ...)
  
  return(data)
}