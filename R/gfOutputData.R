#' Julendat function to generate gap-filled output data
#' 
#' @export gfOutputData
gfOutputData <- function(data.dep,
                         plevel, 
                         ...) {
  
  # Define columns of output data frame
  datetime <- data.dep@Datetime
  timezone <- rep(data.dep@Timezone, length(datetime))
  aggregationtime <- rep(data.dep@Aggregationtime, length(datetime))
  plotid <- data.dep@PlotId$Shortname
  epplotid <- data.dep@EpPlotId
  stationid <- data.dep@StationId$Longname
  processlevel <- rep(plevel, length(datetime))
  qualityflag <- data.dep@Qualityflag
  parameter <- as.data.frame(data.dep@Parameter)
  
  # Return output data frame
  return(data.frame(Datetime = datetime, 
                    Timezone = timezone,
                    Aggregationtime = aggregationtime,
                    PlotId = plotid,
                    EpPlotId = epplotid,
                    StationId = stationid,
                    Processlevel = processlevel,
                    Qualityflag = qualityflag,
                    parameter, 
                    stringsAsFactors = F))
}