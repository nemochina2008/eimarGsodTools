#' Fill small continuous gaps via linear interpolation
#' 
#' @export gfLinInt
gfLinInt <- function(data, 
                     prm = "TEMP",
                     limit = 5, 
                     width = 11, 
                     save.output = T,
                     ...) {

  # Required packages
  library(zoo)
  library(foreach) 
  

## Linear interpolation over small (n <= limit) measurement gaps

  # Loop through all columns to be gap-filled via linear interpolation
  filled.data <- lapply(prm, function(i) {
    
    # Identify lengths of measurement gaps
    pos.na <- do.call("rbind", 
                      gfGapLength(data.dep = data, 
                                  pos.na = which(is.na(data@Parameter[[i]])), 
                                  gap.limit = 999999, 
                                  end.datetime = Sys.Date(), 
                                  units = "days"))
    # Sufficiently small gaps
    pos.na <- pos.na[which(pos.na[, 3] <= limit), ]
    pos.na.small <- foreach(j = seq(nrow(pos.na)), .combine = "c") %do% {
      seq(pos.na[j, 1], pos.na[j, 2])
    }
    
    # Time series
    tmp.ts <- zoo(data@Parameter[[i]], order.by = as.Date(data@Datetime))
    # Rolling mean (window width = 11)
    tmp.ts.rm <- rollapply(data = tmp.ts, width = width, fill = list(NA, NULL, NA), 
                           partial = T, function(...) mean(..., na.rm = T))
    
    # Replace identified gaps by rolling mean
    tmp.ts[pos.na.small] <- tmp.ts.rm[pos.na.small]
    
    # Return gap-filled data
    return(tmp.ts)
  })
  
  # Insert gap-filled data into referring slots
  for (j in seq(filled.data)) 
    data@Parameter[[j]] <- filled.data[[j]]
  
  # Save (optional) and return gap-filled data
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
