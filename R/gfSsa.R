#' Impute missing values based on singular spectrum analysis (SSA)
#' 
#' @export gfSsa
gfSsa <- function(data, 
                  prm = "TEMP", 
                  reversed.forecast = F, 
                  origin = "1970-01-01", 
                  save.output = F, 
                  ...) {
  
  # Required packages
  lib <- c("zoo", "Rssa")
  sapply(lib, function(x) stopifnot(require(x, character.only = TRUE)))
  
  # Read file into ki.data object (optional)
  if (class(data) == "character")
    data <- as.ki.data(data)
  
  # Duplicate input data
  data.rev <- data
  
  # Loop through all columns to be gap-filled via SSA
  filled.data <- lapply(prm, function(h) {

    # Reverse time series prior to forecasting (optional)
    tmp.rev <- if (reversed.forecast) {
      rev(as.numeric(data@Parameter[[h]]))
    } else {
      as.numeric(data@Parameter[[h]])
    }
    
    # Convert numeric vector to 'zoo' time series
    tmp.rev.ts <- zoo(tmp.rev, order.by = as.Date(index(data@Parameter[[h]]), 
                                                  origin = origin))
    # Insert potentially reversed time series into referring parameter slot
    data.rev@Parameter[[h]] <- as.numeric(tmp.rev.ts)
    
    # Identify lengths of measurement gaps
    data.rev.na <- which(is.na(data.rev@Parameter[[h]]))
    ki.rev.na <- do.call(function(...) {
      tmp <- rbind(...)
      names(tmp) <- c("start", "end", "span")
      return(tmp)}, gfGapLength(data.dep = data.rev,
                                pos.na = data.rev.na, 
                                gap.limit = 999999, 
                                end.datetime = Sys.Date(), 
                                units = "days"))
    
    # As long as gaps exists, do the following stuff ...
    while (length(ki.rev.na) > 0) {
      
      # Identify lengths of continuous measurements
      ki.rev.nona <- do.call("rbind", gfNogapLength(gap.lengths = ki.rev.na, 
                                                    data.dep = data.rev))
      
      # Deconstruct continuous measurement series
      tmp.ssa <- 
        ssa(data.rev@Parameter[[h]][ki.rev.nona[1, 1]:ki.rev.nona[1, 2]], 
            L = if (ki.rev.nona[1, 3] > 365) {
              365
            } else if (ki.rev.nona[1, 3] <= 365 & ki.rev.nona[1, 3] > 182) {
              182
            } else {
              cat("Time series not sufficiently continuous to perform SSA!")
              break
            })

      # Forecast the next gap
      data.rev@Parameter[[h]][ki.rev.na[1,1] : ki.rev.na[1,2]] <-
        forecast(tmp.ssa, groups = list(seq(nlambda(tmp.ssa))), len = ki.rev.na[1,3])$mean
      
      # Update lengths of measurement gaps
      data.rev.na <- which(is.na(data.rev@Parameter[[h]]))
      if (length(data.rev.na) > 0) {
        ki.rev.na <- do.call(function(...) {
          tmp <- rbind(...)
          names(tmp) <- c("start", "end", "span")
          return(tmp)}, gfGapLength(data.dep = data.rev,
                                    pos.na = data.rev.na, 
                                    gap.limit = 999999, 
                                    end.datetime = Sys.Date(), 
                                    units = "days"))
      } else {
        ki.rev.na <- list()
      }
    }
    
    # Replace gappy by filled time series
    if (reversed.forecast) {
      tmp <- rev(as.numeric(data.rev@Parameter[[h]]))
    } else {
      tmp <- as.numeric(data.rev@Parameter[[h]])
    }
    
    return(tmp)
  })
  
  # Insert gap-filled time series into referring slots
  for (j in seq(filled.data)) 
    data@Parameter[[j]] <- filled.data[[j]]
  
  # Save (optional) and return output data
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
  
  # Return gap-filled data sets
  return(data)
}