#' Unzip and merge downloaded GSOD files
#' 
#' @export gzGsodStations
gzGsodStations <- function(dsn = ".",
                           start.year = NULL, 
                           end.year = NULL, 
                           save.output = FALSE,
                           remove.gz = FALSE,
                           remove.op = FALSE,
                           ...) {
  
  # Unzip downloaded *.gz files (Note: not sure if this works on a Windows OS)
  system(paste0("gunzip ", getwd(), "/", dsn, "/*.gz"), 
         intern = FALSE, ignore.stderr = TRUE)
  
  # Optionally remove *.gz files
  if (remove.gz) {
    fls <- list.files(dsn, pattern = ".gz$", full.names = TRUE)
    file.remove(fls)
  }
    
  # List available *.op files
  files <- list.files(dsn, pattern = "*.op$", full.names = TRUE)
  
  # Subset files by supplied temporal range
  start.year <- as.numeric(start.year)
  end.year <- as.numeric(end.year)
  index <- sort(unlist(sapply(seq(start.year, end.year), function(i) {
    grep(i, files)
  })))
  files <- files[index]
  
  # Available GSOD stations
  stations <- unique(substr(basename(files), 1, 6))
  
  # Define fixed column widths
  column.widths <- c(6, 1, 5, 2, 8, 2, 6, 1, 2, 2, 6, 1, 2, 2, 6, 1, 2, 2, 6, 1, 
                     2, 2, 5, 1, 2, 2, 5, 1, 2, 2, 5, 2, 5, 2, 6, 1, 1, 6, 1, 1, 
                     5, 1, 1, 5, 2, 6)
  
  # Loop through all GSOD stations available in 'dsn' folder
  ls.df.all <- lapply(stations, function(h) {
    
    # Loop through all available GSOD files per GSOD station
    df.all <- do.call("rbind", lapply(files[grep(h, files)], function(i) {
      
      # Import fixed width formatted data
      df <- read.fwf(i, column.widths, header = FALSE, skip = 1, fill = TRUE,
                     na.strings = c("999.9", "9999.9", "99.99"),
                     stringsAsFactors = FALSE)
      
      # Remove redundant columns
      df <- df[, -c(seq(2, 34, 2), 37, 40, 43, 44)]
      
      # Set column names
      names(df) <- c("STN---", "WBAN", "YEARMODA", "TEMP", "NC", "DEWP", "NC", 
                     "SLP", "NC", "STP", "NC", "VISIB", "NC", "WDSP", "NC", 
                     "MXSPD", "GUST", "MAX", "MAXFLAG", "MIN", "MINFLAG", "PRCP", 
                     "PRCPFLAG", "SNDP", "FRSHTT")
      
      # Return annual data per station
      return(df)
    }))
    
    # Save (optional) and return merged annual data per station
    if (save.output)
      write.csv(df.all, paste0(dsn, "/", h, "-99999-", start.year, "-", 
                               end.year, ".csv"), row.names = FALSE)
    
    # Optionally remove *.op files
    if (remove.op) {
      fls <- list.files(dsn, pattern = ".op$", full.names = TRUE)
      file.remove(fls)
    }
    
    return(df.all)
  })
  
  # Return merged annual data for all stations
  return(ls.df.all)
}