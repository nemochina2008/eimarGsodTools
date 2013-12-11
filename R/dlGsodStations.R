#' Download selected GSOD data based on supplied station ID
#' 
#' @export dlGsodStations
dlGsodStations <- function(wban,
                           locations = NULL, 
                           start.year.rec, 
                           end.year.rec,
                           dsn = ".", 
                           unzip = T, 
                           ...) {
  
  # Required packages
  lib <- c("sp", "EcoHydRology")
  sapply(lib, function(x) stopifnot(require(x, character.only = T)))

  # Load GSOD station list from scratch if not supplied
  if (is.null(locations)) {
    data(GSOD_history)
    locations <- GSOD_history
  }
  
  # Extract desired station from list of GSOD stations
  dl_wbans <- locations[locations$USAF %in% wban, ]

  # Loop through all GSOD stations to be downloaded
  for (wbans in seq(nrow(dl_wbans))) {
    
    # Verify user-defined temporal range
    start.year <- max(start.year.rec, substr(dl_wbans$BEGIN[wbans], 1, 4))
    end.year <- min(end.year.rec, substr(dl_wbans$END[wbans], 1, 4))

    # Abandon current iteration in case start_year is higher than end_year
    if (start.year > end.year) {
      cat("Skipping GSOD station ", dl_wbans$USAF[wbans], 
          ": Start year is higher than end year!", sep = "")
      next()
    }
    
    cat("Processing GSOD station", dl_wbans$USAF[wbans], "...")
    
    # Download op.gz of current station per year
    for (year in start.year:end.year) {
      # Basename of both URL and destfile
      dlbase <- paste(dl_wbans$USAF[wbans], "-", dl_wbans$WBAN[wbans], "-", 
                      year, ".op.gz", sep = "")
      # URL
      dlurl <- paste0("ftp://ftp.ncdc.noaa.gov/pub/data/gsod/", dlbase)
      # Destfile
      dlfile <- paste0(dsn, "/", dlbase)
      # Download
      print(dlurl)
      try(download.file(dlurl, dlfile), silent = T)
    }
  }
    
  # Unzip downloaded files (optional)
  if (unzip) {
    
    jnk <- gzGsodStations(dsn = dsn, 
                          start.year = start.year, 
                          end.year = end.year, 
                          ...)
    # List and return unzipped .op files
    files <- list.files(dsn, pattern = ".*.op$", full.names = T)
    return(files)
    
  } else {
    
    # List and return downloaded .gz files
    files <- list.files(dsn, pattern = ".*.op.gz$", full.names = T)
    return(files)
  }
}