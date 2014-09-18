#' Download selected GSOD data based on supplied station ID
#' 
#' @export dlGsodStations
dlGsodStations <- function(usaf,
                           start.year.rec, 
                           end.year.rec,
                           dsn = ".", 
                           unzip = TRUE, 
                           remove.gz = FALSE,
                           remove.op = FALSE,
                           ...) {
  
  # Load GSOD station list from scratch if not supplied
  locations <- gsodstations
  
  # Extract desired station from list of GSOD stations
  dl.usaf <- locations[locations$USAF %in% usaf, ]

  # Loop through all GSOD stations to be downloaded
  for (usafs in 1:nrow(dl.usaf)) {
    
    # Verify user-defined temporal range
    start.year <- max(start.year.rec, substr(dl.usaf$BEGIN[usafs], 1, 4))
    end.year <- min(end.year.rec, substr(dl.usaf$END[usafs], 1, 4))

    # Abandon current iteration in case start_year is higher than end_year
    if (start.year > end.year) {
      cat("Skipping GSOD station ", dl.usaf$USAF[usafs], 
          ": Start year is higher than end year!", sep = "")
      next()
    }
    
    cat("Processing GSOD station", dl.usaf$USAF[usafs], "... \n")
    
    # Download op.gz of current station per year
    for (year in start.year:end.year) {
      # Basename of both URL and destfile
      dlbase <- paste(dl.usaf$USAF[usafs], "-", dl.usaf$WBAN[usafs], "-", 
                      year, ".op.gz", sep = "")
      # URL
      dlurl <- paste0("ftp://ftp.ncdc.noaa.gov/pub/data/gsod/", year, "/", 
                      dlbase)
      # Destfile
      dlfile <- paste0(dsn, "/", dlbase)
      # Download
      print(dlurl)
      try(download.file(dlurl, dlfile), silent = TRUE)
    }
  }
    
  # Unzip downloaded files (optional)
  if (unzip) {
    
    jnk <- gzGsodStations(dsn = dsn, 
                          start.year = start.year, 
                          end.year = end.year, 
                          ...)
    # List and return unzipped .op files
    files <- list.files(dsn, pattern = ".*.csv$", full.names = TRUE)
    return(files)
    
  } else {
    
    # List and return downloaded .gz files
    files <- list.files(dsn, pattern = ".*.op.gz$", full.names = TRUE)
    return(files)
  }
}