#' GSOD to julendat conversion
#' 
#' @export gsod2ki
gsod2ki <- function(file.gsod,
                    comma.separated = T,
                    date.column = "YEARMODA",
                    date.start = NULL, 
                    date.end = NULL,
                    date.format = "%Y%m%d",
                    prm.column = "TEMP", 
                    temp2celsius = T, 
                    time.step = "day",
                    timezone = NA,
                    aggtime = "-999",
                    plot.id = NA,
                    ep.plot.id = "xxx",
                    station.id = NA,
                    proc.level = -999,
                    qual.flag = NA,
                    save.output = F,
                    df2ki = T,
                    ...) {
  
  # Required packages
  stopifnot(require(zoo))
  
  # Import data
  gsof <- read.table(file.gsod, header = T, stringsAsFactors = F, 
                     sep = ifelse(comma.separated, ",", ""))
  
  # Subset data by given start and end date (optional)
  index <- as.Date(paste0(substr(gsof[, date.column], 1, 4), "-01-01"))
  if (!is.null(date.start))
    gsof <- gsof[index >= date.start, ]
  if (!is.null(date.end))
    gsof <- gsof[index <= date.end, ]

  # Convert date column to Date format
  gsof.initial.time <- gsof[, date.column]
  
  # Convert temperature columns from Fahrenheit to Celsius (optional)
  if (temp2celsius) {
    for (i in prm.column) {
      if (is.character(gsof[, i]))
        gsof[, i] <- as.numeric(substr(gsof[, i], 1, nchar(gsof[, i])-1))

      gsof[, i] <- round((gsof[, i] - 32.0) / 1.8, digits = 1)
      gsof[which(gsof[, i] > 100), i] <- NA
    }
  }
  
  # Convert parameter column into continuous Date object
  st <- as.Date(paste0(substr(gsof[1, date.column], 1, 4), "-01-01"))
  nd <- as.Date(paste0(substr(gsof[nrow(gsof), date.column], 1, 4), "-12-31"))
  
  # Subset GSOD data by relevant columns
  gsof.ts.prm <- gsof[, c(date.column, prm.column)]
  gsof.ts.prm[, date.column] <- as.Date(as.character(gsof.ts.prm[, date.column]), 
                                        format = date.format)
  
  # Merge continuous Date object with available GSOD measurements
  gsof.ts <- merge(data.frame(seq(st, nd, 1)), gsof.ts.prm, by = 1, all.x = T)
  names(gsof.ts) <- names(gsof.ts.prm)
  
  # Merge generated data
  gsof.ts <- data.frame(Datetime = if (time.step == "day") {
                          paste(gsof.ts[, date.column], "12:00:00") 
                        } else {
                          index(gsof.ts)
                        },
                        Timezone = rep(timezone, nrow(gsof.ts)),
                        Aggregationtime = rep(aggtime, nrow(gsof.ts)),
                        PlotId = rep(plot.id, nrow(gsof.ts)),
                        EpPlotId = rep(ep.plot.id, nrow(gsof.ts)),
                        StationId = rep(station.id, nrow(gsof.ts)),
                        Processlevel = rep(proc.level, nrow(gsof.ts)),
                        Qualityflag = rep(qual.flag, nrow(gsof.ts)),
                        gsof.ts[, prm.column], 
                        stringsAsFactors = F)
  
  # Save (optional) and return data
  if (save.output)
    write.csv(gsof.ts, ...)
  
  if (df2ki)
    gsof.ts <- as.ki.data(gsof.ts)
    
  return(gsof.ts)
}

# # Call
# tmp <- gfData2Ts(file = "E:/kilimanjaro/temperature_network/data/noaa_hourly_global_airport_1973_2013.csv", 
#                  header = TRUE, sep = ",", na.strings = c("*", "**", "***", "****", "*****", "******"), 
#                  date.column = "YR..MODAHRMN", 
#                  prm.column = "TEMP", 
#                  temp2celsius = TRUE, 
#                  time.step = "hour")
# head(tmp)