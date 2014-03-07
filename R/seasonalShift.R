#' Function to plot seasonal shifts in monthly averaged GSOD temperature data
#' 
#' @export seasonalShift
seasonalShift <- function(fls, 
                          start = c("1973-01-01", "1977-12-31"),
                          end = c("2008-01-01", "2012-12-31"), 
                          stations,
                          ...) {
  
  # Required packages
  lib <- c("foreach", "zoo", "reshape2", "ggplot2")
  sapply(lib, function(x) stopifnot(require(x, character.only = TRUE)))

  if ("Rsenal" %in% rownames(installed.packages())) {
    library(Rsenal)
  } else {
    library(devtools)
    install_github("Rsenal", "environmentalinformatics-marburg")
    library(Rsenal)
  }
  
  # Import data
  tmp.all <- foreach(i = fls, j = stations, .combine = "rbind") %do% {
    
    tmp <- read.csv(i, stringsAsFactors = FALSE)
    tmp$year <- as.Date(substr(tmp$Datetime, 1, 10))
    tmp$yearmon <- as.yearmon(tmp$year)
    
    tmp.st <- subset(tmp, year >= as.Date(start[1]) & year <= as.Date(start[2]))
    tmp.st.agg <- aggregate(tmp.st[, c("TEMP", "MAX", "MIN")], 
                                   by = list(tmp.st$yearmon), FUN = mean)
    
    tmp.nd <- subset(tmp, year >= as.Date(end[1]) & year <= as.Date(end[2]))
    tmp.nd.agg <- aggregate(tmp.nd[, c("TEMP", "MAX", "MIN")], 
                                   by = list(tmp.nd$yearmon), FUN = mean)
    
    tst.nd <- vectorHarmonics(tmp.nd.agg$TEMP, frq = 12, fun = mean, m = 2,
                              st = c(as.numeric(substr(start[1], 1, 4)), 01), 
                              nd = c(as.numeric(substr(start[2], 1, 4)), 12))
    tst.st <- vectorHarmonics(tmp.st.agg$TEMP, frq = 12, fun = mean, m = 2,
                              st = c(as.numeric(substr(end[1], 1, 4)), 01), 
                              nd = c(as.numeric(substr(end[2], 1, 4)), 12))
    
    return(data.frame("station" = j, "month" = month.abb, 
                      "st" = tst.st, "nd" = tst.nd))  
    
  }
  
  # Reformat data
  tmp.all <- melt(tmp.all)
  # Reorder factor levels of 'month' column
  tmp.all$month <- factor(tmp.all$month, levels = month.abb)
  
  
  ## ggplot
  
  ggplot(aes(x = month, y = value, colour = variable, group = variable), 
         data = tmp.all) + 
    geom_line(lwd = 1) + 
    facet_wrap(~ station, ncol = 2) + 
    scale_colour_manual("", values = c("cornflowerblue", "red2"), 
                        labels = c("1973-1977", "2008-2012")) + 
    labs(list(x = "\nMonth", y = "Temperature [°C]\n")) + 
    theme_bw() + 
    theme(text = element_text(...), 
          legend.key = element_rect(fill = "transparent"), 
          panel.grid.major = element_line(size = 1.2), 
          panel.grid.minor = element_line(size = 1.1))
  
}
