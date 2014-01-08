#' Function to plot gap-filled GSOD data sets
#' 
#' @export gsodPlot
gsodPlot <- function(fls.orig = NULL,
                     fls.gf, 
                     stations, 
                     plot.orig = FALSE, 
                     ...) {
  
  # Required packages
  libs <- c("zoo", "doParallel", "Hmisc", "bfast", "ggplot2", "scales")
  lapply(libs, function(i) require(i, character.only = TRUE))
  
  
  ## Data import
  
  # Initial GSOD data
  if (!is.null(fls.orig)) {
    ta.orig <- lapply(fls.orig, function(i) {
      tmp.ts <- read.zoo(i, format = "%Y-%m-%d %H:%M:%S", 
                         header = TRUE, sep = ",", regular = TRUE)
      frequency(tmp.ts) <- 365
      return(tmp.ts)
    })
    
    # Reformat and append data
    ta.orig.df <- foreach(i = ta.orig, j = stations, .combine = "rbind") %do%
      data.frame(DATE = time(i), PLOT = j, TEMP = as.numeric(i$TEMP), 
                 MAX = as.numeric(i$MAX), MIN = as.numeric(i$MIN))
  }
  
  # Gap-filled GSOD data
  ta.gf <- lapply(fls.gf, function(i) {
    ta.gf.ts <- read.zoo(i, format = "%Y-%m-%d %H:%M:%S", 
                         header = TRUE, sep = ",", regular = TRUE)
    return(ta.gf.ts)
  })
  
  # Reformat and append data
  ta.gf.df <- foreach(i = ta.gf, j = stations, .combine = "rbind") %do%
    data.frame(DATE = time(i), PLOT = j, TEMP = as.numeric(i$TEMP), 
               MAX = as.numeric(i$MAX), MIN = as.numeric(i$MIN))
  
  
  
  # ## Breakpoint detection
  # 
  # # Loop through each time series
  # tmp.bfast <- foreach(i = tmp, .packages = lib) %dopar% {
  #   tmp.ts <- ts(na.exclude(as.numeric(i[, 8])), 
  #                start = 1973, end = 2013, frequency = 365)
  #   bfast(tmp.ts, season = "harmonic", max.iter = 1, breaks = 5)
  # }
  
  
  ## Plotting stuff
    
  # Plot original and gap-filled GSOD data  
  if (plot.orig) {
    ggplot() + 
      geom_line(aes(x = DATE, y = TEMP, colour = "grey50"), data = ta.gf.df) + 
      facet_wrap(~ PLOT, ncol = 2) + 
      geom_line(aes(x = DATE, y = TEMP, colour = "black"), data = ta.orig.df) + 
      scale_x_date(limits = c(as.Date("1973-01-01"), as.Date("2013-12-31")), 
                   breaks = seq(as.Date("1970-01-01"), as.Date("2020-01-01"), "10 years"), 
                   labels = date_format("%Y"), minor_breaks = date_breaks("2 years")) + 
      scale_colour_manual("", values = c("black", "grey50"), 
                          labels = c("Original data", "Gap-filled data")) +
      labs(list(x = "Time [d]", y = "Temperature [°C]")) +
      theme_bw() + 
      theme(text = element_text(size = 25), 
            legend.key = element_rect(fill = "transparent"), 
            panel.grid.major = element_line(size = 1.2), 
            panel.grid.minor = element_line(size = 1.1))
    
  # Plot gap-filled GSOD data only  
  } else {  
    ggplot() + 
      geom_line(aes(x = DATE, y = TEMP, colour = "grey50"), data = ta.gf.df) + 
      facet_wrap(~ PLOT, ncol = 2) + 
      stat_smooth(aes(x = DATE, y = TEMP, colour = "black"), data = ta.gf.df, 
                  method = "lm", size = 1.5, linetype = "dashed", se = FALSE) + 
      stat_smooth(aes(x = DATE, y = MIN, colour = "blue"), data = ta.gf.df, 
                  method = "lm", size = 1.2, linetype = "dashed", se = FALSE) + 
      stat_smooth(aes(x = DATE, y = MAX, colour = "red"), data = ta.gf.df, 
                  method = "lm", size = 1.2, linetype = "dashed", se = FALSE) + 
      scale_x_date(limits = c(as.Date("1973-01-01"), as.Date("2013-12-31")), 
                   breaks = seq(as.Date("1970-01-01"), as.Date("2020-01-01"), "10 years"), 
                   labels = date_format("%Y"), minor_breaks = date_breaks("2 years")) + 
      scale_colour_manual("GSOD air temperature", 
                          values = c("black" = "black", "grey50" = "grey50", 
                                     "blue" = "blue", "red" = "red"), 
                          breaks = c("grey50", "black", "blue", "red"),
                          labels = c("Mean", "Mean (linear trend)", 
                                     "Minimum (linear trend)", 
                                     "Maximum (linear trend")) +
      labs(list(x = "Time [d]", y = "Temperature [°C]")) + 
      theme_bw() + 
      theme(text = element_text(size = 25), 
            legend.key = element_rect(fill = "transparent"), 
            panel.grid.major = element_line(size = 1.2), 
            panel.grid.minor = element_line(size = 1.1))
  }
}