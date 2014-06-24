#' Function to plot gap-filled GSOD data sets
#' 
#' @export gsodPlot
gsodPlot <- function(fls.orig = NULL,
                     fls.gf, 
                     stations, 
                     prm.orig = "TEMP",
                     type = "filled",
                     ...) {
  
  # Required packages
  libs <- c("zoo", "doParallel", "Hmisc", "bfast", "ggplot2", "scales", 
            "plyr", "reshape2")
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
      data.frame(DATE = time(i), PLOT = j, Original = as.numeric(i[, prm.orig]))
  }
  
  # Gap-filled GSOD data
  if (!is.null(fls.gf)) {
    ta.gf <- lapply(fls.gf, function(i) {
      ta.gf.ts <- read.zoo(i, format = "%Y-%m-%d %H:%M:%S", 
                           header = TRUE, sep = ",", regular = TRUE)
      return(ta.gf.ts)
    })
    
    
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
    if (type == "original") {
      
      #     # Reformat and append gap-filled station data
      #     ta.gf.df <- foreach(i = ta.gf, j = stations, .combine = "rbind") %do%
      #       data.frame(DATE = time(i), PLOT = j, Imputed = as.numeric(i[, prm.orig]))
      
#       # Merge and melt original and gap-filled data
#       ta.orig.gf.df <- merge(ta.orig.df, ta.gf.df, all = TRUE, by = c(1, 2))
#       ta.orig.gf.df.mlt <- melt(ta.orig.gf.df, id.vars = c(1, 2))
#       
#       ta.orig.gf.df.mlt$variable <- factor(ta.orig.gf.df.mlt$variable, 
#                                            levels = c("Imputed", "Original"))
      
      ggplot(aes(x = DATE, y = Original), data = ta.orig.df) + 
        geom_line() + 
        facet_wrap(~ PLOT, ncol = 2) + 
        scale_x_date(limits = c(as.Date("1973-01-01"), as.Date("2013-12-31")), 
                     breaks = seq(as.Date("1970-01-01"), as.Date("2020-01-01"), "10 years"), 
                     labels = date_format("%Y"), minor_breaks = date_breaks("2 years")) + 
        #       scale_colour_manual("", values = c("grey65", "black"), 
        #                           labels = c("Original data", "Imputed data"), 
        #                           breaks = c("Original", "Imputed")) +
        labs(list(title = "GSOD daily air temperature (1973-2012)\n", 
                  x = "\nTime [d]", y = "Temperature [°C]\n")) + 
        theme_bw() + 
        theme(text = element_text(...), 
              legend.key = element_rect(fill = "transparent"), 
              panel.grid.major = element_line(size = 1.2), 
              panel.grid.minor = element_line(size = 1.1))
      
    } else if (type == "both") {
      # Reformat and append gap-filled station data
      ta.gf.df <- foreach(i = ta.gf, j = stations, .combine = "rbind") %do%
        data.frame(DATE = time(i), PLOT = j, Imputed = as.numeric(i[, prm.orig]))
      
      # Merge and meltoriginal and gap-filled data
      ta.orig.gf.df <- merge(ta.orig.df, ta.gf.df, all = TRUE, by = c(1, 2))
      ta.orig.gf.df.mlt <- melt(ta.orig.gf.df, id.vars = c(1, 2))
      
      ta.orig.gf.df.mlt$variable <- factor(ta.orig.gf.df.mlt$variable, 
                                           levels = c("Imputed", "Original"))
      
      ggplot(aes(x = DATE, y = value, group = variable, colour = variable), 
             data = ta.orig.gf.df.mlt) + 
        geom_line() + 
        facet_wrap(~ PLOT, ncol = 2) + 
        scale_x_date(limits = c(as.Date("1973-01-01"), as.Date("2013-12-31")), 
                     breaks = seq(as.Date("1970-01-01"), as.Date("2020-01-01"), "10 years"), 
                     labels = date_format("%Y"), minor_breaks = date_breaks("2 years")) + 
        scale_colour_manual("", values = c("grey65", "black"), 
                            labels = c("Original data", "Imputed data"), 
                            breaks = c("Original", "Imputed")) +
        labs(list(title = "GSOD daily air temperature (1973-2012)\n", 
                  x = "\nTime [d]", y = "Temperature [°C]\n")) + 
        theme_bw() + 
        theme(text = element_text(...), 
              legend.key = element_rect(fill = "transparent"), 
              panel.grid.major = element_line(size = 1.2), 
              panel.grid.minor = element_line(size = 1.1))
      
    } else if (type == "all.in.one") {
      # Reformat and append gap-filled station data
      ta.gf.df <- foreach(i = ta.gf, j = stations, .combine = "rbind") %do%
        data.frame(DATE = time(i), PLOT = j, Imputed = as.numeric(i[, prm.orig]))
      
      # Merge and meltoriginal and gap-filled data
      ta.orig.gf.df <- merge(ta.orig.df, ta.gf.df, all = TRUE, by = c(1, 2))
      ta.orig.gf.df.mlt <- melt(ta.orig.gf.df, id.vars = c(1, 2))
      
      ta.orig.gf.df.mlt$variable <- factor(ta.orig.gf.df.mlt$variable, 
                                           levels = c("Imputed", "Original"))
      
      ggplot(aes(x = DATE, y = value, group = variable, colour = variable), 
             data = ta.orig.gf.df.mlt) + 
        geom_line() + 
        facet_wrap(~ PLOT, ncol = 2) + 
        scale_x_date(limits = c(as.Date("1973-01-01"), as.Date("2013-12-31")), 
                     breaks = seq(as.Date("1970-01-01"), as.Date("2020-01-01"), "10 years"), 
                     labels = date_format("%Y"), minor_breaks = date_breaks("2 years")) + 
        scale_colour_manual("", values = c("grey65", "black"), 
                            labels = c("Original data", "Imputed data"), 
                            breaks = c("Original", "Imputed")) +
        labs(list(title = "GSOD daily air temperature (1973-2012)\n", 
                  x = "\nTime [d]", y = "Temperature [°C]\n")) + 
        theme_bw() + 
        theme(text = element_text(...), 
              legend.key = element_rect(fill = "transparent"), 
              panel.grid.major = element_line(size = 1.2), 
              panel.grid.minor = element_line(size = 1.1))
      
      # Plot gap-filled GSOD data only  
    } else {  
      # Reformat and append gap-filled data
      ta.gf.df <- foreach(i = ta.gf, j = stations, .combine = "rbind") %do%
        data.frame(DATE = time(i), PLOT = j, MEAN = as.numeric(i$TEMP), 
                   MAX = as.numeric(i$MAX), MIN = as.numeric(i$MIN))
      
      # Melt gap-filled data
      ta.gf.df <- melt(ta.gf.df, id.vars = c(1, 2))
      # Reorder factor levels
      ta.gf.df$variable <- factor(ta.gf.df$variable, levels = c("MIN", "MEAN", "MAX"))
      
      #     ggplot() + 
      #       geom_line(aes(x = DATE, y = TEMP, colour = "grey50"), data = ta.gf.df) + 
      #       facet_wrap(~ PLOT, ncol = 2) + 
      #       stat_smooth(aes(x = DATE, y = TEMP, colour = "black"), linetype = "solid",
      #                   data = ta.gf.df, method = "lm", size = 1.5, se = FALSE) + 
      #       stat_smooth(aes(x = DATE, y = MIN, colour = "blue"), linetype = "dotted", 
      #                   data = ta.gf.df, method = "lm", size = 1.2, se = FALSE) + 
      #       stat_smooth(aes(x = DATE, y = MAX, colour = "red"), linetype = "dotted", 
      #                   data = ta.gf.df, method = "lm", size = 1.2, se = FALSE) + 
      #       scale_x_date(limits = c(as.Date("1973-01-01"), as.Date("2013-12-31")), 
      #                    breaks = seq(as.Date("1970-01-01"), as.Date("2020-01-01"), "10 years"), 
      #                    labels = date_format("%Y"), minor_breaks = date_breaks("2 years")) + 
      #       scale_colour_manual("GSOD daily \nair temperature", 
      #                           values = c("black" = "black", "grey50" = "grey50", 
      #                                      "blue" = "blue", "red" = "red"), 
      #                           breaks = c("grey50", "black", "blue", "red"),
      #                           labels = c("Mean", "Mean (linear trend)", 
      #                                      "Minimum (linear trend)", 
      #                                      "Maximum (linear trend)")) +
      #       scale_linetype_manual(values = c(rep("solid", 2), rep("dotted", 2))) +
      #       labs(list(x = "Time [d]", y = "Temperature [°C]")) + 
      #       theme_bw() + 
      #       theme(text = element_text(size = 25), 
      #             legend.key = element_rect(fill = "transparent"), 
      #             panel.grid.major = element_line(size = 1.2), 
      #             panel.grid.minor = element_line(size = 1.1))
      ggplot(aes(x = DATE, y = value, colour = variable, linetype = variable), 
             data = ta.gf.df) + 
        geom_line(,subset = .(variable == "MEAN"), colour = "grey65") +
        stat_smooth(size = 1.2, method = "lm", se = FALSE) + 
        facet_wrap(~ PLOT, ncol = 2) + 
        scale_x_date(limits = c(as.Date("1973-01-01"), as.Date("2013-12-31")), 
                     breaks = seq(as.Date("1970-01-01"), as.Date("2020-01-01"), "10 years"), 
                     labels = date_format("%Y"), minor_breaks = date_breaks("2 years")) + 
        scale_linetype_manual("Linear trends of daily", 
                              values = c("dotted", "solid", "dotted"), 
                              labels = c("Minimum", 
                                         "Mean",                                      
                                         "Maximum")) +
        scale_colour_manual("Linear trends of daily", 
                            values = c("blue", "black", "red"), 
                            labels = c("Minimum", 
                                       "Mean",                                      
                                       "Maximum")) +
        labs(colour = "Linear trends of daily", 
             linetype = "Linear trends of daily") + 
        labs(list(title = "GSOD daily air temperature (1973-2012)\n", 
                  x = "\nTime [d]", y = "Temperature [°C]\n")) + 
        theme_bw() + 
        theme(text = element_text(...), 
              legend.key = element_rect(fill = "transparent"), 
              panel.grid.major = element_line(size = 1.2), 
              panel.grid.minor = element_line(size = 1.1))
    }
  }
}