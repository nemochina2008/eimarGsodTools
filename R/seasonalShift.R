#' Function to plot seasonal shifts in monthly averaged GSOD temperature data
#' 
#' @export seasonalShift
seasonalShift <- function(fls, 
                          start = c("1973-01-01", "1977-12-31"),
                          end = c("2008-01-01", "2012-12-31"), 
                          stations,
                          ncol = 2,
                          palette = c("cornflowerblue", "red2", "grey65", "grey35"), 
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
    
    Reduce(function(...) merge(..., by = c(1, 2)), 
           foreach(k = start, l = end) %do% {
      tmp <- read.csv(i, stringsAsFactors = FALSE)
      tmp$year <- as.Date(substr(tmp$Datetime, 1, 10))
      tmp$yearmon <- as.yearmon(tmp$year)
      
      tmp.st <- subset(tmp, year >= as.Date(k[1]) & year <= as.Date(k[2]))
      tmp.st.agg <- aggregate(tmp.st[, c("TEMP", "MAX", "MIN")], 
                                     by = list(tmp.st$yearmon), FUN = mean)
      
      tmp.nd <- subset(tmp, year >= as.Date(l[1]) & year <= as.Date(l[2]))
      tmp.nd.agg <- aggregate(tmp.nd[, c("TEMP", "MAX", "MIN")], 
                                     by = list(tmp.nd$yearmon), FUN = mean)
      
      tst.st <- vectorHarmonics(tmp.st.agg$TEMP, frq = 12, fun = mean, m = 2,
                                st = c(as.numeric(substr(k[1], 1, 4)), 01), 
                                nd = c(as.numeric(substr(k[2], 1, 4)), 12))
      tst.nd <- vectorHarmonics(tmp.nd.agg$TEMP, frq = 12, fun = mean, m = 2,
                                st = c(as.numeric(substr(l[1], 1, 4)), 01), 
                                nd = c(as.numeric(substr(l[2], 1, 4)), 12))
      
      tmp.df <- data.frame("station" = j, 
                           "month" = month.abb, 
                           "st" = tst.st, 
                           "nd" = tst.nd)
      
      index <- c(grep("^st$", names(tmp.df)), grep("^nd$", names(tmp.df)))
      names(tmp.df)[index] <- 
        c(paste(substr(k, 1, 4), collapse = "-"), 
          paste(substr(l, 1, 4), collapse = "-"))
      
      return(tmp.df)
    })
    
  }
  
  # Reformat data
  tmp.all <- melt(tmp.all, id.vars = 1:2)
  # Reorder factor levels of 'month' column
  tmp.all$month <- factor(tmp.all$month, levels = month.abb)
  
  
  ## ggplot

  if (is.list(start) & is.list(end)) {
    
    label.st.1 <- paste(substr(start[[1]], 1, 4), collapse = "-")
    label.st.2 <- paste(substr(start[[2]], 1, 4), collapse = "-")
    
    label.nd.1 <- paste(substr(end[[1]], 1, 4), collapse = "-")
    label.nd.2 <- paste(substr(end[[2]], 1, 4), collapse = "-")
    
    cols <- palette
    names(cols) <- c(label.st.1, label.nd.1, label.st.2, label.nd.2)
    
    ggplot(aes(x = month, y = value, group = variable, colour = variable), 
           data = tmp.all) + 
      geom_line(lwd = 1) + 
      facet_wrap(~ station, ncol = ncol, scales = "free_y") + 
      scale_colour_manual("", 
                          values = cols, 
                          breaks = c(label.st.2, label.st.1, label.nd.1, label.nd.2)) + 
      labs(list(x = "\nMonth", y = "Temperature [°C]\n")) + 
      theme_bw() + 
      theme(text = element_text(...), 
            legend.key = element_rect(fill = "transparent"), 
            panel.grid.major = element_line(size = 1.2), 
            panel.grid.minor = element_line(size = 1.1))
  } else {

    label.st <- paste(substr(start, 1, 4), collapse = "-")
    label.nd <- paste(substr(end, 1, 4), collapse = "-")
    
    ggplot(aes(x = month, y = value, colour = variable, group = variable), 
           data = tmp.all) + 
      geom_line(lwd = 1) + 
      facet_wrap(~ station, ncol = ncol) + 
      scale_colour_manual("", values = c("cornflowerblue", "red2"), 
                          labels = c(label.st.1, label.nd.1)) + 
      labs(list(x = "\nMonth", y = "Temperature [°C]\n")) + 
      theme_bw() + 
      theme(text = element_text(...), 
            legend.key = element_rect(fill = "transparent"), 
            panel.grid.major = element_line(size = 1.2), 
            panel.grid.minor = element_line(size = 1.1))
  
  }
  
}
