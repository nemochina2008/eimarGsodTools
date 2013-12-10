#' Julendat function to assure measurement quality
gfRejectLowQuality <- function(data, 
                               prm.dep = "Ta_200",
                               quality.levels, 
                               ...) {
  
  ################################################################################
  ##  
  ##  This program takes ki.data objects from dependent or independent plots as
  ##  input and rejects those measurements of the specified parameter that are
  ##  characterized by a bad quality flag.
  ##  
  ##  parameters are as follows:
  ##  
  ##  data (ki.data):           Monthly data set of dependent or independent plots.
  ##                            Either a single ki.data object or a list of ki.data
  ##                            objects.
  ##  prm.dep (character):      Parameter under investigation.
  ##  quality.levels (numeric): Numeric value(s) of rejected flags.
  ##  ...                       Further arguments to be passed
  ##
  ################################################################################
  ##
  ##  Copyright (C) 2013 Florian Detsch, Tim Appelhans
  ##
  ##  This program is free software: you can redistribute it and/or modify
  ##  it under the terms of the GNU General Public License as published by
  ##  the Free Software Foundation, either version 3 of the License, or
  ##  (at your option) any later version.
  ##
  ##  This program is distributed in the hope that it will be useful,
  ##  but WITHOUT ANY WARRANTY; without even the implied warranty of
  ##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
  ##  GNU General Public License for more details.
  ##
  ##  You should have received a copy of the GNU General Public License
  ##  along with this program.  If not, see <http://www.gnu.org/licenses/>.
  ##
  ##  Please send any comments, suggestions, criticism, or (for our sake) bug
  ##  reports to florian.detsch@geo.uni-marburg.de
  ##
  ################################################################################
  
  cat("\n",
      "Module   :  gfRejectLowQuality.R", "\n",
      "Author   :  Florian Detsch <florian.detsch@geo.uni-marburg.de>, Tim Appelhans <tim.appelhans@gmail.com>",
      "Version  :  2013-01-09", "\n",
      "License  :  GNU GPLv3, see http://www.gnu.org/licenses/", "\n",
      "\n")
  
  ########## FUNCTION BODY #######################################################
  
  # Column with specified parameter in ki.data slot @Parameter
  prm.dep.col <- ifelse(!is.list(data), 
                        which(names(data@Parameter) == prm.dep), 
                        which(names(data[[1]]@Parameter) == prm.dep))

  # Limits for quality flag extraction
  if (prm.dep.col == 1) {
    substr.min <- 1 + prm.dep.col + (prm.dep.col - 1) * 2
    substr.max <- substr.min + 2
  } else {  
  # substr.min <- ifelse(!is.list(data), 
  #                     2 + length(unique(substr(names(data@Parameter[c(1:(prm.dep.col-1))]), 1, 3))) * 3,
  #                     2 + length(unique(substr(names(data[[1]]@Parameter[c(1:(prm.dep.col-1))]), 1, 2))) * 3)
  substr.min <- prm.dep.col * 3 - 1
  substr.max <- substr.min + 2
  }
  
  if(!is.list(data)) {
    # Identify measurements that yield a bad quality flag
    pos.na.qc <- unique(c(which(is.na(data@Parameter[[prm.dep[1]]])), which(as.numeric(substr(data@Qualityflag, substr.min, substr.max)) %in% quality.levels)))
    # Replace invalid measurements by NA
    data@Parameter[[prm.dep]][pos.na.qc] <- NaN
    # Update quality flag
    data@Qualityflag[pos.na.qc] <- paste(substr(data@Qualityflag[pos.na.qc], 1, substr.min - 1), 
                                         as.character(as.numeric(substr(data@Qualityflag[pos.na.qc], substr.min, substr.max)) + 200), 
                                         substr(data@Qualityflag[pos.na.qc], substr.max + 1, nchar(data@Qualityflag[pos.na.qc])), sep = "")
    data@Qualityflag[-pos.na.qc] <- paste(substr(data@Qualityflag[-pos.na.qc], 1, substr.min - 1), 
                                         as.character(as.numeric(substr(data@Qualityflag[-pos.na.qc], substr.min, substr.max)) + 100), 
                                         substr(data@Qualityflag[-pos.na.qc], substr.max + 1, nchar(data@Qualityflag[-pos.na.qc])), sep = "")
    
  } else {
    lapply(seq(data), function(i) {
      pos.na.qc <- unique(c(data[[i]]@Valid$NAIndex, which(as.numeric(substr(data[[i]]@Qualityflag, substr.min, substr.max)) %in% quality.levels)))
      data[[i]]@Parameter[[prm.dep]][pos.na.qc] <- NaN
    })
  }
  
  # Output
  return(data)
}
