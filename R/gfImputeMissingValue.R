#' Julendat wrapper function for imputation of missing values
gfImputeMissingValue <- function(data.dep, 
                                 data.indep,
                                 na.limit = 0.2,
                                 time.window, 
                                 pos.na, 
                                 data.coords,
                                 n.plot = 10, 
                                 prm.dep = "Ta_200",
                                 prm.indep = NA,
                                 family = gaussian,
                                 ...) {
  
  ################################################################################
  ##  
  ##  This program is designed to call a number of sub-functions in order to fit
  ##  a linear model for a given NA position and return a fitted value. 
  ##  
  ##  parameters are as follows:
  ##  
  ##  data.dep (ki.data):         Monthly data set of dependent plot.
  ##  data.indep (list):          List of monthly data sets of independent plots.
  ##                              Must be composed of ki.data objects.
  ##  na.limit (numeric):         Accepted threshold percentage of NA values in monthly 
  ##                              data set of independent plots.
  ##  time.window (numeric):      Values to consider for fitting the model before and 
  ##                              after the gap.
  ##  pos.na (numeric):           Gap in data set of dependent plot including
  ##                              starting point, endpoint, and length of the gap.
  ##  data.coords (data.frame):   Plot coordinates.
  ##  n.plot (numeric):           Number of independent plots for linear regression.
  ##  prm.dep (character):        Parameter under investigation.
  ##  prm.indep (character):      Single character object or character vector with 
  ##                              independent parameters.
  ##  family (family):            Description of the error distribution and link 
  ##                              function to be used in the model
  ##  ...                         Further arguments to be passed
  ##
  ################################################################################
  ##
  ##  Copyright (C) 2012 Florian Detsch, Tim Appelhans
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
      "Module   :  gfImputeMissingValue", "\n",
      "Author   :  Florian Detsch <florian.detsch@geo.uni-marburg.de>, Tim Appelhans <tim.appelhans@gmail.com>",
      "Version  :  2012-12-18", "\n",
      "License  :  GNU GPLv3, see http://www.gnu.org/licenses/", "\n",
      "\n")
  
  ########## FUNCTION BODY #######################################################
  
  
  ## Status message
  
  print(paste("Current gap:", paste(pos.na[1:2], collapse = " to ")))
  
  ## Required libraries
  
  library(gmt) # function 'geodist'
  
  
  ## Revision of defined time window with respect to the current gap
  
  # Time span before ...
  time.window.pre <- pos.na[1] - time.window
  time.window.pre <- ifelse(time.window.pre > 0, time.window.pre, 1)
  time.window.pre.span <- pos.na[1] - time.window.pre
  # ... and after gap
  time.window.post <- pos.na[2] + time.window
  time.window.post <- ifelse(time.window.post <= length(data.dep@Parameter[[prm.dep]]), 
                             time.window.post, 
                             length(data.dep@Parameter[[prm.dep]]))
  time.window.post.span <- time.window.post - pos.na[2]
  
  
  ## Reject independent data sets with a too high amount of NA values
  
  # Load function 'gfValidNaThreshold'
  source("gfValidNaThreshold.R")
  # Reject plots with too much NA values
  data.indep <- gfValidNaThreshold(data.indep = data.indep,
                                   prm.dep = prm.dep, 
                                   na.limit = na.limit, 
                                   time.window.pre = time.window.pre,
                                   time.window.post = time.window.post,
                                   pos.na = pos.na)
  
  
  ## Refuse gap filling of current gap in case there are no valid independent plots
  
  # Check if there is at least one adjacent plot
  if (length(data.indep) == 0) {
    
    # Error message and breakup of current iteration
    print(paste("Filling of gap from", pos.na[1], "to", pos.na[2], "not possible."))
    return(lapply(seq(pos.na[1], pos.na[2]), function(i) {
      list(data.dep@Datetime[i],
           data.dep@PlotId$Unique,
           prm.dep,
           NA,
           NA,
           NA,
           prm.indep,
           NA)
    }))
    
    # If there is at least one valid adjacent plot  
  } else {
    
    
    ## Get plots with valid measurements at the given NA position
    
    # Load function 'gfNonNaStations'
    source("gfNonNaStations.R")
    # Identify plots
    data.indep.avl <- gfNonNaStations(data.indep = data.indep, 
                                      pos.na = pos.na, 
                                      prm.dep = prm.dep)
    
    # Check if there is at least one plot with valid measurement
    if (sum(data.indep.avl[,2]) == 0) {
      
      # Error message and breakup of current iteration
      print(paste("Filling of gap from", pos.na[1], "to", pos.na[2], "not possible."))
      return(lapply(seq(pos.na[1], pos.na[2]), function(i) {
        list(data.dep@Datetime[i],
             data.dep@PlotId$Unique,
             prm.dep,
             NA,
             NA,
             NA,
             prm.indep,
             NA)
      }))
      
    } else {
      
      ## Calculate distance between independent plots and dependent plot if desired
      if (!is.null(data.coords)) {
        data.indep.avl[,3] <- unlist(lapply(seq(data.indep.avl[,1]), function(k) {
          geodist(Nfrom=data.coords[which(data.coords[,1] == data.dep@PlotId$Unique),"Lat"], 
                  Efrom=data.coords[which(data.coords[,1] == data.dep@PlotId$Unique),"Lon"], 
                  Nto=data.coords[which(data.coords[,1] == data.indep.avl[k,1]), "Lat"], 
                  Eto=data.coords[which(data.coords[,1] == data.indep.avl[k,1]), "Lon"])
        }))
        
        # Order independent stations by distance from the dependent plot
        data.indep.avl <- data.indep.avl[order(data.indep.avl[,3]),]
        data.indep <- data.indep[as.numeric(row.names(data.indep.avl))]
      }
      
      
      ## Merge monthly data sets to obtain complete cases of dependent and independent plots
      
      # Load function 'gfCompleteMonthlyCases'
      source("gfCompleteMonthlyCases.R")
      # Get complete cases
      data.prm.cc <- gfCompleteMonthlyCases(data.dep = data.dep, 
                                            data.indep = data.indep, 
                                            data.indep.avl = data.indep.avl, 
                                            n.plot = n.plot, 
                                            prm.dep = prm.dep,
                                            time.window.pre = time.window.pre, 
                                            time.window.post = time.window.post, 
                                            pos.na = pos.na,
                                            prm.indep = prm.indep)
      
      
      ## Fit generalized linear model 
      
      # Check if there are any complete cases
      if (nrow(data.prm.cc[[2]]) == 0) {
        
        # Error message and breakup of current iteration
        print(paste("Filling of gap from", pos.na[1], "to", pos.na[2], "not possible."))
        return(lapply(seq(pos.na[1], pos.na[2]), function(i) {
          list(data.dep@Datetime[i],
               data.dep@PlotId$Unique,
               prm.dep,
               NA,
               NA,
               NA,
               prm.indep,
               NA)
        }))
        
      } else {
        
        # Load function 'gfComputeLinearModel'
        source("gfComputeLinearModel.R")
        # Fit linear Model
        data.prm.cc.lm <- gfComputeLinearModel(data = data.prm.cc[[1]], 
                                               data.cc = data.prm.cc[[2]], 
                                               data.dep = data.dep, 
                                               family = family, 
                                               pos.na = pos.na, 
                                               plots = data.indep.avl,
                                               n.plot = n.plot, 
                                               prm.dep = prm.dep, 
                                               prm.indep = prm.indep, 
                                               time.window.pre = time.window.pre, 
                                               time.window.pre.span = time.window.pre.span, 
                                               time.window.post = time.window.post,
                                               time.window.post.span = time.window.post.span)
        
        
        
        ## Return output list
        
        return(data.prm.cc.lm)
      }
    }
  }
}
