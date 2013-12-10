#' Julendat gap-filling based on linear modelling
gfComputeLinearModel <- function(data = NULL, 
                                 data.cc = NULL,
                                 data.dep,
                                 family = gaussian,
                                 pos.na,
                                 plots = NULL,
                                 n.plot = 10,
                                 prm.dep = "Ta_200",
                                 prm.indep = NA,
                                 time.window.pre, 
                                 time.window.pre.span,
                                 time.window.post,
                                 time.window.post.span,
                                 ...) {
  
  ################################################################################
  ##  
  ##  This program carries out multiple regression for both the dependent plot
  ##  under investigation and the previously identified independent plots. It 
  ##  imports the merged and uninterrupted data set of dependent and independent 
  ##  plots, the monthly data set of the dependent plot, and the names of the 
  ##  independent plots as well as the investigated NA position and parameter.
  ##  The output list contains  the date of the particular NA position, the 
  ##  investigated parameter, the formula calculated from the linear model, the 
  ##  predicted value and the station names being used for multiple regression.
  ##  
  ##  parameters are as follows:
  ##  
  ##  data (data.frame):      Merged monthly data sets.
  ##  data.cc (data.frame):   Complete cases of merged monthly data sets.
  ##  data.dep (ki.data):     Monthly data set of dependent plot.
  ##  family (character):     Family function to be used in the model.
  ##  pos.na (numeric):       Gap in data set of dependent plot including
  ##                          starting point, endpoint, and length of the gap.
  ##  plots (data.frame):     Data frame containing all plot names, information
  ##                          about data availability at pos.na, and distance from
  ##                          the dependent plot.
  ##  n.plot (numeric):       Number of independent plots for linear regression.
  ##  prm.dep (character):    Parameter under investigation.
  ##  prm.indep (character):  Single character object or Character vector with 
  ##                          independent parameters.
  ##  ...                     Further arguments to be passed
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
      "Module   :  gfComputeLinearModel", "\n",
      "Author   :  Florian Detsch <florian.detsch@geo.uni-marburg.de>, Tim Appelhans <tim.appelhans@gmail.com>",
      "Version  :  2013-01-08", "\n",
      "License  :  GNU GPLv3, see http://www.gnu.org/licenses/", "\n",
      "\n")
  
  ########## FUNCTION BODY #######################################################
  
  
  # In case of one plot with dependent and independent parameters
  if (is.null(data))
    data <- as.data.frame(cbind(data.dep@Datetime, unlist(data.dep@Parameter[prm.indep]), unlist(data.dep@Parameter[prm.dep])), row.names=FALSE)
  
  # Replicate data of dependent plot in case is.null(data.cc) == TRUE
  if (is.null(data.cc))
    data.cc <- data
  
  # Column names of dependent and independent plots and parameters
  coln.dep <- names(data.cc)[which(names(data.cc) == prm.dep)]
  coln.indep <- names(data.cc)[-c(1,which(names(data.cc) == prm.dep))]
  
  # Continue if there is at least one valid independent plot
  if (length(coln.indep) > 0) {
  
  # Formula for computation of linear model
  formula <- sapply(list(seq(coln.indep)), function(i) {
    paste(coln.dep, "~", paste(coln.indep[i], collapse=" + "))
  })
  
  # Linear model
  model <- glm(formula, data = data.cc, family = family)
  # Calculate r-squared
  r.squ <- cor(data.cc[,prm.dep], predict(model))^2 
  
  # Formula for imputation of missing value
  lm.formula <- sapply(list(2:length(model$coefficients)), function(i) {
    paste(model$coefficients[1], 
          paste(model$coefficients[i], " * data$", names(model$coefficients[i]), "[pos.na]", sep="", collapse=" + "), sep=" + ")
  })
  
  # Loop through single NA values 
  lm.fitted <- lapply(seq(time.window.pre.span + 1, time.window.pre.span + pos.na[3]), function(h) {
    # Fitted value at pos.na
    sum(unlist(sapply(list(2:length(model$coefficients)), function(i) {
      model$coefficients[i] * data[h,names(model$coefficients[i])]
    }))) + model$coefficients[1]
  })
  
  # Reassign n.plot in case number of valid plots < n.plot
  if (!is.null(plots) && sum(plots[,2]) < n.plot)
    n.plot <- sum(plots[,2])
  
  # Output
  return(lapply(seq(pos.na[1], pos.na[2]), function(i) {
    list(data.dep@Datetime[i],
         data.dep@PlotId$Unique,
         prm.dep,
         lm.fitted[[i-pos.na[1]+1]],
         lm.formula,
         r.squ,
         prm.indep,
         ifelse(!is.null(plots) && n.plot != 0, paste(plots[which(plots[,2])[1:n.plot],1], collapse=", "), NA))
  }))
  
  } else {
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
  }
}  
