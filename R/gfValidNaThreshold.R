#' Julendat function to calculate the amount of missing values
gfValidNaThreshold <- function(data.indep,
                               prm.dep = "Ta_200",
                               na.limit = 0.2, 
                               time.window.pre,
                               time.window.post,
                               pos.na,
                               ...) {
  
  ################################################################################
  ##  
  ##  This program computes the percentage amount of NA values in the independent 
  ##  monthly data sets and rejects those plots that exceed a user-defined limit.
  ##  
  ##  parameters are as follows:
  ##  
  ##  data.indep (list):      List of monthly data sets of independent plots.
  ##                          Must be composed of ki.data objects.
  ##  prm.dep (character):    Parameter under investigation.
  ##  na.limit (numeric):     Accepted threshold percentage of NA values in monthly 
  ##                          data set of independent plots.
  ##  time.window (numeric):  Values to consider for fitting the model before and 
  ##                          after the gap.
  ##  pos.na (numeric):       Gap in data set of dependent plot including
  ##                          starting point, endpoint, and length of the gap.
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
      "Module   :  gfValidNaThreshold", "\n",
      "Author   :  Florian Detsch <florian.detsch@geo.uni-marburg.de>, Tim Appelhans <tim.appelhans@gmail.com>",
      "Version  :  2013-01-08", "\n",
      "License  :  GNU GPLv3, see http://www.gnu.org/licenses/", "\n",
      "\n")
  
  ########## FUNCTION BODY #######################################################
  

  # Time sequences before and after gap
  time.window.pre.seq <- seq(time.window.pre, pos.na[1] - 1)
  time.window.post.seq <- seq(pos.na[2] + 1, time.window.post)
  
  # Proportion of NA values in given time window
  na.ratio <- lapply(seq(data.indep), function(h) {
    sum(is.na(data.indep[[h]]@Parameter[[prm.dep]][c(time.window.pre.seq, time.window.post.seq)])) / length(c(time.window.pre.seq, time.window.post.seq))
  })
  
  # Reject plots with too high number of NA values
  data.indep <- data.indep[unlist(na.ratio) <= na.limit]
  return(data.indep)
}  
  
