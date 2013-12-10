#' Julendat identification of plots with valid measurements
gfNonNaStations <- function(data.indep, 
                            pos.na, 
                            prm.dep = "Ta_200", 
                            ...) {
  
################################################################################
##  
##  This program takes a known NA record from an incomplete monthly
##  data set of a given plot as input argument and identifies all 
##  other plots that provide valid records at this particular date
##  
##  parameters are as follows:
##  
##  data.indep (list):    List of data sets of independent plots.
##                        Must be composed of ki.data objects.
##  pos.na (numeric):     Gap in data set of dependent plot including
##                        starting point, endpoint, and length of the gap.
##  prm.dep (character):  Parameter under investigation.
##  ...                   Further arguments to be passed
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
    "Module   :  gfNonNaStations", "\n",
    "Author   :  Florian Detsch <florian.detsch@geo.uni-marburg.de>, Tim Appelhans <tim.appelhans@gmail.com>",
    "Version  :  2013-01-08", "\n",
    "License  :  GNU GPLv3, see http://www.gnu.org/licenses/", "\n",
    "\n")

########## FUNCTION BODY #######################################################
  
    
  # Identify plots with available records for the given gap
  data.temp <- lapply(seq(data.indep), function(i) {
    (pos.na[1]:pos.na[2]) %in% which(is.na(data.indep[[i]]@Parameter[[prm.dep]]))
  })

  data.avl <- lapply(seq(data.indep), function(i) {
    sum(data.temp[[i]]) == 0
  })

  # Plot names
  plot.avl <- lapply(seq(data.indep), function(i) {
    data.indep[[i]]@PlotId$Unique
  })
  
  # Return data.frame containing plot names and information about availablity of particular record
  return(data.frame(unlist(plot.avl), unlist(data.avl), stringsAsFactors=FALSE))
}
