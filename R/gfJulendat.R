#' Fill medium-sized continuous gaps via the Julendat gap-filling routine
#' 
#' @export gfJulendat
gfJulendat <- function(files.dep, 
                       files.indep, 
                       filepath.coords = NULL, 
                       quality.levels = NULL, 
                       gap.limit,
                       end.datetime,
                       units = "hours", 
                       na.limit,
                       time.window,
                       n.plot,
                       prm.dep, 
                       prm.indep, 
                       family,
                       plevel,
                       save.output = F, 
                       ...) {
  
  # Perform imputation of missing values
  imputation.data <- gfRun(files.dep = files.dep,
                           files.indep = files.indep,
                           filepath.coords = filepath.coords, 
                           quality.levels = quality.levels,
                           gap.limit = gap.limit,
                           end.datetime = end.datetime,
                           units = units, 
                           na.limit = na.limit,
                           time.window = time.window,
                           n.plot = n.plot,
                           prm.dep = prm.dep, 
                           prm.indep = prm.indep, 
                           family = family, 
                           plevel = plevel)
  
  # Save (optional) and return gap-filled data
  if (save.output)
    write.csv(imputation.data[[2]], ...)
  
  return(imputation.data[[2]])
}
