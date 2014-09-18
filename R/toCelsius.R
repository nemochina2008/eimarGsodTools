#' Convert °F to °C
#' 
#' @export toCelsius
toCelsius <- function(val, 
                      ...) {
  
  val_new <- (val - 32) * 5 / 9
  val_new <- round(val_new, ...)
  
  return(val_new)
}
