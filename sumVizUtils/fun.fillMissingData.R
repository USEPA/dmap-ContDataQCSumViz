#' find variables to process from a file

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @export
fun.fillMissingData <- function(userData, dateColName) {
  #Not done yet
  timediff <- get_interval(userData[[dateColName]])
  #print(timediff)
  timediff <- ifelse(timediff == "min", "15 mins", timediff)
  
  #fill missing data
  userData <- userData %>%
    mutate(!! dateColName := as.POSIXct(deparse(substitute(dateColName)))) %>%
    complete(!! dateColName := seq(min(deparse(substitute(dateColName)),na.rm = TRUE), max(deparse(substitute(dateColName)), na.rm = TRUE), by=timediff))

  return(userData)
}
    
