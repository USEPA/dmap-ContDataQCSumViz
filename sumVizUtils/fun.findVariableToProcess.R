#' find variables to process from a file

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @export
fun.findVariableToProcess <- function(fileColNames, getDateCols= FALSE) {
  parmsToProcess <- NULL
  if(length(fileColNames) > 0) {
    all_date_related_keys <- c("Date.Time","DATE.TIME","Year","YEAR","Date","DATE","MonthDay","MONTHDAY","Time","TIME","Month","Day","RAW.Date.Time","mm.dd.yyyy.HH.MM.SS")
    all_date_columns <- all_date_related_keys[all_date_related_keys %in% fileColNames]
    
    ## this part is to find any column name related to "ID" or "Flag"
    idx_no_ID_Flag <- !str_detect(fileColNames,"ID") & !str_detect(fileColNames,"SITE") & !str_detect(fileColNames,"Flag") & !str_detect(fileColNames,"Comment")
    not_ID_or_Flag_cols <- fileColNames[idx_no_ID_Flag]
    parameters_cols_best_guess <- not_ID_or_Flag_cols[!not_ID_or_Flag_cols %in% all_date_columns]
    
    if(getDateCols == TRUE) {
      parmsToProcess <-  all_date_related_keys[all_date_related_keys %in% fileColNames]
    } else {
      parmsToProcess <- parameters_cols_best_guess
    }
  }
  return(parmsToProcess)
}
    
