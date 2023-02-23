#' calculate time range for the X axis

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @export
calculate_time_range <- function(baseData) {
  
  if(!is.null(baseData$Date)) {
      time_range <- difftime(max(as.POSIXct(baseData$Date,format="%Y-%m-%d")),min(as.POSIXct(baseData$Date,format="%Y-%m-%d")),units="days")
      if (as.numeric(time_range)<365*2){
        myBreaks = paste0(1," months")
        x_date_label = "%Y-%m-%d"
        return(list(myBreaks, x_date_label))
      }else if(as.numeric(time_range)>=365*2&as.numeric(time_range)<365*5){
        myBreaks = paste0(2," months")
        x_date_label = "%Y-%m-%d"
        return(list(myBreaks, x_date_label))
      }else{
        myBreaks = paste0(6," months")
        x_date_label = "%Y-%m"
        return(list(myBreaks, x_date_label))
      }
  }
}
    


