#' Validate Date and Time format
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic Operations:
# validate date and convert to correct format needed for the application
# uses lubridate package
#library(lubridate)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' @keywords internal
#' @export
fun.ConvertDateFormat <- function(fun.userDateFormat
                     ,fun.userTimeFormat
                     ,fun.userTimeZone ="UTC"
                     ,fun.userDateFieldName
                     ,fun.userTimeFieldName
                     ,fun.rawData
                     ,fun.date.org) {


  # print(fun.userDateFieldName)
  # print(fun.userTimeFieldName)
  # print(fun.userTimeZone)
  # print(fun.userDateFormat)
  # print(fun.userTimeFormat)
  # print(fun.date.org)
    
    if(fun.date.org == 'combined') {
        tmpDateData <- fun.rawData %>% pull(fun.userDateFieldName)
        if(fun.userTimeFormat == "None") {
          fun.userTimeFormat <- "Hour, Minute, Second"
          tmpDateData <- paste(tmpDateData, "00:00:00", sep=" ")
        }
        
        fun.rawData <- fun.sub.format.date(fun.userDateFormat, fun.userTimeFormat, fun.userTimeZone, fun.rawData, tmpDateData)
    } else if (fun.date.org == 'separate' && !is.null(fun.userTimeFieldName)) {
        tmpDateData <- fun.rawData %>% pull(fun.userDateFieldName)
        tmpTimeDate <- fun.rawData %>% pull(fun.userTimeFieldName)
        if(fun.userTimeFormat == "None") {
          fun.userTimeFormat <- "Hour, Minute, Second"
          tmpCol <- paste(tmpDateData, "00:00:00", sep=" ")
        } else {
          tmpCol <- paste(tmpDateData, tmpTimeDate, sep=" ")
        }
        fun.rawData <- fun.sub.format.date(fun.userDateFormat, fun.userTimeFormat, fun.userTimeZone, fun.rawData, tmpCol)
    }
    return(fun.rawData)
}##FUN.fun.ConvertDateFormat.END

fun.sub.format.date <- function(fun.userDateFormat, fun.userTimeFormat, fun.userTimeZone, fun.rawData, tmpData) {
  
  if(fun.userDateFormat == "Year, Month, Day" && fun.userTimeFormat == "Hour, Minute") {
    fun.rawData$date.formatted <- lubridate::ymd_hm(tmpData, tz=fun.userTimeZone)
  } else if(fun.userDateFormat == "Year, Day, Month" && fun.userTimeFormat == "Hour, Minute") {
    fun.rawData$date.formatted <- lubridate::ydm_hm(tmpData, tz=fun.userTimeZone)
  } else if(fun.userDateFormat == "Month, Day, Year" && fun.userTimeFormat == "Hour, Minute") {
    fun.rawData$date.formatted <- lubridate::mdy_hm(tmpData, tz=fun.userTimeZone)
  }  else if(fun.userDateFormat == "Day, Monty, Year" && fun.userTimeFormat == "Hour, Minute") {
    fun.rawData$date.formatted <- lubridate::dmy_hm(tmpData, tz=fun.userTimeZone)
  } else if(fun.userDateFormat == "Abbreviated month, Day of the month, Year" && fun.userTimeFormat == "Hour, Minute") {
    fun.rawData$date.formatted <- lubridate::mdy_hm(tmpData, tz=fun.userTimeZone)
  } 
  
  else if(fun.userDateFormat == "Year, Month, Day" && fun.userTimeFormat == "Hour, Minute, Second") {
    fun.rawData$date.formatted <- lubridate::ymd_hms(tmpData, tz=fun.userTimeZone)
  } else if(fun.userDateFormat == "Year, Day, Month" && fun.userTimeFormat == "Hour, Minute, Second") {
    fun.rawData$date.formatted <- lubridate::ydm_hms(tmpData, tz=fun.userTimeZone)
  } else if(fun.userDateFormat == "Month, Day, Year" && fun.userTimeFormat == "Hour, Minute, Second") {
    fun.rawData$date.formatted <- lubridate::mdy_hms(tmpData, tz=fun.userTimeZone)
  }  else if(fun.userDateFormat == "Day, Monty, Year" && fun.userTimeFormat == "Hour, Minute, Second") {
    fun.rawData$date.formatted <- lubridate::dmy_hms(tmpData, tz=fun.userTimeZone)
  } else if(fun.userDateFormat == "Abbreviated month, Day of the month, Year" && fun.userTimeFormat == "Hour, Minute, Second") {
    fun.rawData$date.formatted <- lubridate::mdy_hms(tmpData)
  }
  
  else if(fun.userDateFormat == "Year, Month, Day" && fun.userTimeFormat == "Hour in 12-hour format, Minute, AM/PM") {
    fun.rawData$date.formatted <- lubridate::ymd_hm(tmpData, tz=fun.userTimeZone)
  } else if(fun.userDateFormat == "Year, Day, Month" && fun.userTimeFormat == "Hour in 12-hour format, Minute, AM/PM") {
    fun.rawData$date.formatted <- lubridate::ydm_hm(tmpData, tz=fun.userTimeZone)
  } else if(fun.userDateFormat == "Month, Day, Year" && fun.userTimeFormat == "Hour in 12-hour format, Minute, AM/PM") {
    fun.rawData$date.formatted <- lubridate::mdy_hm (tmpData, tz=fun.userTimeZone)
  }  else if(fun.userDateFormat == "Day, Monty, Year" && fun.userTimeFormat == "Hour in 12-hour format, Minute, AM/PM") {
    fun.rawData$date.formatted <- lubridate::dmy_hm(tmpData, tz=fun.userTimeZone)
  } else if(fun.userDateFormat == "Abbreviated month, Day of the month, Year" && fun.userTimeFormat == "Hour in 12-hour format, Minute, AM/PM") {
    fun.rawData$date.formatted <- lubridate::mdy_hm(tmpData, tz=fun.userTimeZone)
  }
  
  else if(fun.userDateFormat == "Year, Month, Day" && fun.userTimeFormat == "Hour in 12-hour format, Minute, Second, AM/PM") {
    fun.rawData$date.formatted <- lubridate::ymd_hms(tmpData, tz=fun.userTimeZone)
  } else if(fun.userDateFormat == "Year, Day, Month" && fun.userTimeFormat == "Hour in 12-hour format, Minute, Second, AM/PM") {
    fun.rawData$date.formatted <- lubridate::ydm_hms(tmpData, tz=fun.userTimeZone)
  } else if(fun.userDateFormat == "Month, Day, Year" && fun.userTimeFormat == "Hour in 12-hour format, Minute, Second, AM/PM") {
    fun.rawData$date.formatted <- lubridate::mdy_hms(tmpData, tz=fun.userTimeZone)
  }  else if(fun.userDateFormat == "Day, Monty, Year" && fun.userTimeFormat == "Hour in 12-hour format, Minute, Second, AM/PM") {
    fun.rawData$date.formatted <- lubridate::dmy_hms(tmpData, tz=fun.userTimeZone)
  } else if(fun.userDateFormat == "Abbreviated month, Day of the month, Year" && fun.userTimeFormat == "Hour in 12-hour format, Minute, Second, AM/PM") {
    fun.rawData$date.formatted <- lubridate::mdy_hms(tmpData, tz=fun.userTimeZone)
  }
  
  return(fun.rawData)
}
