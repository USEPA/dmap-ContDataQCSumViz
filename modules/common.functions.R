#' @param varNum 
#' @description This function is used to calculate height of plots
#' @export
calculatePlotHeight <- function(varNum) {
  plotHeight <- 400
  if(varNum > 2) {
    plotHeight <- plotHeight + ((varNum - 2) * 82)
  }
  return(plotHeight)
}

#' @param baseData 
#'
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

#' @param myDf 
#'
#' @export
fun.getMetaSummary <- function(myDf){
  all.days <- seq.Date(min(myDf$Date,  na.rm = TRUE),max(myDf$Date, na.rm = TRUE),by="day")
  N.missing.days <- (length(all.days)-sum(all.days %in% myDf$Date))+sum(myDf$sumNA>0)
  if (ncol(myDf)>2){
    N.days.flagged.fail <- sum(myDf$sumFail>0)
    N.days.flagged.suspect <- sum(myDf$sumSuspect>0)
    N.days.flagged.noFlagData <- sum(myDf$SumNoFlagData>0)
  }else{
    N.days.flagged.fail <- "No flag field found"
    N.days.flagged.suspect <- "No flag field found"
    N.days.flagged.noFlagData <- "No flag field found"
  }
  mySummary <- c(N.missing.days,N.days.flagged.fail,N.days.flagged.suspect,N.days.flagged.noFlagData)
  return(mySummary)
}

#' @param fileColNames 
#'
#' @param getDateCols 
#'
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

#' @param shandingName 
#'
#' @param userTitle 
#' @param lowerColumn 
#' @param upperColumn 
#'
#' @export
getMapTitle <- function(shandingName, userTitle, lowerColumn, upperColumn) {
  shadingText <- " \n <span style='font-size:10px'></span>"
  if (shandingName=="quantiles"){
    shadingText <- " \n <span style='font-size:10px'>(Shading between daily 25th percentiles and 75th percentiles)</span>"
  } else if (shandingName=="minMax"){
    shadingText <- " \n <span style='font-size:10px'>(Shading between daily minimum and maximum values)</span>"
  } else if (shandingName=="dynamic"){
    shadingText <- paste("\n <span style='font-size:10px'>(Shading between" , lowerColumn, "and", upperColumn, ")</span>", sep=" ")
  } else if (shandingName=="noShading"){
    shadingText <- paste("\n <span style='font-size:10px'>(No Shading applied)", "</span>", sep=" ")
  }
  
  if(userTitle != "") {
    shading_text = paste0(userTitle, shadingText)
  } else {
    shading_text =  paste0("Uploaded File metrics", shadingText)
  }
  return(shading_text)
}

#' @param df 
#'
#' @export
addSeason <- function(df=myDf){
  #df[,"year"] <- format(df[,"Date"],"%Y")
  df[,"year"] <- format(as.Date(df$Date, format="%Y-%m-%d %H:%M:%S"),"%Y")
  #df[,"monthday"] <- format(df[,"Date"],"%m%d")
  df[,"monthday"] <- format(as.Date(df$Date, format="%Y-%m-%d %H:%M:%S"),"%m%d")
  df[,"season"] <- NA
  df[,"season"][as.numeric(df[, "monthday"]) >= as.numeric("0101") & as.numeric(df[
    ,"monthday"])< as.numeric(ContData.env$myTimeFrame.Season.Spring.Start)] <- "Winter"
  df[,"season"][as.numeric(df[,"monthday"]) >= as.numeric(ContData.env$myTimeFrame.Season.Spring.Start) &
                  as.numeric(df[,"monthday"])< as.numeric(ContData.env$myTimeFrame.Season.Summer.Start)] <- "Spring"
  df[,"season"][as.numeric(df[,"monthday"]) >= as.numeric(ContData.env$myTimeFrame.Season.Summer.Start) &
                  as.numeric(df[,"monthday"])< as.numeric(ContData.env$myTimeFrame.Season.Fall.Start)] <- "Summer"
  df[,"season"][as.numeric(df[, "monthday"]) >= as.numeric(ContData.env$myTimeFrame.Season.Fall.Start) &
                  as.numeric(df[,"monthday"])< as.numeric(ContData.env$myTimeFrame.Season.Winter.Start)] <- "Fall"
  df[,"season"][as.numeric(df[, "monthday"]) >= as.numeric(ContData.env$myTimeFrame.Season.Winter.Start) &
                  as.numeric(df[,"monthday"])<= as.numeric("1231")] <- "Winter"
  df$season <- reorderSeason(seasonCol=df$season)
  df[,"yearseason"] <- paste(df[,"year"],df[,"season"], sep="")
  return(df)
}

#' @param seasonCol 
#'
#' @export
reorderSeason <- function(seasonCol=mySeasonCol){
  seasonNames <- unique(seasonCol)
  if (length(seasonNames)==4){
    seasonCol <- factor(seasonCol,levels=c("Spring","Summer","Fall","Winter"))
  }else if(length(seasonNames)==3 & !('Winter' %in% seasonNames)){
    seasonCol <- factor(seasonCol,levels=c("Spring","Summer","Fall"))
  }else if(length(seasonNames)==3 & !('Spring' %in% seasonNames)){
    seasonCol <- factor(seasonCol,levels=c("Summer","Fall","Winter"))
  }else if(length(seasonNames)==2 & !('Winter' %in% seasonNames)&!('Spring' %in% seasonNames)){
    seasonCol <- factor(seasonCol,levels=c("Summer","Fall"))
  }
  return(seasonCol)
}




