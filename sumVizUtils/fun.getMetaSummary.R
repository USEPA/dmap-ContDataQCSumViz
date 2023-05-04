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