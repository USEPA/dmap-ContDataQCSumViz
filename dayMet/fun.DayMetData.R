#' Download DayMet Data
#'
#' This a wrapper function for the package dataRetrieval to get DayMet data.
#' Daymet gridded daily  are the default data type.
#'Uses library("daymetr") to download dayMetdata
#Citation
# Hufkens K., Basler J. D., Milliman T. Melaas E., Richardson A.D. 2018 An integrated phenology modelling framework in 
#R: Phenology modelling with phenor. Methods in Ecology & Evolution, 9: 1-10.
# 
# Acknowledgements
# This project was supported by the National Science Foundation’s Macro-system
# Biology Program (awards EF-1065029 and EF-1702697) and the Marie Skłodowska-Curie Action (H2020 grant 797668). 
# Logo design elements are taken from the FontAwesome library according to these terms, 
# where the globe element was inverted and intersected.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library("daymetr")
#' @export

fun.dayMetData <- function(
                           fun.lat,
                           fun.lon,
                           fun.year.Start,
                           fun.year.End,
                           fun.internal = TRUE
                           ) {
  tryCatch({
    dayMetData <- download_daymet(lat = fun.lat,
                                      lon = fun.lon,
                                      start = fun.year.Start,
                                      end = fun.year.End,
                                      internal = fun.internal)


    #write_xlsx(dayMetData$data, paste(getwd(),"/ContDataQC_test_data/dayMetDataReview.csv",sep=""))
    daymetProcessed <- list()
    daymetColumns <- colnames(dayMetData$data)
    daymetProcessed$daymetColumns <- daymetColumns[!(daymetColumns %in% c("year","yday","dayl..s."))]
    
    
    # daymetProcessed$dayMetData <- dayMetData$data%>% select(year, yday, precip="prcp..mm.day.") %>%
    #   mutate(Date=as.Date(yday, origin=paste(as.character(year - 1), "-12-31", sep="")))
    daymetProcessed$dayMetData <- dayMetData$data
    return(daymetProcessed)
    

  },error = function(err) {
    message("Error in download_daymet function")
    print(err)}
  )
 
}##FUN.fun.GageData.END


# test <- fun.dayMetData(fun.lat <- 39.44164,
#                        fun.lon <- -80.87723,
#                        fun.year.start <- 2015,
#                        fun.year.end <- 2020,
#                        fun.internal <-  TRUE)
# print(test)
