#' Plot gage raw data

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @export

fun.gageStatsPlot <- function(
                           fun.gage.id,
                           fun.gage.data,
                           fun.gage.vars.to.process,
                           fun.stats.column,
                           fun.internal = TRUE
                           ) {
  tryCatch({

    if(fun.gage.id != "" && length(fun.gage.id) > 0 && nrow(fun.gage.data) > 0) {
      gage_variables_to_calculate <- fun.gage.vars.to.process
      
      ContData.env$myStats.Fails.Exclude = TRUE
      ContData.env$myStats.Suspects.Exclude = TRUE
      
      #for now, need to make it input base
      gageDailyStats <- SumStats.updated(fun.myFile=NULL
                                         ,fun.myDir.import=NULL
                                         ,fun.myParam.Name=gage_variables_to_calculate
                                         ,fun.myDateTime.Name="Date.Time"
                                         ,fun.myDateTime.Format="%Y-%m-%d %H:%M%:S"
                                         ,fun.myThreshold=20
                                         ,fun.myConfig=""
                                         ,df.input=fun.gage.data
      )
      

      gageList <- list()
      gageList2 <- Reduce(full_join, gageDailyStats)
      gageCols <- paste(gage_variables_to_calculate, fun.stats.column, sep=".")
      
      gageData  <- gageList2 %>%
        select(gageCols,"Date") %>%
        gather(key = "parameter", value = "value",-Date)
      
      main_range = calculate_time_range(as.list(gageData))
      mainBreaks = main_range[[1]]
      main_x_date_label = main_range[[2]]
      
      gagePlot <- ggplot(data = gageData, dynamicTicks = TRUE, aes(x=as.POSIXct(Date,format="%Y-%m-%d"), y = value)) +
        geom_line(aes(colour=parameter)) +
        scale_x_datetime(date_labels=main_x_date_label,date_breaks=mainBreaks)+
        labs(title="USGS gage metrics", x="Date", y="Parameters")+
        theme_bw()+
        theme(
          strip.background = element_blank()
          ,strip.placement = "outside"
          ,text=element_text(size=10,face = "bold", color="cornflowerblue")
          ,plot.title = element_text(hjust=0.5)
          ,legend.position="bottom"
          ,axis.text.x=element_text(angle=65, hjust=10)
        )
      
      gagePlot = gagePlot + facet_grid(parameter ~ ., scales = "free_y")
      return(gagePlot)
    } 
    

  },error = function(err) {
    message("Error in gageStatsPlot function")
    print(err)
  })
 
}##FUN.fun.GageStats.END

