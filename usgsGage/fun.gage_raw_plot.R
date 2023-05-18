#' Plot gage raw data

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @export

fun.gageRawPlot <- function(
                           fun.gage.id,
                           fun.gage.data,
                           fun.gage.vars.to.process,
                           fun.year.End,
                           fun.internal = TRUE
                           ) {
  tryCatch({
        print("insided fun.gageRawPlot function")
        raw_data_merged <- fun.gage.data
        print("printing parameters")
        print(paste0("fun.gage.data:", head(fun.gage.data)))
        print(paste0("fun.gage.vars.to.process:", fun.gage.vars.to.process))
        print(paste0("fun.internal:", fun.internal))
        # covers if user removes GageId from the dropdown
        if(fun.gage.vars.to.process != "" && length(fun.gage.vars.to.process) > 0) {
          print("in the if statement after fun.gage.vars.to.process check")
          raw_data_merged  <- raw_data_merged %>%
            select(any_of(fun.gage.vars.to.process), c("GageID", "Date.Time")) %>%
            gather(key = "parameter", value = "value",-GageID, -Date.Time)
          print("successfully prepared raw_data_merged in the if block")
          print(raw_data_merged)
        } else {
          print("could not goto if so in the else")
          raw_data_merged  <- raw_data_merged %>%
            select(-ends_with("SiteID")) %>%
            gather(key = "parameter", value = "value",-GageID, -Date.Time)
          print("successfully prepared raw_data_merged in the else block")
        }
        
        main_range = calculate_time_range(as.list(raw_data_merged))
        mainBreaks = main_range[[1]]
        main_x_date_label = main_range[[2]]
        
        print("successfully calculated the plot breaks, now going to plot")
        
        p <- ggplot(data = raw_data_merged, aes(x = Date.Time, y = value)) +
          geom_line(aes(colour=parameter)) +
          labs(title="USGS gage Raw Data", y="Parameters", x="Date") + 
          scale_x_datetime(date_labels=main_x_date_label,date_breaks=mainBreaks)+
          theme_bw()+
          theme(
            strip.background = element_blank()
            ,strip.text.y = element_blank()
            ,strip.placement = "outside" 
            ,text=element_text(size=10,face = "bold", color="cornflowerblue")
            ,plot.title = element_text(hjust=0.5)
            ,legend.position="bottom"
            ,axis.text.x=element_text(angle=65, hjust=10)
          )
        p = p + facet_grid(parameter ~ ., scales = "free_y")
        print("successfully prepared the ggplot and returning the object for plotting")
        return(p)
    

  },error = function(err) {
    message("Error in fun.gageRawPlot function")
    print(err)
  })
 
}##FUN.fun.GageData.END

