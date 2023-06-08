#' Plot gage raw data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @param fun.gage.id 
#'
#' @param fun.gage.data 
#' @param fun.gage.vars.to.process 
#' @param fun.year.End 
#' @param fun.internal 
#'
#' @export

fun.gageRawPlot <- function(
                           fun.gage.id,
                           fun.gage.data,
                           fun.gage.vars.to.process,
                           fun.year.End,
                           fun.internal = TRUE
                           ) {
  tryCatch({

        raw_data_merged <- fun.gage.data
        # covers if user removes GageId from the dropdown
        if(length(fun.gage.vars.to.process) > 0) {
          raw_data_merged  <- raw_data_merged %>%
            select(any_of(fun.gage.vars.to.process), c("GageID", "Date.Time")) %>%
            gather(key = "parameter", value = "value",-GageID, -Date.Time)
        } else {
          raw_data_merged  <- raw_data_merged %>%
            select(-ends_with("SiteID")) %>%
            gather(key = "parameter", value = "value",-GageID, -Date.Time)
        }
        
        main_range = calculate_time_range(as.list(raw_data_merged))
        mainBreaks = main_range[[1]]
        main_x_date_label = main_range[[2]]
        
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
        return(p)
    

  },error = function(err) {
    message("Error in fun.gageRawPlot function")
    print(err)
  })
 
}##FUN.fun.GageData.END

