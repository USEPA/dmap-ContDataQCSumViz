#'USGS & Daymet exploration tab (user interface)
#'
#' @param id 
#'
GageAndDaymetModuleUI <- function(id) {
  ns <- NS(id)
  shinyjs::useShinyjs()
  sidebarLayout(
    sidebarPanel(
      width = 3,
      uiOutput(ns("gage_panel")),
      uiOutput(ns("daymet_panel")),
      uiOutput(ns("base_gage_daymet_panel"))
    ),
    mainPanel(
      width = 9,
      column(width = 12,
             div(style="width:100%", uiOutput(ns("gageDayMetError"))),
             withSpinner(plotlyOutput(ns("display_downloaded_data"))),type=1)
    ) # mainPanel end
  ) # sidebarLayout end
}

#'USGS & Daymet exploration tab (server side)
#'
#' @param id 
#' @param homeDTvalues 
#' @param dateRange 
#' @param formated_raw_data 
#' @param renderUsgsAndDaymet 
#'
GageAndDaymetModuleServer <- function(id, homeDTvalues, dateRange, formated_raw_data, renderUsgsAndDaymet) {
  
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      gageColNames  <- NULL
      daymetCols <- NULL
      consoleUSGS <- NULL
      gageRawData <- reactiveValues(gagedata = data.frame(),
                                    gageColName = as.character())
      dayMetRawData <- reactiveValues(dayMetData = data.frame(),
                                      daymetColumns = as.character())
      
      output$display_downloaded_data <- renderPlotly({
        plotly_empty()
      })

      observe({
        if(renderUsgsAndDaymet$render == TRUE) {
          
          output$gage_panel <- renderUI({
            div(class="panel panel-default", style="padding:10px;margin-top:20px;",
                div(class = "panel-heading", style="padding:10px 5px 10px 10px;",
                    span("USGS Gage data", style="font-weight:bold;"),
                    a("View Gage Ids", href="https://waterdata.usgs.gov/nwis/rt", target="_blank", style="float:right")
                ),
                div(style="padding:5px;",
                    textInput(inputId=ns("gage_id"), label="Gage Id",value=""),
                    actionButton(inputId=ns("display_gage_ts"), label="Download USGS gage data",class="btn btn-primary")
                ),
                div(id=ns("gageVarsDiv") , style="padding:5px;display:none",
                    selectizeInput(ns("gaze_params"), label ="Select USGS gage variables",
                                   choices=gageColNames,
                                   multiple = TRUE,
                                   selected=NULL,
                                   options = list(hideSelected = FALSE)),
                    actionButton(inputId=ns("display_gage_raw"), label="View USGS raw data",class="btn btn-primary")
                )
            )
          })
          
          # END OF USGS gage
          #Daymet
          output$daymet_panel <- renderUI({
            div(class="panel panel-default", style="padding:10px;",
                div(class = "panel-heading", style="padding:10px 5px 10px 10px;",
                    span("DayMet data", style="font-weight:bold;")),
                div(style="padding:5px;",
                    textInput(inputId=ns("daymet_lat"), label="Site Latitude",value=""),
                    textInput(inputId=ns("daymet_long"), label="Site Longitude",value=""),
                    actionButton(inputId=ns("get_daymet_data"), label="Download Daymet data",class="btn btn-primary")
                ),
                div(id=ns("daymetVarsDiv"), style="padding:5px;display:none",
                    selectizeInput(ns("daymet_params"), label ="Select Daymet variables",
                                   choices=daymetCols,
                                   multiple = TRUE,
                                   selected=daymetCols[1],
                                   options = list(hideSelected = FALSE)),
                    actionButton(inputId=ns("display_daymet_raw"), label="View Daymet raw data",class="btn btn-primary")
                )
            ) #end of parent div
          })
          #end of Daymet

     
          output$base_gage_daymet_panel <- renderUI({
            variables_avail <- homeDTvalues$homeDateAndTime$parmToProcess()
            div(class="panel panel-default", style="padding:10px;",
                div(class = "panel-heading", style="padding:10px 5px 10px 10px;",
                    span("View Base, Gage and DayMet data merged in a subplot", style="font-weight:bold;", icon("info-circle", style = "color: #2fa4e7", id=ns("baseDataDef")))
                ),
                bsPopover(id=ns("baseDataDef"), title="What is base data\\?", content = "Base data is uploaded on the \\'Upload Data\\' tab.", 
                          placement = "right", trigger = "hover"),
                div(style="padding:5px;",
                    selectizeInput(ns("dailyStats_ts_variable_name2"),label ="Select base variable names",
                                   choices=variables_avail,
                                   multiple = TRUE,
                                   selected=variables_avail[1],
                                   options = list(hideSelected = FALSE)),
                    actionButton(inputId=ns("display_subplot_ts"), label="View Base, Gage and DayMet Merged",class="btn btn-primary")
                )
            )
          })
   
        } # end of reder if
      })
      
      ########USGS Gage########
      #Download USGS gage data
      observeEvent(input$display_gage_ts, {
        clearContents()
        if(input$gage_id != "" && length(input$gage_id) > 0) {
          #data <- uploaded_data()
          consoleUSGS$disp <- data.frame(consoleOutputUSGS = character())
          Sys.sleep(0.5)
          defaultTimeZone = "America/New_York"
          
          withProgress(message = paste("Getting USGS data"), value = 0, {
            incProgress(0, detail = paste("Retrieving records for site ", input$gage_id))
            #Actually gets the gage data from the USGS NWIS system
            tryCatch({
            gageRawData$gagedata <- fun.GageData(
              myData.SiteID           <- input$gage_id,
              myData.Type             <- "Gage",
              myData.DateRange.Start  <- as.character(dateRange$min),
              myData.DateRange.End    <- as.character(dateRange$max),
              myDir.export            <- file.path(".", "data"),
              fun.myTZ = defaultTimeZone #ContData.env$myTZ
            )
            }, error = function(err){
              print("Error while dowonloading data from USGS gage")
              print(err$message)
              renderErrorMsg(err$message)
            })
            message("USGS data retrieved")
            #Fills in the progress bar once the operation is complete
            incProgress(1/1, detail = paste("Retrieved records for site ", input$gage_id))
            Sys.sleep(1)
          })
         
          #print(gageRawData$gagedata)
          allVars <- colnames(gageRawData$gagedata)
          varsToPlot <- allVars[!(allVars %in% c("SiteID","GageID","Date.Time"))]
          updateSelectizeInput(session, 'gaze_params', choices = varsToPlot, selected = varsToPlot[1])
          shinyjs::show(id=ns("gageVarsDiv"),asis=TRUE)
          
          #Names the single column of the R console output data.frame
          colnames(consoleUSGS$disp) <- "R console messages for all USGS data retrieval:"
        } else {
          renderErrorMsg(noGageIdFound)
        }
      })
      
      observeEvent(input$display_gage_raw, {
        clearContents()
        output$display_downloaded_data <- renderUI({})
        if (input$gage_id != "" & length(input$gage_id) > 0 & nrow(gageRawData$gagedata) > 0 & length(input$gaze_params) > 0) {
          gageRawPlot <- fun.gageRawPlot(fun.gage.data = gageRawData$gagedata,
                                         fun.gage.vars.to.process = input$gaze_params,
                                         fun.internal = TRUE)
          
          output$display_downloaded_data <- renderPlotly({
            ggplotly(gageRawPlot, height = calculatePlotHeight(length(isolate(input$gaze_params)) * 2)) 
            #%>% plotly::layout(legend = list(orientation = "h", x = 0.4, y = -0.3))
          })
        } else {
          if (input$gage_id != "" & input$gage_id > 0 & nrow(gageRawData$gagedata) == 0) {
            renderErrorMsg(noGagaDataDownloaded)
            clearPlot()
          } else if (input$gage_id != "" & input$gage_id > 0 & length(input$gaze_params) == 0){
            renderErrorMsg(selectGageVars)
            clearPlot()
          } else if (input$gage_id == "") {
            renderErrorMsg(noGageIdFound)
            clearPlot()
          }
        }
      })

      
      ########DayMet########
      observeEvent(input$get_daymet_data, {
        clearContents()
        clearPlot()
        #Sys.sleep(0.5)
        if (input$daymet_lat != "" && length(input$daymet_lat) > 0 && input$daymet_long != "" && length(input$daymet_long) > 0) {
          
          withProgress(message = "Getting DayMet data", value = 0, {
            incProgress(0, detail = paste("Retrieving records for Latitude and Longitude ", input$daymet_lat, input$daymet_long))
            
            startYear <- format(dateRange$min, format='%Y')
            endYear <- format(dateRange$max, format='%Y')
            
            #Actually gets the gage data from the USGS NWIS system
            tryCatch({
            rawResult <- fun.dayMetData(
              fun.lat <- input$daymet_lat,
              fun.lon <- input$daymet_long,
              fun.year.start <-  as.numeric(startYear),
              fun.year.end <-  as.numeric(endYear),
              fun.internal <-  TRUE
            )
            }, error = function(err){
              print("error while downloading dayMet data from NWIS system")
              renderErrorMsg(err$message)
            })
            dayMetRawData$dayMetData <- rawResult$dayMetData
            daymetCols <- rawResult$daymetColumns
            dayMetRawData$daymetColumns <- rawResult$daymetColumns
            updateSelectizeInput(session, 'daymet_params', choices = daymetCols, selected = daymetCols[1])
            
            shinyjs::show(id="daymetVarsDiv")
            
            #Fills in the progress bar once the operation is complete
            incProgress(1/1, detail = paste("Retrieved records for Latitude and Longitude ",input$daymet_lat, input$daymet_long))
          })
        } else {
          renderErrorMsg(noDaymetDataDownloaded)
          clearPlot()
        }
        
      })
      
      observeEvent(input$display_daymet_raw, {
        clearContents()
        dayMetPlotRaw <- draw_daymet_raw("DayMet Raw Data Plot")
        if (!is.null(dayMetPlotRaw) & length(input$daymet_params) > 0) {
          #output$display_time_series_3 <- renderPlotly({
          output$display_downloaded_data <- renderPlotly({
            ggplotly(dayMetPlotRaw, height = calculatePlotHeight(length(isolate(input$daymet_params)) * 2)) 
            #%>% plotly::layout(legend = list(orientation = "h", x = 0.4, y = -0.3))
          })
        } else {
          alertMsg <- NULL
          if (is.null(dayMetRawData$dayMetData)) {
            alertMsg <- downlaodDaymetData
          } else if(!is.null(dayMetRawData$dayMetData) & length(input$daymet_params) == 0) {
            alertMsg <- selectDaymetVars
          }
          renderErrorMsg(alertMsg)
          clearPlot()
        }
      }) 
      
      draw_daymet_raw <- function(plotTitle) {
        dayMetPlot <- NULL
        if (!is.null(dayMetRawData$dayMetData) & length(input$daymet_params) > 0) {
          dayMetPlot <-  fun.dayMetRawPlot(
            fun.daymet.data = dayMetRawData$dayMetData,
            fun.daymet.vars.to.process = input$daymet_params,
            fun.daymet.title = plotTitle,
            fun.internal = TRUE
          )
        } 
        return(dayMetPlot)
      }
      
      #subplot for daymet, gage and base
      observeEvent(input$display_subplot_ts, {
        
        daymet_data_raw <- NULL
        gage_data_raw <- NULL
        base_data_raw <- NULL
        mergedList <- list()
        totalH <- 0L
        if(length(input$dailyStats_ts_variable_name2) > 0) {
          clearContents()
          #test all row data
          if (!is.null(dayMetRawData$dayMetData) & length(input$daymet_params) > 0) {
            daymet_data_raw  <- dayMetRawData$dayMetData %>%
              select(c(input$daymet_params), c("year", "yday")) %>%
              mutate(Date=as.Date(yday, origin=paste(as.character(year - 1), "-12-31", sep=""))) %>%
              select(c(input$daymet_params), "Date") %>%
              gather(key = "parameter", value = "value",-Date)
            allParames <- daymet_data_raw %>% pull(parameter)
            
            totalH <- totalH + length(input$daymet_params)
            daymet_data_raw$parameter <- paste("DayMet",allParames,sep="_")
            mergedList[["DayMet"]] <- daymet_data_raw
          }
          
          if(input$gaze_params != "" && length(input$gaze_params) > 0) {
            #gageGroup <- paste(input$gage_params, "Gage", sep=".")
            gage_data_raw  <- gageRawData$gagedata %>%
              select(all_of(input$gaze_params), all_of("GageID"), 'Date'=all_of("Date.Time")) %>%
              gather(key = "parameter", value = "value",-GageID, -Date)
            
            totalH <- totalH + length(input$gaze_params)
            allParames <- gage_data_raw %>% pull(parameter)
            gage_data_raw$parameter <- paste("Gage",allParames,sep="_")
            mergedList[["Gage"]] <- gage_data_raw
          }
          raw_data <- formated_raw_data$derivedDF
          ##print(formated_raw_data$derivedDF)
          variable_to_plot <- input$dailyStats_ts_variable_name2
          if (!is.null(variable_to_plot) & nrow(raw_data) != nrow(raw_data[is.na(raw_data$date.formatted),])){
            
            
            timediff <- get_interval(raw_data$date.formatted)
            #print(timediff)
            timediff <- ifelse(timediff == "min", "15 mins", timediff)
            
            raw_data <- raw_data %>%
              mutate(date.formatted = as.POSIXct(date.formatted)) %>%
              complete(date.formatted = seq(min(date.formatted,na.rm = TRUE), max(date.formatted, na.rm = TRUE), by=timediff))
            
            base_data_raw  <- raw_data %>%
              select(any_of(variable_to_plot), Date= c("date.formatted")) %>%
              gather(key = "parameter", value = "value", -Date)
            
            totalH <- totalH + length(variable_to_plot)
            allParames <- base_data_raw %>% pull(parameter)
            base_data_raw$parameter <- paste("BaseFile",allParames,sep="_")
            mergedList[["BaseFile"]] <- base_data_raw
          }
          
          
          main_range = calculate_time_range(as.list(base_data_raw))
          mainBreaks = main_range[[1]]
          main_x_date_label = main_range[[2]]
          
          allCom <- ggplot(arrange(bind_rows(mergedList, .id="df"),parameter), aes(x = as.POSIXct(Date,format="%Y-%m-%d"), y = value)) +
            geom_line(aes(colour=parameter)) +
            labs(title="Base, USGS gage and DayMet Data", y="Parameters", x="Date") + 
            scale_x_datetime(date_labels=main_x_date_label,date_breaks=mainBreaks)+
            theme_bw()+
            theme(
              strip.background = element_blank()
              #,strip.text.y = element_blank()
              ,strip.placement = "outside" 
              ,text=element_text(size=10,face = "bold", color="cornflowerblue")
              ,plot.title = element_text(hjust=0.5)
              ,legend.position="bottom"
              ,axis.text.x=element_text(angle=65, hjust=10)
            )
          allCom = allCom + facet_grid(parameter ~ ., scales = "free_y")
          
          
          output$display_downloaded_data <- renderPlotly({
            ggplotly(allCom, height = calculatePlotHeight(totalH*2)) 
            #%>% plotly::layout(legend = list(orientation = "h", x = 0.4, y = -0.4))
          }) 
          #overridePotlyStyle("display_downloaded_data")
        } else {
          renderErrorMsg(selectBaseVars)
          clearPlot()
        }
      })
      
      
      #common
      renderErrorMsg <- function(msg) {
        output$gageDayMetError <- renderUI({
          div(class="alert alert-danger" , msg) 
        })
      }
      clearContents <- function(){
        output$gageDayMetError <- renderUI({})
      }
      
      clearPlot <- function(){
        output$display_downloaded_data <- renderPlotly({
          plotly_empty()
        })
      }

    }) # end of module server
}
