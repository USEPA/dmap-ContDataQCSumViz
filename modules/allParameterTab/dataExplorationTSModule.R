#' Continuous Data Exploration / All Parameter / Time Series (user interface side)
#' 
#' @param id

DataExplorationTSModuleUI <- function(id) {
  ns <- NS(id)
  shinyjs::useShinyjs()
  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(
        class = "panel panel-default", width = "100%", style = "margin:10px;",
        div(class = "panel-heading"),
        div(
          class = "panel-body",
          uiOutput(ns("time_series_input_1")),
          uiOutput(ns("time_series_input_2")),
          div(
            id = ns("cp_shaded_region"),
            uiOutput(ns("time_series_input_3")),
          ), # div end
          # Tom Faber asked to comment this out for now
          # shinyjs::hidden(
          #   div(
          #     id = ns("cp_new_data"),
          #     conditionalPanel(
          #       condition = "input$dailyStats_shading == 'newData' ",
          #       hr(),
          #       fileInput(ns("uploaded_newData_file"),
          #                 label = "Upload your new data", multiple = FALSE,
          #                 accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
          #       )
          #     ) # conditionalPanel end
          #   ) # div end
          # ), # shinyjs:: hidden end
          uiOutput(ns("time_series_input_4")),
          hr(),
          fluidRow(
            column(
              width = 9,
              uiOutput(ns("time_series_input_5"))
            )
          ) # fluidRow close,
        ) # end of panel body
      ) # end of panel
    ),
    mainPanel(
      width = 9,
      fluidRow(column(
        width = 12,
        div(style = "width:100%", uiOutput(ns("tsError"))),
        plotlyOutput(ns("display_time_series"))
      ))
    ) # mainPanel end
  ) # sidebarLayout end
}

#' Continuous Data Exploration / All Parameter / Time Series (server side)
#' 
#' @param id 
#' @param dailyStats 
#' @param renderDataExp 
#'
#'
DataExplorationTSModuleServer <- function(id, dailyStats, renderDataExp) {
  localStats <- reactiveValues(stats = list())
  variables_avail <- reactiveValues(params = list())

  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      observe({
        localStats <- dailyStats
        variables_avail$params <- names(localStats$processed_dailyStats)
        localStats$stats <- localStats$processed_dailyStats
        clearPlot()
        clearContents()
        # print(localStats$stats)
      })


      observe({
        if (renderDataExp$render == TRUE) {
          output$time_series_input_1 <- renderUI({
            # variables_avail <- names(localStats$processed_dailyStats)
            selectizeInput(ns("dailyStats_ts_variable_name"),
              label = "Select variable name",
              choices = variables_avail$params,
              multiple = TRUE,
              selected = variables_avail$params[1],
              options = list(hideSelected = FALSE)
            )
          })
          output$time_series_input_2 <- renderUI({
            selectizeInput(ns("dailyStats_ts_metrics"),
              label = "Select daily statistics metrics",
              choices = c("mean", "median", "min", "max", "range", "sd", "var", "cv", "n"),
              multiple = FALSE,
              selected = "mean",
              options = list(hideSelected = FALSE)
            )
          })
          output$time_series_input_3 <- renderUI({
            div(
              radioButtons(ns("dailyStats_shading"), "Add shading",
                choices = c(
                  "No shading" = "noShading",
                  "25th & 75th percentiles" = "quantiles",
                  "Minimum & Maximum" = "minMax"
                ),
                selected = "noShading"
              )
            )
          })
          output$time_series_input_4 <- renderUI({
            textInput(inputId = ns("dailyStats_ts_title"), label = "Plot title", value = "")
          })

          output$time_series_input_5 <- renderUI({
            tagList(
              actionButton(inputId = ns("display_ts"), label = "Display", class = "btn btn-primary")
            )
          })
        }
      })

      observeEvent(input$dailyStats_ts_metrics, {
        if (!is.null(input$dailyStats_ts_metrics) & (input$dailyStats_ts_metrics == "mean" | input$dailyStats_ts_metrics == "median")) {
          shinyjs::show("cp_shaded_region")
        } else {
          reset(ns("dailyStats_shading"), asis=TRUE)
          shinyjs::hide("cp_shaded_region")
        }
        # click("display_ts")
      })
      observeEvent(input$display_ts, {
        clearContents()
        clearPlot()
        localStats <- dailyStats
        if (length(variables_avail$params) > 0 & length(input$dailyStats_ts_variable_name) > 0) {
          # Display uploaded file stats
          if ((!is.null(input$dailyStats_ts_metrics) & (input$dailyStats_ts_metrics == "mean" | input$dailyStats_ts_metrics == "median")) & input$dailyStats_shading != "noShading") {
            mainPlot <- draw_uploaded_file_ts()
            if (!is.null(mainPlot) & length(input$dailyStats_ts_variable_name) > 0) {
              output$display_time_series <- renderPlotly({
                ggplotly(mainPlot, height = calculatePlotHeight(length(isolate(input$dailyStats_ts_variable_name)) * 2))
                # %>% plotly::layout(legend = list(orientation = "h", x = 0.4, y = -0.4))
              })
            }
          } else {
            basePlot <- draw_uploaded_file_stats()
            if (!is.null(basePlot)) {
              output$display_time_series <- renderPlotly({
                ggplotly(basePlot, height = calculatePlotHeight(length(isolate(input$dailyStats_ts_variable_name)) * 2))
                # %>% plotly::layout(legend = list(orientation = "h", x = 0.4, y = -0.4))
              })
            }
          }
        } else {
          renderErrorMsg(calculateDailyStatsMsg)
          clearPlot()
        }
      }) # observeEvent end


      draw_uploaded_file_ts <- function() {
        mainPlot <- NULL
        mainMapTitle <- getMapTitle(input$dailyStats_shading, input$dailyStats_ts_title, lowerColumn = NULL, upperColumn = NULL)
        localStats <- dailyStats
        statsList <- localStats$stats
        variable_to_plot <- input$dailyStats_ts_variable_name

        mainData <- Reduce(full_join, statsList)
        # shadingText <- paste0(variable_to_plot, " between daily 25th percentiles and 75th percentiles")
        mainList <- list()
        for (varName in variable_to_plot) {
          if (input$dailyStats_shading == "quantiles") {
            tempData <- as.data.frame(mainData %>% select(value = paste(varName, input$dailyStats_ts_metrics, sep = "."), lower_col = paste(varName, "q.25%", sep = "."), upper_col = paste(varName, "q.75%", sep = "."), Date = Date))
            if (ContData.env$myStats.missing.data.fill == TRUE) {
              timediff <- get_interval(tempData$Date)
              # print(timediff)
              timediff <- ifelse(timediff == "min", "15 mins", timediff)
              tempData <- as.data.frame(tempData %>%
                mutate(Date = as.POSIXct(Date)) %>%
                complete(Date = seq(min(Date, na.rm = TRUE), max(Date, na.rm = TRUE), by = timediff)))
            }
            tempData$lowerBoundgrp <- paste(varName, input$dailyStats_ts_metrics, ': lower bound', sep = ".")
            tempData$upperBoundgrp <- paste(varName, input$dailyStats_ts_metrics, ': upper bound', sep = ".")

            mainList[[paste(varName, input$dailyStats_ts_metrics, sep = ".")]] <- tempData

            # mainList[[paste(varName,input$dailyStats_ts_metrics, sep=".")]] <- as.data.frame(mainData %>% select(value=paste(varName,input$dailyStats_ts_metrics, sep="."), lower_col= paste(varName, "q.25%", sep="."), upper_col=paste(varName, "q.75%", sep="."), Date=Date))
          } else if (input$dailyStats_shading == "minMax") {
            tempData <- as.data.frame(mainData %>% select(value = paste(varName, input$dailyStats_ts_metrics, sep = "."), lower_col = paste(varName, "min", sep = "."), upper_col = paste(varName, "max", sep = "."), Date = Date))

            if (ContData.env$myStats.missing.data.fill == TRUE) {
              timediff <- get_interval(tempData$Date)
              # print(timediff)
              timediff <- ifelse(timediff == "min", "15 mins", timediff)
              tempData <- as.data.frame(tempData %>%
                mutate(Date = as.POSIXct(Date)) %>%
                complete(Date = seq(min(Date, na.rm = TRUE), max(Date, na.rm = TRUE), by = timediff)))
            }

            tempData$lowerBoundgrp <- paste(varName, input$dailyStats_ts_metrics, ': minimum', sep = ".")
            tempData$upperBoundgrp <- paste(varName, input$dailyStats_ts_metrics, ': maximum', sep = ".")


            mainList[[paste(varName, input$dailyStats_ts_metrics, sep = ".")]] <- tempData
            # mainList[[paste(varName,input$dailyStats_ts_metrics, sep=".")]] <- as.data.frame(mainData %>% select(value=paste(varName,input$dailyStats_ts_metrics, sep="."), lower_col= paste(varName, "min", sep="."), upper_col=paste(varName, "max", sep="."), Date=Date))
          } 
        }
        main_range <- calculate_time_range(as.list(bind_rows(mainList, .id = "df")))
        mainBreaks <- main_range[[1]]
        main_x_date_label <- main_range[[2]]


        mainPlot <- prepareBasePlot(dataList = mainList, mapTitle = mainMapTitle, xDateLabel = main_x_date_label, xDateBrakes = mainBreaks)
        return(mainPlot)
      }
      
      #Tried geom ribbon for shading but at present it has issues with missing data so changed it to line graphs
      #in the future, geom_riboon could be a better option
      # geom_ribbon(show.legend=TRUE, aes(ymin=lower_col,ymax=upper_col,x=as.POSIXct(Date)), alpha=0.3,inherit.aes = FALSE)+

      prepareBasePlot <- function(dataList, mapTitle, xDateLabel, xDateBrakes) {
        mainPlot <- NULL
        tsData <- bind_rows(dataList, .id = "df")
        isMissingDate <- FALSE
        #print(tsData[complete.cases(tsData), ])
        #print(tsData)
        checkForMissingDate <- tsData[!complete.cases(tsData), ]
        if (nrow(checkForMissingDate) > 0) {
          isMissingDate <- TRUE
        }

        mainPlot <- ggplot(data = tsData, dynamicTicks = TRUE, aes(x=as.POSIXct(Date))) +
          labs(title = mapTitle, x = "Date", y = "Parameters")
        if (isMissingDate == TRUE) {
          mainPlot <- mainPlot +
            geom_line(aes(y=upper_col, color=factor(upperBoundgrp)))+
            geom_line(aes(y=value, colour=df))+
            geom_line(aes(y=lower_col, color=factor(lowerBoundgrp)))
        } else {
          mainPlot <- mainPlot +
            geom_ribbon(show.legend = TRUE, aes(ymin = lower_col, ymax = upper_col, x = as.POSIXct(Date)), alpha = 0.3, inherit.aes = FALSE) +
            geom_line(aes(y = value, colour = df))
        }

        mainPlot <- mainPlot +
          scale_x_datetime(date_labels = xDateLabel, date_breaks = xDateBrakes) +
          theme_bw() +
          scale_color_discrete(name = "") +
          facet_grid(df ~ ., scales = "free_y") +
          theme(
            strip.background = element_blank(),
            legend.title = element_blank()
            # ,strip.text.y = element_blank()
            , strip.placement = "outside",
            text = element_text(size = 10, face = "bold", color = "cornflowerblue"),
            plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 65, hjust = 1, vjust = 1)
          )
        return(mainPlot)
      }


      draw_uploaded_file_stats <- function() {
        basePlot <- NULL
        localStats <- dailyStats
        statsList <- localStats$stats

        mainMapTitle <- getMapTitle(input$dailyStats_shading, input$dailyStats_ts_title, lowerColumn = NULL, upperColumn = NULL)
        variable_to_plot <- input$dailyStats_ts_variable_name
        statsCols <- paste(variable_to_plot, input$dailyStats_ts_metrics, sep = ".")
        # shinyjs::alert(statsCols)

        mainData <- Reduce(full_join, statsList)
        
        
        if (ContData.env$myStats.missing.data.fill == TRUE) {
          timediff <- get_interval(mainData$Date)
          # print(timediff)
          timediff <- ifelse(timediff == "min", "15 mins", timediff)
          mainData <- as.data.frame(mainData %>%
                                    mutate(Date = as.POSIXct(Date)) %>%
                                    complete(Date = seq(min(Date, na.rm = TRUE), max(Date, na.rm = TRUE), by = timediff)))
          }
        
           
        
        myData <- mainData %>%
          select(statsCols, "Date") %>%
          gather(key = "parameter", value = "value", -Date)

        main_range <- calculate_time_range(as.list(myData))
        mainBreaks <- main_range[[1]]
        main_x_date_label <- main_range[[2]]

        basePlot <- ggplot(data = myData, aes(x = as.POSIXct(Date, format = "%Y-%m-%d"), y = value)) +
          geom_line(aes(colour = parameter)) +
          # scale_x_datetime(date_labels="%Y-%m-%d",date_breaks=paste0(1," month"))+
          scale_x_datetime(date_labels = main_x_date_label, date_breaks = mainBreaks) +
          labs(title = mainMapTitle, x = "Date", y = "Parameters") +
          theme_bw() +
          facet_grid(parameter ~ ., scales = "free_y") +
          theme(
            strip.background = element_blank(),
            strip.text.y = element_blank()
            # ,strip.text.y = element_text(angle = 35)
            , strip.placement = "outside",
            text = element_text(size = 10, face = "bold", color = "cornflowerblue"),
            plot.title = element_text(hjust = 0.5),
            legend.position = "bottom",
            axis.text.x = element_text(angle = 65, hjust = 10)
          )
        return(basePlot)
      }

      # common
      renderErrorMsg <- function(msg) {
        output$tsError <- renderUI({
          div(class = "alert alert-danger", msg)
        })
      }
      clearContents <- function() {
        output$tsError <- renderUI({})
      }

      clearPlot <- function() {
        output$display_time_series <- renderPlotly({
          plotly_empty()
        })
      }


      # NEW data feature is removed for now but saving the code here as it was part of this tab
      # This part will need to be converted to shiny modules. If needed Discrete data upload
      # can be used as an example to convert below part to a shiny module

      # observeEvent(input$dailyStats_shading,{
      #   if (input$dailyStats_shading == 'newData' | input$dailyStats_shading == 'discreteData'){
      #     shinyjs::show("cp_new_data")
      #   }else{
      #     shinyjs::hide("cp_new_data")
      #   }
      # })

      # uploaded_newData<-eventReactive(c(input$uploaded_newData_file),{
      #   my_data <- uploadFile(c(input$uploaded_newData_file), stopExecution = FALSE)
      #   my_data <- my_data[rowSums(is.na(my_data) | is.null(my_data) | my_data == "") != ncol(my_data),]
      #   return(my_data)
      # })
      # continuous Data Exploration with New File

      # observeEvent(input$uploaded_newData_file,{
      #   cols_avail <- colnames(uploaded_newData())
      #   fileContentForDisplay <- head(uploaded_newData())
      #   output$dateAndTimeBox <- renderUI({
      #
      #     div(id="dt_new",
      #         div(class = "panel panel-default",width = "100%",
      #             div(class = "panel-heading",
      #                 span("Step 2: Select Date and Time for new data", style="font-weight:bold;"),
      #                 span(
      #                   actionButton(inputId="dateTimeBoxButton_new",
      #                                style="float:right;", class="btn btn-primary btn-xs",
      #                                label="Hide Selection", icon= icon("arrow-down"))
      #                 )
      #             ),
      #             div(uiOutput("newDateAndTimeError"), style = "font-weight:bold;color:#b94a48;margin:5px;"),
      #             box(width="100%",class="displayed",id="dateBox_new",
      #                 div(
      #                   style = "margin-left:10px",
      #                   dateAndTimeUI(id = "newDataUpload", paramChoices = cols_avail, uploadedCols = cols_avail)
      #                 ),
      #                 fluidRow(
      #                   column(width = 4,
      #                          div(style="margin-left:15px;",
      #                              selectizeInput("newData_lower_col",
      #                                             label = "Select column to be used as lower bound",
      #                                             choices = c("",cols_avail),
      #                                             multiple = FALSE,
      #                                             selected = NULL,
      #                                             options = list(hideSelected = FALSE))
      #                          )),
      #                   column(width = 4,
      #                          selectizeInput("newData_upper_col",
      #                                         label = "Select column to be used as upper bound",
      #                                         choices = c("",cols_avail),
      #                                         multiple = FALSE,
      #                                         selected = NULL,
      #                                         options = list(hideSelected = FALSE))
      #                   )
      #                 ),
      #                 # fluidRow(
      #                 #   column(width=2, actionButton(inputId="display_new_data", label="Display",class="btn btn-primary")),
      #                 #   column(width=10, div(id="dummy"))
      #                 # ),
      #                 hr(style="margin:0px;padding:0px;"),
      #                 fluidRow(
      #                   column(width=12,
      #                          tags$div(
      #                            renderTable({
      #                              fileContentForDisplay
      #                            },type="html",bordered = TRUE,striped=TRUE,align="c"),style="overflow-x:auto;")
      #                   )
      #                 )
      #             )
      #         ) # end of box
      #     )
      #   })
      #   newDTvalues$newDateAndTime <- dateAndTimeServer(id = "newDataUpload", )
      # })
      #
      # display_new_data <- function(renderStatus=FALSE) {
      #   shinyjs::runjs("$('#newDateAndTimeErrorinnerDiv').remove()")
      #   newDTvalues$newDateAndTime <- dateAndTimeServer(id = "newDataUpload",uploaded_newData())
      #   newHomeDateAndTime <- newDTvalues$newDateAndTime
      #   mainPlot <- NULL
      #   if (newHomeDateAndTime$isTimeValid() & newHomeDateAndTime$isDateAndtimeValid() &
      #       validateNewLowerAndUpper("display_validation_msgs_new2") == FALSE) {
      #     tryCatch({
      #       new_raw_data <- getFormattedRawData(newHomeDateAndTime, uploaded_newData(), tabName = "", errorDivId = "newDateAndTimeError")
      #       if(nrow(new_raw_data) != nrow(new_raw_data[is.na(new_raw_data$date.formatted),])) {
      #         variable_to_plot <- newDTvalues$newDateAndTime$parmToProcess()
      #         mainList <- list()
      #         for(varName in variable_to_plot) {
      #           mainList[[varName]] <- as.data.frame(new_raw_data %>% select(value=all_of(varName), lower_col=input$newData_lower_col , upper_col=input$newData_upper_col, Date=date.formatted))
      #         }
      #         plotTitle <- ifelse((input$dailyStats_ts_title == ""), "New Data", input$dailyStats_ts_title)
      #         mainMapTitle <- getMapTitle("dynamic", plotTitle, lowerColumn = input$newData_lower_col, upperColumn = input$newData_upper_col)
      #         main_range = calculate_time_range(as.list(bind_rows(mainList, .id="df")))
      #         mainBreaks = main_range[[1]]
      #         main_x_date_label = main_range[[2]]
      #         mainPlot <- prepareBasePlot(dataList= mainList, mapTitle=mainMapTitle, xDateLabel=main_x_date_label, xDateBrakes= mainBreaks)
      #         if(!is.null(mainPlot) & length(newHomeDateAndTime$parmToProcess()) > 0 & renderStatus==FALSE){
      #           return(mainPlot)
      #         }else{
      #           shinyjs::runjs("$('#dateTimeBoxButton_new').click()")
      #           output$display_time_series_new <-  renderPlotly({
      #             ggplotly(mainPlot,height=calulatePlotHeight(length(newHomeDateAndTime$parmToProcess()) * 2)) %>% plotly::layout(legend = list(orientation = "h", x = 0.4, y = -0.4))
      #           })
      #         }
      #       }
      #       return(mainPlot)
      #     },error = function(parsingMsg) {
      #       processErrors(parsingMsg,elementId="newDateAndTimeError")
      #     }, warning = function(parsingMsg){
      #       processErrors(parsingMsg,elementId="newDateAndTimeError")
      #     }, message = function(parsingMsg) {
      #       processErrors(parsingMsg,elementId="newDateAndTimeError")
      #     }, finally = {
      #       return(mainPlot)
      #     })
      #   }
      # }


      # validateNewLowerAndUpper <- function(elementId){
      #   missingInputs <- FALSE
      #   if (input$newData_lower_col == "" | input$newData_upper_col == ""){
      #     missingInputs <- TRUE
      #     output[[elementId]] <- renderUI({
      #       shiny::validate(
      #         shiny::need(input$newData_lower_col != "", 'Please lower bound column.'),
      #         shiny::need(
      #           input$newData_upper_col != "", 'Please select upper bound column.'
      #         )
      #       )
      #     })
      #   }
      #   return(missingInputs)
      # }
    }
  ) # end of module server
}
