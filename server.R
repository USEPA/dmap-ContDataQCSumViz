#
# This is the server logic of ContDataSumViz Shiny web application. You can run
# the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# Notes:
# 1) If this is the first time to run this app, you need to install the required
#    R packages first
#    you can do that by typing this in your Console:
#    source("_moved/install_packages_for_app.R")
# 2) This App use several revised R functions from "ContDataQC" R package, but
#    the changes/edits may not be committed yet when you run the testing
#    versions, please remember to include the directory /update_ContDataQC/
#    under the same folder where you unzip this App package.
#    We don't need to source these R functions after we finalize them
#

##if(!require(ContDataQC)){source("_moved/install_packages_for_app.R")}  #install if not yet

# library("readxl")        # to read excel files
# library("writexl")
# library("data.table")
# library("DT")
# library("tidyverse")
# library("tibbletime")
# library("shiny")
# library("shinydashboard")
# library("shinyjs")
# library("shinyBS")
# library("shinythemes")
# library("shinyalert")
# library("conflicted")
# library("dataRetrieval")
# library("doBy")
# library("knitr")
# library("htmltools")
# library("rmarkdown")
# library("highr")
# library("survival")
# library("shinyFiles")
# library("plotly")
# library("zip")
# library("reshape2")
# library("ContDataQC")
# library("ContDataSumViz")
# library("StreamThermal")
# library("IHA")
# library("XLConnect")


## Moved to Global ***

# source("_moved/import_raw_data.R")
# source("update_ContDataQC/config.R")
# source("update_ContDataQC/CompSiteCDF.updated.R")
# source("update_ContDataQC/SumStats.updated.R")
#source("update_ContDataQC/ReportMetaData.R")
#
#
# options(shiny.maxRequestSize = 100*1024^2)


function(input, output, session) {
  
  #logf <- file.path(".","contDataSumViz.log")
  #open logfile
  #logfile <- log_open(logf)
  #log_print("I do log")
  useShinyjs()
  conflict_prefer("box", "shinydashboard")
  conflict_prefer("dataTableOutput", "DT")
  conflict_prefer("yday", "data.table")
  conflict_prefer("select", "dplyr")
  
  loaded_data <- reactiveValues()
  raw_data_columns<-reactiveValues()
  dateRange <- reactiveValues()
  workflowStatus <- reactiveValues(finish=FALSE)
  currentOutPutId <- reactiveValues()
  gageColNames  <- NULL
  daymetCols <- NULL
  consoleUSGS <- NULL
  selected_to_plot <- reactiveValues(all_selected=data.frame())
  processed <- reactiveValues(processed_dailyStats=list(),
                              ST.freq=data.frame(),
                              ST.mag=data.frame(),
                              ST.roc=data.frame(),
                              ST.tim=data.frame(),
                              ST.var=data.frame())
  gageRawData <- reactiveValues(gagedata = data.frame(),
                                gageColName = as.character())
  dayMetRawData <- reactiveValues(dayMetData = data.frame(),
                                  daymetColumns = as.character())
  
  formated_raw_data <- reactiveValues(derivedDF = data.frame(),
                                baseColNames = as.character())
  homeDTvalues <- reactiveValues(
    homeDateAndTime = list(),
    homeDateFormat = as.character()
  )
  
  metaHomeValues <- reactiveValues(
    metaVal = list()
  )
  
  discreteDTvalues <- reactiveValues(disDateAndTime = list())
  formatted_dis_data <- reactiveValues(derivedDF = list())
  
  newDTvalues <- reactiveValues(newDateAndTime = list())

  to_download <- reactiveValues()
  saveToReport <- reactiveValues(metadataTable=data.frame())
  
  

  #  Upload Data##############################
  #EWL
  if (file.exists("_moved/File_Format.rds")) file.remove("_moved/File_Format.rds")
  do.call(file.remove, list(list.files("Selected_Files", full.names = TRUE)))
 

  #Home page file upload
  uploaded_data<-eventReactive(c(input$uploaded_data_file),{
    
      
    
       my_data <- uploadFile(c(input$uploaded_data_file), stopExecution=TRUE, tab="homePage")
       # drop all rows where all the columns are empty
       my_data <- my_data[rowSums(is.na(my_data) | is.null(my_data) | my_data == "") != ncol(my_data),]

       if (length(my_data) > 0 ) {
         my_colnames <- colnames(my_data)
         shinyjs::show(id="displayidLeft")
         parmsToProcess <- fun.findVariableToProcess(my_colnames, getDateCols= FALSE)
         updateWorkFlowState("step1","success")
         shinyjs::show(id="dateTimeBoxButton")
         
         #goes to left panel
         output$display_runmetasummary <-
           renderUI({
             actionButton(inputId="runQS", label="Step 3: Run meta summary",class="btn btn-primary")
           })
         #goes to right panel
         output$display_raw_ts <- renderUI({
           div(id="mainBox",
               div(class = "panel panel-default",width = "100%",
                   div(class = "panel-heading",
                       span("Step 2: Select Date and Time", style="font-weight:bold;"),
                       span(
                         actionButton(inputId="dateTimeBoxButton", 
                                      style="float:right;", class="btn btn-primary btn-xs", 
                                      label="Hide Selection", icon= icon("arrow-down"))
                       )
                   ),
                   div(id="dateAndTimeError", style="padding:2px;"),
                   box(width="100%",class="displayed",id="dateBox",
                       div(
                         style = "margin-left:10px",
                         dateAndTimeUI(id = "homePage", paramChoices = parmsToProcess, uploadedCols = my_colnames)
                       ),
                       hr(style = "margin:0px;padding:0px;"),
                       fluidRow(
                         tagList(
                           div(
                             style = "padding:2px;",
                             span(actionButton(inputId = "showrawTS", label = "Display time series", class = "btn btn-primary"), style = "margin:5px 15px 5px 25px;"),
                             span("Note: Red border denotes required fields.", style = "font-weight:bold;color:#b94a48;")
                           )
                         )
                       ),
                       fluidRow(
                         div(uiOutput("contents"), style="overflow-x:auto;margin:0px 15px 0px 15px;")
                       ),
                   ) # end of box
               ), # end of parent div,
               fluidRow(column(
                 width = 12,
                 box(
                   width = "100%", id = "statsBox",
                   fluidRow(
                     column(
                       width = 12, style = "padding:20px;",
                       uiOutput("display_fill_data")
                     ), # column close
                   ) # fluidRow end
                 ) # end of statsBox
               )),
               fluidRow(
                 tags$div(
                   id = "display_all_raw_ts_div", style = "height:100%;width:100%;display:none;",
                   column(width = 12, plotlyOutput("display_all_raw_ts"))
                 ) # end of div
               ) # fluidRow end
           ) # end of mainBox
         })
       }
       
     return(my_data)
  })
  
  #init server modules
  observeEvent(uploaded_data(), {
    homeDTvalues$homeDateAndTime <- dateAndTimeServer(id = "homePage", uploaded_data())
    #shinyjs::runjs("$('#dateAndTimeErrorParent').remove()")
    #shinyjs::runjs("$('#metaDataHome-meta_footnote_text').empty()")
    #shinyjs::runjs("$('#metaDataHome-metaSummaryTb').empty()")
  })
  
  getFormattedRawData <- function(dateAndTimeFields, userData, tabName, errorDivId) {
    tryCatch(
      {
        shinyjs::runjs(paste0("$('#",errorDivId,"').text('')"))
        shinyjs::runjs(paste0("$('#",errorDivId,"').css('padding', '0px')"))
        shinyjs::removeClass(errorDivId, "alert alert-danger")
        userDataL <- fun.ConvertDateFormat(
          fun.userDateFormat = dateAndTimeFields$dateFormat(),
          fun.userTimeFormat = dateAndTimeFields$timeFormat(),
          fun.userTimeZone = dateAndTimeFields$timeZone(),
          fun.userDateFieldName = dateAndTimeFields$dateFieldName(),
          fun.userTimeFieldName = dateAndTimeFields$timeFieldName(),
          fun.rawData = userData,
          fun.date.org = dateAndTimeFields$dateColumnNums()
        )
      },
      error = function(parsingMsg) {
        print(parsingMsg)
      },
      warning = function(parsingMsg) {
        #, id=paste0(errorDivId, "innerDiv")
        processedMsg <- prepareDateFormatErrorMsg(parsingMsg, tab = tabName)
        shinyjs::runjs(paste0("$('#",errorDivId,"').text('",processedMsg,"')"))
        shinyjs::addClass(errorDivId, "alert alert-danger")

      },
      message = function(parsingMsg) {
        print(parsingMsg)
      }
    )
    return(userDataL)
  }


  ## Copy uploaded files to local folder
  # observeEvent(input$uploaded_data_file,{
  #   
  #   shinyjs::runjs("$('#display_validation_msgs').empty()")
  #   my_data <- uploaded_data()
  #   output$display_raw_ts <- renderUI({
  # 
  #     if (length(my_data) > 0 ) {
  #       my_colnames <- colnames(my_data)
  #       shinyjs::show(id="displayidLeft")
  #       parmsToProcess <- fun.findVariableToProcess(my_colnames, getDateCols= FALSE)
  #       shinyjs::runjs("$('#quick_summary_table').empty()")
  #       shinyjs::runjs("$('#quick_summary_table_footnote').empty()")
  #       updateWorkFlowState("step1","success")
  #       shinyjs::show(id="dateTimeBoxButton")
  #       
  #       #goes to left panel
  #       output$display_runmetasummary <-
  #         renderUI({
  #             actionButton(inputId="runQS", label="Step 3: Run meta summary",class="btn btn-primary")
  #         })
  #       
  #       div(id="mainBox",
  #       div(class = "panel panel-default",width = "100%",
  #           div(class = "panel-heading",
  #             span("Step 2: Select Date and Time", style="font-weight:bold;"),
  #             span(
  #               actionButton(inputId="dateTimeBoxButton", 
  #                            style="float:right;", class="btn btn-primary btn-xs", 
  #                            label="Hide Selection", icon= icon("arrow-down"))
  #               )
  #           ),
  #           box(width="100%",class="displayed",id="dateBox",
  #           dateAndTimeCommonBox(
  #                                dateColumnNumsId="dtNumOfCols",
  #                                parmToProcessId="parameters_to_process2",
  #                                dateFieldNameId="selectedDateFieldName", 
  #                                dateFormatId="selectedDateFormat", 
  #                                timeFieldNameId="selectedTimeFieldName", 
  #                                timeFormatId="selectedTimeFormat",
  #                                timeZoneId="selectedTimeZone",
  #                                validationDivId="display_validation_msgs",
  #                                timeFieldParentId="timeFieldDiv",
  #                                paramChoices=parmsToProcess,
  #                                ),
  #           hr(style="margin:0px;padding:0px;"),
  #           fluidRow(
  #             div(width="85%", actionButton(inputId="showrawTS", label="Display time series",class="btn btn-primary"),style="margin:5px 15px 5px 25px;")
  #           ),
  #           fluidRow(
  #             div(uiOutput("contents"), style="overflow-x:auto;margin:0px 15px 0px 15px;")
  #           ),
  #           ) # end of box
  #       ), # end of parent div,
  # 
  #           fluidRow(column(width=12,
  #                   box(width="100%", id="statsBox", 
  #                    tags$head(tags$style(HTML("#quick_summary_table {
  #                                                     text-align: center;
  #                                                     font-size: 16px;
  #                                                     font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
  #                                                     }
  #                                                     "))),
  #                     fluidRow(column(width=12,uiOutput("display_quick_summary_table")), # column close
  #                              column(width=12,uiOutput("display_footnote_text"),class="text-info fs-6")
  #                             ), #fluidRow end
  #                     fluidRow(column(width=8,style="padding:20px;",
  #                               uiOutput("display_fill_data"),
  #                               uiOutput("display_checkBoxes_dailyStats_1"),
  #                               uiOutput("display_radioButtons_dailyStats_2")
  #                            ), # column close
  #                            #column(width=4,style="padding:25px;",uiOutput("display_actionButton_calculateDailyStatistics")
  #                            #), # column close
  #                            #column(width=4,div(style="margin-bottom:20px")),
  #                            #column(width=4,style="padding:30px;",uiOutput("display_actionButton_saveDailyStatistics")
  #                            #), # column close
  #                    ) #fluidRow end
  #                   ) # end of statsBox
  #               )
  #           ),
  #           fluidRow(
  #             tags$div(id="display_all_raw_ts_div", style="height:100%;width:100%;display:none;", 
  #                      column(width=12,plotlyOutput("display_all_raw_ts"))
  #             ) #end of div
  #           ) #fluidRow end
  #       ) # end of main div
  #     }
  #   })
  #   
  #   my_data_continuous <- my_data %>% dplyr::select(where(is.numeric))
  #   all_continuous_col_names <- colnames(my_data_continuous)
  #   uploadedCols <- colnames(my_data)
  # 
  # 
  #   updateSelectInput(session,"selectedDateFieldName",choices = c("", uploadedCols),selected="")
  #   updateSelectInput(session,"selectedTimeFieldName",choices = c("", uploadedCols),selected="")
  #   updateSelectizeInput(session, 'rawDataToPlot', choices = c("",all_continuous_col_names), server = TRUE)
  #   #updateSelectizeInput(session, 'parameters_to_process2', choices = c("",all_continuous_col_names), server = TRUE)
  # 
  # })  # observeEvent end
  
  observeEvent(input$dateTimeBoxButton,{
    hideShowDateTimeBox("dateTimeBoxButton")
  })
  observeEvent(input$dateTimeBoxButton_discrete,{
    hideShowDateTimeBox("dateTimeBoxButton_discrete")
  })
  
  observeEvent(input$dateTimeBoxButton_new,{
    hideShowDateTimeBox("dateTimeBoxButton_new")
  })
  
  hideShowDateTimeBox <- function(buttonId) {
    boxId <- "dateBox"
    if(grepl( "_new", buttonId, fixed = TRUE)) {
      boxId <- "dateBox_new"
    } else if(grepl( "discrete", buttonId, fixed = TRUE)) {
      boxId <- "dateBox_discrete"
    }
    if(input[[buttonId]] %% 2 == 1){
      shinyjs::hide(id = boxId)
      updateActionButton(session, buttonId, icon = icon("arrow-up"), label="Show Selection")
    }else{
      shinyjs::show(id = boxId)
      updateActionButton(session, buttonId, icon = icon("arrow-down"),label="Hide Selection")
    }
  }
  

  ## Load all the uploaded files to a list, this feature will be activated in the future
  # datasetlist <- eventReactive(input$uploadId,{
  #
  #   Selected_Files <- list.files("Selected_Files/")
  #   Sys.sleep(2)
  #   File_Format <- readRDS("File_Format.rds")
  #   datalist <- list()
  #   datalist <- lapply(1:length(File_Format[[1]]), function(d) read.csv(paste0("Selected_Files/",File_Format$file[d] ),
  #                                                                       header = File_Format$header[d],
  #                                                                       sep = File_Format$sep[d],
  #                                                                       dec = File_Format$dec[d],
  #                                                                       check.names = FALSE,
  #                                                                       quote = File_Format$quote[d]))
  #   names(datalist) <- paste(File_Format$index, File_Format$file,sep = ". ")
  #   return(datalist)
  #
  # })

  # output$manage <- renderUI({
  #   data <- uploaded_data() ## datasetlist()
  #   print(length(data))
  #   selectInput("dataset", "Dataset", choices = loaded_data$name, selected = loaded_data$name)  ## names(data) if use datasetlist()
  # })
  # 
  # output$siteType <- renderUI({
  #   data <- uploaded_data() ## datasetlist()
  #   selectInput("siteType_input",label="Single site or multiple sites",
  #               choices = c("Single site","Multiple sites"),
  #               selected = "Single site")
  # })
  
  output$displayFC <- renderUI({
    
    data <- uploaded_data() ## datasetlist()
    tagList(
    hr(),
    radioButtons("disp", "Display file information", choices = c(Head = "head",Tail="tail",ColumnNames="Column names"), inline = TRUE,
                 selected = "head"),
    actionButton(inputId="displayidLeft",label = "Display file contents", class="btn btn-primary")
    )
  })

  observeEvent(input$displayidLeft, {
    output$contents <- renderTable(
      {
        sub_df <- uploaded_data()
        if (isolate(input$disp == "head")) {
          return(head(sub_df))
        } else if (isolate(input$disp == "tail")) {
          return(tail(sub_df))
        } else {
          return(colnames(sub_df))
        }
      },
      type = "html",
      bordered = TRUE,
      striped = TRUE,
      align = "c",
      width = "100%"
    )
  })

  # myQuickSummary <- function(myDf){
  #   # print(str(myDf$Date))
  #   # print(class(myDf$Date))
  #   all.days <- seq.Date(min(myDf$Date,  na.rm = TRUE),max(myDf$Date, na.rm = TRUE),by="day")
  #   N.missing.days <- (length(all.days)-sum(all.days %in% myDf$Date))+sum(myDf$sumNA>0)
  #   if (ncol(myDf)>2){
  #   N.days.flagged.fail <- sum(myDf$sumFail>0)
  #   N.days.flagged.suspect <- sum(myDf$sumSuspect>0)
  #   N.days.flagged.noFlagData <- sum(myDf$SumNoFlagData>0)
  #   }else{
  #     N.days.flagged.fail <- "No flag field found"
  #     N.days.flagged.suspect <- "No flag field found"
  #     N.days.flagged.noFlagData <- "No flag field found"
  #   }
  #   mySummary <- c(N.missing.days,N.days.flagged.fail,N.days.flagged.suspect,N.days.flagged.noFlagData)
  #   return(mySummary)
  # }
  

  observeEvent(input$showrawTS,{

    # output$display_all_raw_ts <- renderUI({
    #   withSpinner(plotlyOutput(outputId="all_raw_ts"))
    # })
    shinyjs::show(id="display_all_raw_ts_div")
    raw_data <- uploaded_data()
    homeDTvalues$homeDateAndTime <- dateAndTimeServer(id = "homePage", uploaded_data())
    showRawDateAndTime <- homeDTvalues$homeDateAndTime
    
    #display_validation_msgs dateBox
    if (showRawDateAndTime$isTimeValid() & showRawDateAndTime$isDateAndtimeValid()) {
      tryCatch({
        my_raw_choices = showRawDateAndTime$parmToProcess()
        # if error had occured then on fix reset the step
        shinyjs::removeClass("step3", "btn-danger")
        shinyjs::addClass("step3", "btn-primary")
        
        #shinyjs::runjs("$('#display_validation_msgs').empty()")
        formated_raw_data$derivedDF <- getFormattedRawData(showRawDateAndTime, raw_data, tabName = "homePage", errorDivId = "dateAndTimeError")
        raw_data <- formated_raw_data$derivedDF
        #print(formated_raw_data$derivedDF)
    
        if (!is.null(my_raw_choices) & nrow(raw_data) != nrow(raw_data[is.na(raw_data$date.formatted),])){
          timediff <- get_interval(raw_data$date.formatted)
          timediff <- ifelse(timediff == "min", "15 mins", timediff)
             uploaded_raw_data  <- raw_data %>% 
               mutate(date.formatted = as.POSIXct(date.formatted)) %>%
               complete(date.formatted = seq(min(date.formatted,na.rm = TRUE), max(date.formatted, na.rm = TRUE), by=timediff)) %>%
               select(c(showRawDateAndTime$parmToProcess()), "Date" = c(date.formatted)) %>%
               gather(key = "parameter", value = "value", -Date)
              
              main_range = calculate_time_range(as.list(uploaded_raw_data))
              mainBreaks = main_range[[1]]
              main_x_date_label = main_range[[2]]
              
              p <- ggplot(data = uploaded_raw_data, aes(x=as.POSIXct(Date,format="%Y-%m-%d"), y = value)) +
                geom_line(aes(colour=parameter)) +
                labs(title="Uploaded data", x="Date", y="Parameters")+
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
               
                 output$display_all_raw_ts <- renderPlotly({
                  ggplotly(p, height = calulatePlotHeight(length(my_raw_choices) * 2)) %>% plotly::layout(legend = list(orientation = "h", x = 0.4, y = -0.3))
                })
                overridePotlyStyle("display_all_raw_ts")
                #click the button to hide the selection box
                shinyjs::runjs("$('#dateTimeBoxButton').click()")
                shinyjs::runjs("$('html, body').animate({scrollTop: $(document).height()},2000)")
              } else {
                shinyAlertUI("common_alert_msg" , invalidDateFormt, "ERROR")
              }
       
    
      },error = function(parsingMsg) {
        output$display_validation_msgs <- renderUI({
          prepareDateFormatErrorMsg(parsingMsg, tab="homePage")
        })
      }, warning = function(parsingMsg){
        output$display_validation_msgs <- renderUI({
          prepareDateFormatErrorMsg(parsingMsg, tab="homePage")
        })
      }, message = function(parsingMsg) {
        output$display_validation_msgs <- renderUI({
          prepareDateFormatErrorMsg(parsingMsg, tab="homePage")
        })
      }) #end of tryCatch
    } #end of validation check
  })  ## observeEvent end

  observeEvent(input$runQS,{
    
    #shinyjs::runjs("$('#display_validation_msgs').empty()")
    tryCatch({
      #shinyjs::runjs("$('#dateAndTimeErrorinnerDiv').remove()")
      raw_data <- uploaded_data()
      homeDTvalues$homeDateAndTime <- dateAndTimeServer(id = "homePage", uploaded_data())
      localHomeDateAndTime <- homeDTvalues$homeDateAndTime
      #It does not stop user, just warns them
      #validateSiteId(raw_data)
      #display_validation_msgs dateBox
      if (localHomeDateAndTime$isTimeValid() & localHomeDateAndTime$isDateAndtimeValid()) {
         # output$display_quick_summary_table <- renderUI({
         #    column(12, align = "center", withSpinner(tableOutput("quick_summary_table")))
         #  })
          # update the reactiveValues
          raw_data <- getFormattedRawData(localHomeDateAndTime, raw_data, tabName = "homePage", errorDivId = "dateAndTimeError")
          #now shorten the varname
          if ("date.formatted" %in% colnames(raw_data) & !is.null(localHomeDateAndTime$parmToProcess()) & nrow(raw_data) != nrow(raw_data[is.na(raw_data$date.formatted), ])) {
             print("passed fun.ConvertDateFormat")
            
             formated_raw_data$derivedDF <- raw_data
             # now shorten the varname
             raw_data <- formated_raw_data$derivedDF
              metaHomeValues$metaVal <-  metaDataServer("metaDataHome", localHomeDateAndTime$parmToProcess(), formatedUploadedData=raw_data, uploadData=uploaded_data())
              raw_data_columns$date_column_name = "date.formatted"
              output$display_fill_data <- renderUI({
                metaDataUI("metaDataHome")
              })
          
          output$display_actionButton_calculateDailyStatistics <-
            renderUI({
              actionButton(inputId = "calculateDailyStatistics",
                           label = "Step 4: Calculate daily statistics",
                           class="btn btn-primary"
                           )
            })
          
          ## change the actionButton to downloadButton
          output$display_actionButton_saveDailyStatistics <- renderUI({
            downloadButton(outputId = "saveDailyStatistics",
                           label = "Save daily statistics",
                           class="btn btn-primary",
                           style = "padding-left:15px;padding-right:15px;display:none;")
          })
          
            #click the button to hide the selection box
            shinyjs::runjs("$('#dateTimeBoxButton').click()")
            if(workflowStatus$finish==FALSE) {
              updateWorkFlowState("step3", "success")
            }
           
          } else {
            #shinyAlertUI("common_alert_msg" , invalidDateFormt, "ERROR")
            print("it should have updated users on the UI")
          }     
        }
    },error = function(parsingMsg) {
      output$errorMsg <- renderUI({
        print(parsingMsg)
        prepareDateFormatErrorMsg(parsingMsg, tab="homePage")
      })
    }, warning = function(parsingMsg){
      output$errorMsg <- renderUI({
        print(parsingMsg)
        prepareDateFormatErrorMsg(parsingMsg, tab="homePage")
      })
    }, message = function(parsingMsg) {
      output$errorMsg <- renderUI({
        print(parsingMsg)
        prepareDateFormatErrorMsg(parsingMsg, tab="homePage")
      })
    })

  })  ## observeEvent end

  ## close the warning messages inside the above oberveEvent

  observeEvent(input$get_the_year,{
    #print(input$alert_no_date)
    shinyjs::runjs("swal.close();")
  })
  
  ### when user clicked actionButton "calculateDailyStatistics"

  observeEvent(input$calculateDailyStatistics,{
    tryCatch({
    raw_data <- uploaded_data()
    
    withProgress(message = paste("Calculating the daily statistics"), value = 0, {
      incProgress(0, detail = "now... ")
    raw_data <- formated_raw_data$derivedDF
    #print(head(raw_data)) 
    ##print(formated_raw_data$derivedDF)
    dateRange$min <- min(as.Date(raw_data$date.formatted), na.rm = TRUE)
    dateRange$max <- max(as.Date(raw_data$date.formatted), na.rm = TRUE)
    variables_to_calculate <- homeDTvalues$homeDateAndTime$parmToProcess()
    raw_data_columns$date_column_name = "date.formatted"
    ## how to handle "fail" or "suspect" measurements
    print("ng test calculateDaily")
    print(metaHomeValues$metaVal$fillMissingData2())
    print(metaHomeValues$metaVal$exclude_flagged2())
    print(metaHomeValues$metaVal$how_to_save2())

    if (is.null(metaHomeValues$metaVal$fillMissingData2())) {
      ContData.env$myStats.Fails.Exclude <- FALSE
      ContData.env$myStats.Suspects.Exclude <- FALSE
    }
    if ("fail" %in% metaHomeValues$metaVal$fillMissingData2()) {
      print(paste0("check the exclude flagged choices are:", metaHomeValues$metaVal$fillMissingData2()))
      ContData.env$myStats.Fails.Exclude <- TRUE
    } else {
      ContData.env$myStats.Fails.Exclude <- FALSE
    }
    if ("suspect" %in% metaHomeValues$metaVal$fillMissingData2()) {
      print(paste0("check the exclude flagged choices are:", metaHomeValues$metaVal$fillMissingData2()))
      ContData.env$myStats.Suspects.Exclude <- TRUE
    } else {
      ContData.env$myStats.Suspects.Exclude <- FALSE
    }
    if ("flag not known" %in% metaHomeValues$metaVal$fillMissingData2()) {
      # do nothing, logic is not there in the sumStts.updated function
      print("Just for testing")
    }
     
     
     # Fill missing data
     ContData.env$myStats.missing.data.fill = metaHomeValues$metaVal$fillMissingData2()

     dailyStats <- SumStats.updated(fun.myFile=NULL
                                   ,fun.myDir.import=NULL
                                   ,fun.myParam.Name=variables_to_calculate
                                   ,fun.myDateTime.Name=raw_data_columns$date_column_name
                                   ,fun.myDateTime.Format="%Y-%m-%d %H:%M:%S"
                                   ,fun.myThreshold=20
                                   ,fun.myConfig=""
                                   ,df.input=raw_data
                                   )
      processed$processed_dailyStats <- dailyStats
      shinyjs::show(id="saveDailyStatistics")
      updateWorkFlowState("step4", "success")
      updateWorkFlowState("step5", "success")
      incProgress(1/1, detail = "Calculated the daily statistics")
    })
    } ,error = function(parsingMsg) {
      updateWorkFlowState("step4", "error")
      print(parsingMsg$message)
    })
  
  })

  output$saveDailyStatistics <- downloadHandler(
    
    filename = function(){
      name_in_file <- loaded_data$name
      if (endsWith(loaded_data$name,".csv")) name_in_file <- sub(".csv$","",loaded_data$name)
      if (endsWith(loaded_data$name,".xlsx")) name_in_file <- sub(".xlsx$","",loaded_data$name)
  
      if (metaHomeValues$metaVal$how_to_save2() == "save2") {
        paste0("saved_dailyStats_", name_in_file, "_dailyStats.csv")
      } else if (metaHomeValues$metaVal$how_to_save2() == "save1") {
        paste0("saved_dailyStats_", name_in_file, ".zip")
      } else if (metaHomeValues$metaVal$how_to_save2() == "save4") {
        paste0("saved_dailyStats_wqx_", name_in_file, ".csv")
      }
    },

    content = function(file){
      if (metaHomeValues$metaVal$how_to_save2() == "save2") {
        combined_data <- Reduce(full_join, processed$processed_dailyStats)
        write.csv(combined_data, file, row.names = FALSE)
      } else if (metaHomeValues$metaVal$how_to_save2() == "save4") {
        wqxData <- Reduce(full_join, processed$processed_dailyStats)
        wqxData <- wqxData %>%
          gather(key = "CharacteristicName", value = "Value", -Date)
        write.csv(wqxData, file, row.names = FALSE)
      } else if (metaHomeValues$metaVal$how_to_save2() == "save1") {
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        files <- NULL
        for (i in 1:length(processed$processed_dailyStats)) {
          name_i <- names(processed$processed_dailyStats)[i]
          print(name_i)
          filename <- paste0("saved_dailyStats_", loaded_data$name, "_", name_i, "_dailyStats.csv")
          write.csv(processed$processed_dailyStats[[i]], filename, row.names = FALSE)
          files <- c(filename, files)
        }
        zip::zip(file, files)
      }
    }

  )
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Data Exploration####
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  observeEvent(input[["tabset"]], {
    ### DE, ALL, summary table ####

    output$summary_table_input_1 <- renderUI({
      variables_avail <- names(processed$processed_dailyStats)
      selectizeInput("summarise_variable_name",
                     label ="Select variable name",
                     choices=variables_avail,
                     multiple = FALSE,
                     selected=variables_avail[1],
                     options = list(hideSelected = FALSE))
    })

    output$summary_table_input_2 <- renderUI({
      radioButtons("summarise_by", "Summarise by", choices = c("year/month"="year/month"
                                                               ,"year"="year"
                                                               ,"year/season"="year/season"
                                                               ,"season"="season"),
                   selected = "year/month")
    })

    output$summary_table_input_3 <- renderUI({
      selectizeInput("summarise_metrics",label ="Select metrics",
                     choices=c("mean","median","min", "max","range","sd","var","cv","n"),
                     multiple = FALSE,
                     selected="mean",
                     options = list(hideSelected = FALSE))
    })

    output$summary_table_input_4 <- renderUI({
      actionButton(inputId="display_table", label="Summarise",class="btn btn-primary")
    })


    ## DE, ALL, time series plot" << All parameters ####

    output$time_series_input_1 <- renderUI({
      variables_avail <- names(processed$processed_dailyStats)
      selectizeInput("dailyStats_ts_variable_name",label ="Select variable name",
                     choices=variables_avail,
                     multiple = TRUE,
                     selected=variables_avail[1],
                     options = list(hideSelected = FALSE))
    })

    output$time_series_input_2 <- renderUI({
      selectizeInput("dailyStats_ts_metrics",label ="Select daily statistics metrics",
                     choices=c("mean","median","min", "max","range","sd","var","cv","n"),
                     multiple = FALSE,
                     selected="mean",
                     options = list(hideSelected = FALSE))
    })

    output$time_series_input_3 <- renderUI({
      div(
          radioButtons("dailyStats_shading", "Add shading with", choices = c("25th & 75th percentiles"="quantiles",
                                                                           "Mminimum & Maximum"="minMax"),
                       selected = "quantiles"))

    })

    output$time_series_input_4 <- renderUI({
      textInput(inputId="dailyStats_ts_title", label="Plot title",value="")
    })

    output$time_series_input_5 <- renderUI({
      tagList(
      actionButton(inputId="display_ts",label="Display",class="btn btn-primary"),
     # actionButton(inputId="display_subplot_ts", label="Display Subplots",class="btn btn-primary"),
      )
    })

    output$testing2 <- renderUI({
      div("coming soon")
    })

    #USGS Gage
    output$gage_panel <- renderUI({
      div(class="panel panel-default", style="padding:10px;",
          div(class = "panel-heading", style="padding:10px 5px 10px 10px;",
              span("USGS Gage data", style="font-weight:bold;"),
              a("View Gage Ids", href="https://waterdata.usgs.gov/nwis/rt", target="_blank", style="float:right")
             ),
          div(style="padding:5px;",
            textInput(inputId="gage_id", label="Gage Id",value=""),
            actionButton(inputId="display_gage_ts", label="Download USGS gage data",class="btn btn-primary")
          ),
          div(id="gageVarsDiv" , style="padding:5px;display:none",
            selectizeInput("gaze_params", label ="Select USGS gage variables",
                              choices=gageColNames,
                              multiple = TRUE,
                              selected=NULL,
                              options = list(hideSelected = FALSE)),
            actionButton(inputId="display_gage_raw", label="View USGS raw data",class="btn btn-primary")
          )
      )
    })
    
   output$base_gage_daymet_panel <- renderUI({
      variables_avail <- homeDTvalues$homeDateAndTime$parmToProcess()
      div(class="panel panel-default", style="padding:10px;",
          div(class = "panel-heading", style="padding:10px 5px 10px 10px;",
              span("View Base, Gage and DayMet data merged in a subplot", style="font-weight:bold;", icon("info-circle", style = "color: #2fa4e7", id="baseDataDef"))
          ),
          bsPopover(id="baseDataDef", title="What is base data\\?", content = "Base data is uploaded on the \\'Upload Data\\' tab.", 
                    placement = "right", trigger = "hover"),
          div(style="padding:5px;",
              selectizeInput("dailyStats_ts_variable_name2",label ="Select base variable names",
                             choices=variables_avail,
                             multiple = TRUE,
                             selected=variables_avail[1],
                             options = list(hideSelected = FALSE)),
              actionButton(inputId="display_subplot_ts", label="View Base, Gage and DayMet Merged",class="btn btn-primary")
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
        textInput(inputId="daymet_lat", label="Site Latitude",value=""),
        textInput(inputId="daymet_long", label="Site Longitude",value=""),
        actionButton(inputId="get_daymet_data", label="Download Daymet data",class="btn btn-primary")
        ),
        div(id="daymetVarsDiv", style="padding:5px;display:none",
        selectizeInput("daymet_params", label ="Select Daymet variables",
                       choices=daymetCols,
                       multiple = TRUE,
                       selected=daymetCols[1],
                       options = list(hideSelected = FALSE)),
        actionButton(inputId="display_daymet_raw", label="View Daymet raw data",class="btn btn-primary")
        )
     ) #end of parent div
    })
    #end of Daymet
    
    tagList(
      sliderInput("n", "N", 1, 1000, 500),
      textInput("label", "Label")
    )


    ## DE, ALL, time series - annual overlays" << All parameters ############

    output$time_series_overlay_input_1 <- renderUI({
      variables_avail <- names(processed$processed_dailyStats)
      selectizeInput("dailyStats_ts_overlay_variable_name",label ="Select variable name",
                     choices=variables_avail,
                     multiple = FALSE,
                     selected=variables_avail[1],
                     options = list(hideSelected = FALSE))
    })

    output$time_series_overlay_input_2 <- renderUI({

      selectizeInput("dailyStats_ts_overlay_metrics",label ="Select daily statistics metrics",
                     choices=c("mean","median","min", "max","range","sd","var","cv","n"),
                     multiple = FALSE,
                     selected="mean",
                     options = list(hideSelected = FALSE))
    })

    output$time_series_overlay_input_3 <- renderUI({
      textInput(inputId="dailyStats_ts_overlay_title", label="Plot title",value="")
    })

    output$time_series_overlay_input_4 <- renderUI({
          radioButtons("overlay_shading", "Add shading with", choices = c("none"="none"
                                                                          ,"overall minimum and maximum(all years)"="overall"
                                                                          ),
                       selected = "none")

    })

    output$time_series_overlay_input_5 <- renderUI({
      actionButton(inputId="display_ts_overlay", label="Display",class="btn btn-primary")
    })

    ############ DE, ALL, box plots" << All parameters ############

    output$box_input_1 <- renderUI({
      variables_avail <- names(processed$processed_dailyStats)
      selectizeInput("boxplot_variable_name",label ="Select variable name",
                     choices=variables_avail,
                     multiple = FALSE,
                     selected=variables_avail[1],
                     options = list(hideSelected = FALSE))
    })

    output$box_input_2 <- renderUI({

      selectizeInput("boxplot_metrics",label ="Select daily statistics metrics",
                     choices=c("mean","median","min", "max","range","sd","var","cv","n"),
                     multiple = FALSE,
                     selected="mean",
                     options = list(hideSelected = FALSE))
    })

    output$box_input_3 <- renderUI({
      div(br(),
          radioButtons("box_group", "Group by", choices = c("month"="month"
                                                            ,"month(years side by side)"="month2"
                                                            ,"year"="year"
                                                            ,"season"="season"
                                                            ,"season(years side by side)"="season2"),
                       selected = "month"))

    })

    output$box_input_4 <- renderUI({
      textInput(inputId="box_title", label="Plot title",value="")
    })


    output$box_input_5 <- renderUI({
      actionButton(inputId="display_box", label="Display",class="btn btn-primary")
    })


    ############ DE, ALL, CompSiteCDF" << All parameters ############

    output$CDF_input_1 <- renderUI({
      variables_avail <- names(processed$processed_dailyStats)
      selectizeInput("CDF_variable_name",label ="Select variable name",
                     choices=variables_avail,
                     multiple = FALSE,
                     selected=variables_avail[1],
                     options = list(hideSelected = FALSE))
    })

    output$CDF_input_2 <- renderUI({
      div(br(),
          radioButtons("CDF_shading", "Add shading with", choices = c("25th & 75th percentiles"="quantiles",
                                                                    "minimum & maximum"="minMax"
                                                                    ),
                       selected = "minMax"))

    })

    output$CDF_input_3 <- renderUI({
          myList <- processed$processed_dailyStats
          variable_to_plot <- input$CDF_variable_name
          myData.all <- myList[[which(names(myList)==variable_to_plot)]]
          myData.all[,"year"] <- format(myData.all[,"Date"],"%Y")

          selectizeInput("CDF_select_year",label ="Select year",
                         choices=c("All", unique(myData.all$year)),
                         multiple = FALSE,
                         selected = "All",
                         options = list(hideSelected = FALSE))
    })

    output$CDF_input_4 <- renderUI({

      selectizeInput("CDF_select_season",label ="Select season",
                     choices=c("All","Fall", "Winter", "Spring","Summer" ),
                     multiple = FALSE,
                     selected = "All",
                     options = list(hideSelected = FALSE))
    })

    output$CDF_input_5 <- renderUI({

      textInput(inputId="CDF_title", label="Plot title",value="")

    })

    output$display_CDF_button <- renderUI({
      actionButton(inputId="run_CDF", label="Run and display",class="btn btn-primary")
    })

    ############  DE, ALL, Raster graphs" << All parameters ############

    output$raster_input_1 <- renderUI({
      variables_avail <- names(processed$processed_dailyStats)
      selectizeInput("dailyStats_raster_variable_name",label ="Select variable name",
                     choices=variables_avail,
                     multiple = FALSE,
                     selected=variables_avail[1],
                     options = list(hideSelected = FALSE))
    })

    output$raster_input_2 <- renderUI({

      selectizeInput("dailyStats_raster_metrics",label ="Select daily statistics metrics",
                     choices=c("mean","median","min", "max","range","sd","var","cv","n"),
                     multiple = FALSE,
                     selected="mean",
                     options = list(hideSelected = FALSE))
    })

    output$raster_input_3 <- renderUI({
      textInput(inputId="dailyStats_raster_title", label="Plot title",value="")

    })

    output$raster_input_4 <- renderUI({
      numericInput(inputId="raster_plot_aspect_ratio", label="Adjust plot aspect ratio",0.5,min=0,max=10,step=0.1)

    })

    output$raster_input_5 <- renderUI({
      radioButtons(inputId="raster_plot_color",label ="Color palette options",choices=c("hcl","rainbow","heat","terrain","topo"),selected="hcl",inline=FALSE)
    })

    output$raster_input_6 <- renderUI({
      actionButton(inputId="run_raster", label="Display",class="btn btn-primary")
    })



    ############  DE, ALL, Thermal Statistics" << Temperature ############
    output$thermal_input_1 <- renderUI({
      variables_avail <- names(uploaded_data())
      site_keys_in_favor_order <- c("Site","SITE","SiteID","SITEID")
      possible_site_columns <- site_keys_in_favor_order[site_keys_in_favor_order %in% variables_avail]
      if (length(possible_site_columns)==0){
        site_to_select <- variables_avail[grep('site',variables_avail,ignore.case=TRUE)][1]
      }else{
        site_to_select <- possible_site_columns[1]
      }
      selectizeInput("thermal_SiteID_name",label ="Select SiteID Column",
                     choices=variables_avail,
                     multiple = FALSE,
                     selected=site_to_select,
                     options = list(hideSelected = FALSE))
    })

    # output$thermal_input_2 <- renderUI({
    #   variables_avail <- names(uploaded_data())
    #   date_keys_in_favor_order <- c("Date.Time","DATE.TIME","Year","YEAR","Date","DATE","MonthDay")
    #   possible_date_columns <- date_keys_in_favor_order[date_keys_in_favor_order %in% variables_avail]
    #   selectizeInput("thermal_Date_name",label ="Select Date Column",
    #                  choices=variables_avail,
    #                  multiple = FALSE,
    #                  selected=possible_date_columns[1],
    #                  options = list(hideSelected = FALSE))
    # })

    output$thermal_input_3 <- renderUI({
      variables_avail <- names(uploaded_data())
      temp_keys_in_favor_order <- c("Water.Temp.C","WATER.TEMP.C","Water_Temp_C",
                                    "WATER_TEMP_C","Air.Temp.C","AIR.TEMP.C","Air_Temp_C","AIR_TEMP_C")
      possible_temp_columns <- temp_keys_in_favor_order[temp_keys_in_favor_order %in% variables_avail]
      if (length(possible_temp_columns)==0){
        temp_to_select <- variables_avail[grep('temp',variables_avail,ignore.case=TRUE)][1]
      }else{
        temp_to_select <- possible_temp_columns[1]
      }
      selectizeInput("thermal_Temp_name",label ="Select Temperature Column",
                     choices=variables_avail,
                     multiple = FALSE,
                     selected= temp_to_select,
                     options = list(hideSelected = FALSE))
    })

    output$display_run_thermal_button <- renderUI({
      actionButton(inputId="display_thermal", label="Display streamThermal",class="btn btn-primary")
    })

    output$display_save_thermal_button <- renderUI({
      downloadButton(outputId="save_thermal", label="Save thermal statistics to excel",class="btn btn-primary")
    })

    output$display_help_text_thermal_statistics <- renderUI({
      verbatimTextOutput("help_text_thermal_statistics")
    })

    output$help_text_thermal_statistics <- renderText({
      filePath <- "help_text_files/Temperature_ThermalStatistics.txt"
      fileText <- paste(readLines(filePath,encoding="UTF-8"),collapse="\n")
      fileText
    })

    ############ DE, TEMP, Air vs water" << Temperature ############

    output$air_vs_water_input_1 <- renderUI({
      variables_avail <- names(uploaded_data())
      air_keys_in_favor_order <- c("Air.Temp.C","AIR.TEMP.C","Air_Temp_C","AIR_TEMP_C")
      possible_air_columns <- air_keys_in_favor_order[air_keys_in_favor_order %in% variables_avail]
      if (length(possible_air_columns)==0){
        air_to_select <- variables_avail[grep('air',variables_avail,ignore.case=TRUE)][1]
      }else{
        air_to_select <- possible_air_columns[1]
      }
      selectizeInput("air_temp_name",label ="Select Air Temperature Column",
                     choices=variables_avail,
                     multiple = FALSE,
                     selected=air_to_select,
                     options = list(hideSelected = FALSE))
    })

    output$air_vs_water_input_2 <- renderUI({
      variables_avail <- names(uploaded_data())
      water_keys_in_favor_order <- c("Water.Temp.C","WATER.TEMP.C","Water_Temp_C","WATER_TEMP_C")
      possible_water_columns <- water_keys_in_favor_order[water_keys_in_favor_order %in% variables_avail]
      if (length(possible_water_columns)==0){
        water_to_select <- variables_avail[grep('water',variables_avail,ignore.case=TRUE)][1]
      }else{
        water_to_select <- possible_water_columns[1]
      }
      selectizeInput("water_temp_name",label ="Select Water Temperature Column",
                     choices=variables_avail,
                     multiple = FALSE,
                     selected=water_to_select,
                     options = list(hideSelected = FALSE))
    })

    air_limit_temp_tooltip_text = paste0("limit the data points with air temperature")
    output$air_vs_water_input_4 <- renderUI({
      tipify(numericInput("air_limit_temp",label ="air temperature less than this value will be excluded",0,min=-10,max=100,step=1.0),air_limit_temp_tooltip_text,placement="right",trigger="hover")
    })

    output$display_thermal_sensitivity_button <- renderUI({
      actionButton(inputId="display_thermal_sensitivity", label="Display thermal sensitivity",class="btn btn-primary")
    })

    output$display_help_text_air_water <- renderUI({
      verbatimTextOutput("help_text_air_water")
    })

    output$help_text_air_water <- renderText({
      filePath <- "help_text_files/Temperature_AirWater.txt"
      fileText <- paste(readLines(filePath,encoding="UTF-8"),collapse="\n")
      fileText
    })

    ############  DE, TEMP, "growing degree days" << Temperature ###############

    output$growing_degree_days_input_1 <- renderUI({
      verbatimTextOutput("come_later_text")
    })

    # output$come_later_text <- renderText({
    #   myText <- "Coming later"
    # })

    output$display_help_text_growing_degree_days <- renderUI({
      verbatimTextOutput("help_text_growing_degree_days")
    })

    output$help_text_growing_degree_days <- renderText({
      filePath <- "help_text_files/Temperature_GrowingDegreeDays.txt"
      fileText <- paste(readLines(filePath,encoding="UTF-8"),collapse="\n")
      fileText
    })



    ############ DE, TEMP, thermal classification" << Temperature ############

    output$water_temp_class_input_1 <- renderUI({
      variables_avail <- names(uploaded_data())
      water_keys_in_favor_order <- c("Water.Temp.C","WATER.TEMP.C","Water_Temp_C","WATER_TEMP_C")
      possible_water_columns <- water_keys_in_favor_order[water_keys_in_favor_order %in% variables_avail]
      if (length(possible_water_columns)==0){
        water_to_select <- variables_avail[grep('water',variables_avail,ignore.case=TRUE)][1]
      }else{
        water_to_select <- possible_water_columns[1]
      }
      selectizeInput("water_temp_name_in_class",label ="Select Water Temperature Column",
                     choices=variables_avail,
                     multiple = FALSE,
                     selected=water_to_select,
                     options = list(hideSelected = FALSE))
    })

    output$display_water_temp_class_button <- renderUI({
      actionButton(inputId="display_water_class", label="Display water temperature class",class="btn btn-primary")
    })

    output$display_help_text_water_temp_class <- renderUI({
      verbatimTextOutput("help_text_water_temp_class")
    })

    output$help_text_water_temp_class <- renderText({
      filePath <- "help_text_files/Temperature_Classification.txt"
      fileText <- paste(readLines(filePath,encoding="UTF-8"),collapse="\n")
      fileText
    })

    ############  DE, HYDRO, IHA" << Hydrology ############

    output$IHA_input_1 <- renderUI({
      if (length(processed$processed_dailyStats)==0){
      variables_avail <- names(uploaded_data())
      date_keys_in_favor_order <- c("Date.Time","DATE.TIME","Year","YEAR","Date","DATE","MonthDay")
      possible_date_columns <- date_keys_in_favor_order[date_keys_in_favor_order %in% variables_avail]
      }else{
        possible_date_columns <- "Date"
        variables_avail <- possible_date_columns
      }
      selectizeInput("IHA_Date_name",label ="Select Date Column",
                     choices=variables_avail,
                     multiple = FALSE,
                     selected=possible_date_columns[1],
                     options = list(hideSelected = FALSE))
    })

    output$IHA_input_2 <- renderUI({
      variables_avail <- names(processed$processed_dailyStats)
      parameter_to_select <- variables_avail[grep('Discharge',variables_avail,ignore.case=TRUE)][1]
      selectizeInput("parameter_name",label ="Select Parameter Column",
                     choices=variables_avail,
                     multiple = FALSE,
                     selected= parameter_to_select,
                     options = list(hideSelected = FALSE))
    })

    output$display_IHA_button <- renderUI({
      actionButton(inputId="display_IHA", label="Display IHA tables",class="btn btn-primary")
    })


    output$display_save_IHA_button <- renderUI({
      downloadButton(outputId="save_IHA", label="Save IHA results to excel",class="btn btn-primary")
    })

    output$display_help_text_IHA <- renderUI({
      verbatimTextOutput("help_text_IHA")
    })

    output$help_text_IHA <- renderText({
      filePath <- "help_text_files/Hydrology_IHA.txt"
      fileText <- paste(readLines(filePath,encoding="UTF-8"),collapse="\n")
      fileText
    })

    ############  DE, HYDRO, Flashiness" << Hydrology ############

    output$flashiness_input_1 <- renderUI({
      verbatimTextOutput("come_later_text_1")
    })

    output$come_later_text_1 <- renderText({
      myText <- "Coming later"
    })

    output$display_help_text_flashiness <- renderUI({
      verbatimTextOutput("help_text_flashiness")
    })

    output$help_text_flashiness <- renderText({
      filePath <- "help_text_files/Hydrology_RBI.txt"
      fileText <- paste(readLines(filePath,encoding="UTF-8"),collapse="\n")
      fileText
    })

  }) #observe Event end



  #################  1:Summary table << All parameters #################
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

  mySummarisemore <- function(df=myDf, variable=myVariable,metrics=myMetrics,timeframe=myTimeframe){
       variable_col_name <- paste0(variable,".",metrics)
       names(df)[match(variable_col_name,names(df))] <- "x"
    if (timeframe == "year/month"){
      ## summarise by each year&month first
      df[,"yearmonth"] <- format(df[,"Date"],"%Y%m")
      df.summary <- doBy::summaryBy(x~yearmonth,data=df,FUN=mean,na.rm=TRUE,variable.names=variable)
      df.summary[,"year"] <- substr(df.summary[,"yearmonth"],1,4)
      df.summary[,"month"] <- substr(df.summary[,"yearmonth"],5,6)
      df.summary <- df.summary[2:4]
      df.summary[,"x.mean"] <- formatC(df.summary[,"x.mean"],digits=2,format="f")
      df.summary.wide <- pivot_wider(df.summary,names_from=year,values_from=x.mean)
      ## summarise by each month regardless of the year to get overall mean for each month
      df[,"month"] <- format(df[,"Date"],"%m")
      df.summary.overall <- doBy::summaryBy(x~month,data=df,FUN=mean,na.rm=TRUE,variable.names=variable)
      df.summary.overall[,"x.mean"] <- formatC(df.summary.overall[,"x.mean"],digits=2,format="f")
      names(df.summary.overall)[2] <- "Overall"
      df.summary.all <- merge(df.summary.wide,df.summary.overall,by="month")
      return(df.summary.all)
    }else if(timeframe =="year"){
      df[,"year"] <- format(df[,"Date"],"%Y")
      df.summary <- doBy::summaryBy(x~year,data=df,FUN=mean,na.rm=TRUE,variable.names=variable)
      df.summary[,"x.mean"] <- formatC(df.summary[,"x.mean"],digits=2,format="f")
      df.summary.wide <- pivot_wider(df.summary,names_from=year,values_from=x.mean)
      df.summary.wide[,"Overall"] <- formatC(mean(df$x,na.rm=TRUE),digits=2,format="f")
      return(df.summary.wide)
    }else if(timeframe == "year/season"){
      df <- addSeason(df)
      df.summary <- doBy::summaryBy(x~yearseason,data=df,FUN=mean,na.rm=TRUE,variable.names=variable)
      df.summary[,"year"] <- substr(df.summary[,"yearseason"],1,4)
      df.summary[,"season"] <- substr(df.summary[,"yearseason"],5,nchar(df.summary[,"yearseason"]))
      df.summary <- df.summary[2:4]
      df.summary[,"x.mean"] <- formatC(df.summary[,"x.mean"],digits=2,format="f")
      df.summary.wide <- pivot_wider(df.summary,names_from=year,values_from=x.mean)
      df.summary.overall <- doBy::summaryBy(x~season,data=df,FUN=mean,na.rm=TRUE,variable.names=variable)
      df.summary.overall[,"x.mean"] <- formatC(df.summary.overall[,"x.mean"],digits=2,format="f")
      names(df.summary.overall)[2] <- "Overall"
      df.summary.all <- merge(df.summary.wide,df.summary.overall,by="season")
      return(df.summary.all)

    }else if(timeframe =="season"){
      df <- addSeason(df)
      df.summary <- doBy::summaryBy(x~season,data=df,FUN=mean,na.rm=TRUE,variable.names=variable)
      df.summary[,"x.mean"] <- formatC(df.summary[,"x.mean"],digits=2,format="f")
      df.summary.wide <- pivot_wider(df.summary,names_from=season,values_from=x.mean)
      df.summary.wide[,"Overall"] <- formatC(mean(df$x,na.rm=TRUE),digits=2,format="f")
      return(df.summary.wide)
    }else{
      stop("please specify one of the following summarise timeframes:
         'year/month','year','year/season','season'")
    }
  }


  observeEvent(input$display_table, {
    output$display_summary_table_1 <- renderUI({
      withSpinner(dataTableOutput("summary_table_1"))
    })
    myList <- processed$processed_dailyStats
    variable_to_summarise <- input$summarise_variable_name
    myData <- myList[[which(names(myList)==variable_to_summarise)]]
    summary_df <- mySummarisemore(df=myData,variable=input$summarise_variable_name,metric=input$summarise_metrics,timeframe=input$summarise_by)
    table_title <- paste0(input$summarise_variable_name," ",input$summarise_metrics)
    output$summary_table_1 <- DT::renderDataTable({
      print("inside renderDT now...")
      myTable <- DT::datatable(
                 summary_df,
                 caption = htmltools::tags$caption(table_title,style="color:black;font-size:16px;font-weight:bold;text-align:center;"),
                 extensions ="Buttons",
                 rownames = FALSE,
          options = list(
          scrollX = TRUE, #allow user to scroll wide tables horizontally
          stateSave = FALSE,
          pageLength = 15,
          dom = 'Bt',
          buttons = list('copy','print',list(extend = 'collection',buttons = c('csv','excel','pdf'),text='Download')),
          columnDefs = list(list(className="dt-center",targets="_all"))
          )
      ) # dataTable end
      saveToReport$summaryTable <- myTable

      print(myTable)
    })  # renderDT end

  }) # observeEvent end

  ################# 2:Time series plot << All parameters #################
  uploaded_newData<-eventReactive(c(input$uploaded_newData_file),{
    my_data <- uploadFile(c(input$uploaded_newData_file), stopExecution = FALSE)
    my_data <- my_data[rowSums(is.na(my_data) | is.null(my_data) | my_data == "") != ncol(my_data),]
    return(my_data)
  })



  observeEvent(input$dailyStats_ts_metrics,{
    if (!is.null(input$dailyStats_ts_metrics)&(input$dailyStats_ts_metrics == 'mean'|input$dailyStats_ts_metrics == 'median')){
        shinyjs::show("cp_shaded_region")
    }else{
      shinyjs::hide("cp_shaded_region")
    }
    #click("display_ts")
    
  })

  observeEvent(input$dailyStats_shading,{
    if (input$dailyStats_shading == 'newData' | input$dailyStats_shading == 'discreteData'){
      shinyjs::show("cp_new_data")
    }else{
      shinyjs::hide("cp_new_data")
    }
  })
  
  #Discrete Data Exploration
  
  uploaded_discreteData<-eventReactive(c(input$uploaded_discrete_file),{
    discrete_data <- uploadFile(c(input$uploaded_discrete_file), stopExecution = FALSE)
    discrete_data <- discrete_data[rowSums(is.na(discrete_data) | is.null(discrete_data) | discrete_data == "") != ncol(discrete_data),]
    return(discrete_data)
  })
  
 
  
  observeEvent(input$uploaded_discrete_file,{
    cols_avail <- colnames(uploaded_discreteData())
    fileContentForDisplay <- head(uploaded_discreteData())
    output$discreteDateAndTimeBox <- renderUI({
      div(id="dt_discrete",
          div(class = "panel panel-default",width = "100%",
              div(class = "panel-heading",style="width:100%;",
                  span("Select Date and Time for discrete data", style="font-weight:bold;"),
                  span(
                    actionButton(inputId="dateTimeBoxButton_discrete", 
                                 style="float:right;", class="btn btn-primary btn-xs", 
                                 label="Hide Selection", icon= icon("arrow-down"))
                  )
              ),
              div(uiOutput("disDateAndTimeError")),
              box(width="100%",class="displayed",id="dateBox_discrete",
                  div(
                    style = "margin-left:10px",
                    dateAndTimeUI(id = "discretePage", paramChoices = cols_avail, uploadedCols = cols_avail)
                  ),
                  hr(style="margin:0px;padding:0px;"),
                  fluidRow(
                    span(width = "85%", actionButton(inputId = "display_discrete_data", label = "Display", class = "btn btn-primary"), style = "margin:5px 15px 5px 25px;"),
                    span("Note: Red border denotes required fields.", style = "font-weight:bold;color:#b94a48;")
                  ),
                  hr(style="margin:0px;padding:0px;"),
                  fluidRow(
                    column(width=12, 
                           tags$div(
                            renderTable({
                             fileContentForDisplay
                           },type="html",bordered = TRUE,striped=TRUE,align="c", width="100%"),
                           style="overflow-x:auto;")# end of div
                           ) # end of column
                  )# end of row
              )# end of box
          ) 
      )
    })
    # init the module
    discreteDTvalues$disDateAndTime <- dateAndTimeServer(id = "discretePage", uploaded_discreteData())
    
  })
  
  # observeEvent(uploaded_discreteData(), {
  #   discreteDTvalues$disDateAndTime <- dateAndTimeServer(id = "discretePage", uploaded_discreteData())
  # })
  
  
  output$baseParameters <- renderUI({
    baseParams <- homeDTvalues$homeDateAndTime$parmToProcess()
    selectizeInput(
      "discreteBaseId",
      label = "Select continuous parameters to process",
      choices = baseParams,
      multiple = TRUE,
      selected = baseParams[1],
      options = list(
        hideSelected = FALSE,
        plugins = list('remove_button')
      )
    )
  })
  
  
 # draw_discrete_stats <- function(renderStatus=FALSE){
  observeEvent(input$display_discrete_data, {
    shinyjs::runjs("$('#disDateAndTimeErrorinnerDiv').remove()")
    discreteDTvalues$disDateAndTime <- dateAndTimeServer(id = "discretePage", uploaded_discreteData())
    localDiscreteDateAndTime <- discreteDTvalues$disDateAndTime
    mainPlot <- NULL
    if (localDiscreteDateAndTime$isTimeValid() & localDiscreteDateAndTime$isDateAndtimeValid()) {
      tryCatch({
        variable_to_plot <- sort(localDiscreteDateAndTime$parmToProcess(), decreasing = FALSE)
        base_vars_to_plot <- sort(homeDTvalues$homeDateAndTime$parmToProcess(), decreasing = FALSE)
        if(identical(variable_to_plot,base_vars_to_plot)) {
          discrete_data <- getFormattedRawData(localDiscreteDateAndTime, uploaded_discreteData(), tabName = "", errorDivId = "disDateAndTimeError")
          
          if ("date.formatted" %in% colnames(discrete_data) & nrow(discrete_data) != nrow(discrete_data[is.na(discrete_data$date.formatted), ])) {
            base_data <- formated_raw_data$derivedDF
            if (!is.null(base_vars_to_plot) & nrow(base_data) != nrow(base_data[is.na(base_data$date.formatted),])){
              mergedData <- NULL
              for(varName in variable_to_plot) {
                step1 <- base_data %>% select("continuous_value"=all_of(varName),"Date" = c(date.formatted))
                
                step2 <- discrete_data %>% select("discrete_value" = all_of(varName), "Date" = c(date.formatted))
                
                step2 <-  step2 %>% dplyr::left_join(step1,by = "Date") %>% 
                          dplyr::select(discrete_value,"Matching_Continuous_value" = continuous_value, "discrete_Date" = c(Date))
                
                timediff <- get_interval(step1$Date)
                #print(timediff)
                timediff <- ifelse(timediff == "min", "15 mins", timediff)

                step1 <- step1 %>%
                    mutate(Date = as.POSIXct(Date)) %>%
                    complete(Date = seq(min(Date,na.rm = TRUE), max(Date, na.rm = TRUE), by=timediff))
                
                
                #write.csv(step1,"step1.csv",row.names=FALSE)
                
                tempdf <- as.data.frame(qpcR:::cbind.na(step1,step2))
                mergedData[[varName]] <- tempdf
              }
              combinded_df <- bind_rows(mergedData, .id="df")

              combinded_df$bothValues <- c(paste("\nContinuous Value: ", combinded_df$Matching_Continuous_value, "\n",
                                                      "Discrete Value: ", combinded_df$discrete_value, "\n"
                                              ))

              
              # shared x axis so calculate using base data file
              mainMapTitle <- "Discrete and continuous data"
              main_range = calculate_time_range(as.list(combinded_df))
              mainBreaks = main_range[[1]]
              main_x_date_label = main_range[[2]]
              

            mainPlot <- prepareDiscretePlot(combinded_df, mapTitle=mainMapTitle, xDateLabel=main_x_date_label, xDateBrakes= mainBreaks,base_vars_to_plot)
              if(!is.null(mainPlot) & length(variable_to_plot) > 0){
                shinyjs::runjs("$('#dateTimeBoxButton_discrete').click()")
                output$display_time_series_discrete <-  renderPlotly({
                  ggplotly(mainPlot, height=calulatePlotHeight(length(variable_to_plot)*2)) 
                   # %>% plotly::layout(legend = list(orientation = "h", x = 0.4, y = -0.4))
                })
                overridePotlyStyle("display_time_series_discrete")
              }
            }
          } 
        } else {
          shinyAlertUI("common_alert_msg", discreteVarMismatch, "ERROR")
          return(mainPlot)
        }
      },error = function(parsingMsg) {
        print(parsingMsg)
        output$display_validation_msgs_discrete <- renderUI({
          print(parsingMsg)
          prepareDateFormatErrorMsg(parsingMsg)
        })
      }, warning = function(parsingMsg){
          print(parsingMsg)
          output$display_validation_msgs_discrete <- renderUI({
            prepareDateFormatErrorMsg(parsingMsg)
          })
      }, message = function(parsingMsg) {
        print(parsingMsg)
        output$display_validation_msgs_discrete <- renderUI({
          prepareDateFormatErrorMsg(parsingMsg)
        })
      })
    }
  })
 
  # End of Discrete data related functions
  
  
  
  # continuous Data Exploration with New File
  
  observeEvent(input$uploaded_newData_file,{
    cols_avail <- colnames(uploaded_newData())
    fileContentForDisplay <- head(uploaded_newData())
     output$dateAndTimeBox <- renderUI({

     div(id="dt_new",
           div(class = "panel panel-default",width = "100%",
               div(class = "panel-heading",
                   span("Step 2: Select Date and Time for new data", style="font-weight:bold;"),
                   span(
                     actionButton(inputId="dateTimeBoxButton_new", 
                                  style="float:right;", class="btn btn-primary btn-xs", 
                                  label="Hide Selection", icon= icon("arrow-down"))
                   )
               ),
               div(uiOutput("newDateAndTimeError"), style = "font-weight:bold;color:#b94a48;margin:5px;"),
               box(width="100%",class="displayed",id="dateBox_new",
                   div(
                     style = "margin-left:10px",
                     dateAndTimeUI(id = "newDataUpload", paramChoices = cols_avail, uploadedCols = cols_avail)
                   ),
              fluidRow(
              column(width = 4,
                   div(style="margin-left:15px;",
                   selectizeInput("newData_lower_col",
                                  label = "Select column to be used as lower bound",
                                  choices = c("",cols_avail),
                                  multiple = FALSE,
                                  selected = NULL,
                                  options = list(hideSelected = FALSE))
                   )),
              column(width = 4,
                   selectizeInput("newData_upper_col",
                                  label = "Select column to be used as upper bound",
                                  choices = c("",cols_avail),
                                  multiple = FALSE,
                                  selected = NULL,
                                  options = list(hideSelected = FALSE))
                   )
          ),
          # fluidRow(
          #   column(width=2, actionButton(inputId="display_new_data", label="Display",class="btn btn-primary")),
          #   column(width=10, div(id="dummy"))
          # ),
          hr(style="margin:0px;padding:0px;"),
          fluidRow(
            column(width=12, 
                   tags$div(
                     renderTable({
                     fileContentForDisplay
                   },type="html",bordered = TRUE,striped=TRUE,align="c"),style="overflow-x:auto;")
           )
          )
      )
      ) # end of box
     )
    })
     newDTvalues$newDateAndTime <- dateAndTimeServer(id = "newDataUpload", )
  })

 display_new_data <- function(renderStatus=FALSE) {
   shinyjs::runjs("$('#newDateAndTimeErrorinnerDiv').remove()")
   newDTvalues$newDateAndTime <- dateAndTimeServer(id = "newDataUpload",uploaded_newData())
   newHomeDateAndTime <- newDTvalues$newDateAndTime
   mainPlot <- NULL
   if (newHomeDateAndTime$isTimeValid() & newHomeDateAndTime$isDateAndtimeValid() &
       validateNewLowerAndUpper("display_validation_msgs_new2") == FALSE) {
            tryCatch({
              new_raw_data <- getFormattedRawData(newHomeDateAndTime, uploaded_newData(), tabName = "", errorDivId = "newDateAndTimeError")
            if(nrow(new_raw_data) != nrow(new_raw_data[is.na(new_raw_data$date.formatted),])) {
                  variable_to_plot <- newDTvalues$newDateAndTime$parmToProcess()
                  mainList <- list()
                  for(varName in variable_to_plot) {
                    mainList[[varName]] <- as.data.frame(new_raw_data %>% select(value=all_of(varName), lower_col=input$newData_lower_col , upper_col=input$newData_upper_col, Date=date.formatted))
                  }
                  plotTitle <- ifelse((input$dailyStats_ts_title == ""), "New Data", input$dailyStats_ts_title) 
                  mainMapTitle <- getMapTitle("dynamic", plotTitle, lowerColumn = input$newData_lower_col, upperColumn = input$newData_upper_col)
                  main_range = calculate_time_range(as.list(bind_rows(mainList, .id="df")))
                  mainBreaks = main_range[[1]]
                  main_x_date_label = main_range[[2]]
                  mainPlot <- prepareBasePlot(dataList= mainList, mapTitle=mainMapTitle, xDateLabel=main_x_date_label, xDateBrakes= mainBreaks)
                  if(!is.null(mainPlot) & length(newHomeDateAndTime$parmToProcess()) > 0 & renderStatus==FALSE){
                    return(mainPlot)
                  }else{
                    shinyjs::runjs("$('#dateTimeBoxButton_new').click()")
                    output$display_time_series_new <-  renderPlotly({
                      ggplotly(mainPlot,height=calulatePlotHeight(length(newHomeDateAndTime$parmToProcess()) * 2)) %>% plotly::layout(legend = list(orientation = "h", x = 0.4, y = -0.4))
                    })
                  }
                }
               return(mainPlot)
            
              },error = function(parsingMsg) {
                print(parsingMsg)
                output$display_validation_msgs_new <- renderUI({
                  print(parsingMsg)
                  prepareDateFormatErrorMsg(parsingMsg)
                })
              }, warning = function(parsingMsg){
                print(parsingMsg)
                output$display_validation_msgs_new <- renderUI({
                  prepareDateFormatErrorMsg(parsingMsg)
                })
              }, message = function(parsingMsg) {
                print(parsingMsg)
                output$display_validation_msgs_new <- renderUI({
                  prepareDateFormatErrorMsg(parsingMsg)
                })
              }, finally = {
                 return(mainPlot)
              })
          }
  }
  
  observeEvent(input$dtNumOfCols, {
    if(isolate(input$dtNumOfCols) == "combined")
      shinyjs::hide("timeFieldDiv")
    else if(isolate(input$dtNumOfCols) == "separate")
      shinyjs::show("timeFieldDiv")
  })
  
  
  observeEvent(input$dtNumOfCols1, {
    if(isolate(input$dtNumOfCols1) == "combined")
      shinyjs::hide("timeFieldDiv2")
    else if(isolate(input$dtNumOfCols1) == "separate") 
      shinyjs::show("timeFieldDiv2")
  })
  
  observeEvent(input$dtNumOfColsDis, {
    if(isolate(input$dtNumOfColsDis) == "combined")
      shinyjs::hide("timeFieldDiv3")
    else if(isolate(input$dtNumOfColsDis) == "separate") 
      shinyjs::show("timeFieldDiv3")
  })
  
  
  #Download USGS gage data
  observeEvent(input$display_gage_ts, {
    
     if(input$gage_id != "" && length(input$gage_id) > 0) {
      #data <- uploaded_data()
      consoleUSGS$disp <- data.frame(consoleOutputUSGS = character())
      Sys.sleep(0.5)
      defaultTimeZone = "America/New_York"
     
      withProgress(message = paste("Getting USGS data"), value = 0, {
        incProgress(0, detail = paste("Retrieving records for site ", input$gage_id))
          #Actually gets the gage data from the USGS NWIS system
          gageRawData$gagedata <- fun.GageData(
            myData.SiteID           <- input$gage_id,
            myData.Type             <- "Gage",
            myData.DateRange.Start  <- as.character(dateRange$min),
            myData.DateRange.End    <- as.character(dateRange$max),
            myDir.export            <- file.path(".", "data"),
            fun.myTZ = defaultTimeZone #ContData.env$myTZ
          )
        message("USGS data retrieved")
        #Fills in the progress bar once the operation is complete
        incProgress(1/1, detail = paste("Retrieved records for site ", input$gage_id))
        Sys.sleep(1)
      })
      #print(gageRawData$gagedata)

      allVars <- colnames(gageRawData$gagedata)
      varsToPlot <- allVars[!(allVars %in% c("SiteID","GageID","Date.Time"))]
      updateSelectizeInput(session, 'gaze_params', choices = varsToPlot, selected = varsToPlot[1])
      shinyjs::show(id="gageVarsDiv")

      #Names the single column of the R console output data.frame
      colnames(consoleUSGS$disp) <- "R console messages for all USGS data retrieval:"
     } else {
       shinyAlertUI("common_alert_msg", noGageIdFound, "WARNING")
    }
  })
  
  observeEvent(input$display_subplot_ts, {
    shinyjs::runjs("$('#display_time_series').empty()")
    shinyjs::runjs("$('#display_time_series_1').empty()")
    shinyjs::runjs("$('#display_time_series_3').empty()")
    
    daymet_data_raw <- NULL
    gage_data_raw <- NULL
    base_data_raw <- NULL
    mergedList <- list()
    totalH <- 0L
    if(length(input$dailyStats_ts_variable_name2) > 0) {
      
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
        
          print("ng gage data is")
          print(gage_data_raw)
        
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
       
        # d <- as.Date(raw_data$date.formatted)
        #  date_temp <- seq(min(d), max(d), by = 1) 
        #  allMissing <- date_temp[!date_temp %in% d] 
        #  
        #  if(length(allMissing) > 0) {
        #    raw_data <- raw_data %>%
        #      mutate(date.formatted = as.Date(date.formatted)) %>%
        #      complete(date.formatted = seq.Date(min(date.formatted,na.rm = TRUE), max(date.formatted, na.rm = TRUE), by="day"))
        #  }
         
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
        labs(title="All Merged", y="Parameters", x="Date") + 
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
      allCom = allCom + facet_grid(parameter ~ ., scales = "free_y")
      
      
      output$display_downloaded_data <- renderPlotly({
        ggplotly(allCom, height = calulatePlotHeight(totalH*2)) 
        #%>% plotly::layout(legend = list(orientation = "h", x = 0.4, y = -0.4))
      }) 
      overridePotlyStyle("display_downloaded_data")
    } else {
      shinyAlertUI("common_alert_msg", selectBaseVars, "WARNING")
    }
  })


  observeEvent(input$display_ts, {
    
          shinyjs::runjs("$('#display_time_series').empty()")
          shinyjs::runjs("$('#display_time_series_1').empty()")
          shinyjs::runjs("$('#display_time_series_3').empty()")
          
          if(length(processed$processed_dailyStats) > 0 & length(input$dailyStats_ts_variable_name) > 0) {
          #Display uploaded file stats
          if (!is.null(input$dailyStats_ts_metrics)&(input$dailyStats_ts_metrics=="mean"|input$dailyStats_ts_metrics=="median")&input$dailyStats_shading != "newData"){
            mainPlot <- draw_uploaded_file_ts()
            if(!is.null(mainPlot) & length(input$dailyStats_ts_variable_name) > 0){
              output$display_time_series <-  renderPlotly({
                ggplotly(mainPlot,height=calulatePlotHeight(length(input$dailyStats_ts_variable_name) * 2)) %>% plotly::layout(legend = list(orientation = "h", x = 0.4, y = -0.4)) 
              })
            }
   
         } else if (!is.null(input$dailyStats_ts_metrics)&(input$dailyStats_ts_metrics=="mean"|input$dailyStats_ts_metrics=="median")&input$dailyStats_shading == "newData"){
           # this option is removed from the front end for now
             newMainPlot <- display_new_data(renderStatus=FALSE)
               if(!is.null(newMainPlot) & length(newDTvalues$newDateAndTime$parmToProcess()) > 0){
                 shinyjs::runjs("$('#dateTimeBoxButton_new').click()")
                 output$display_time_series_new <-  renderPlotly({
                   ggplotly(newMainPlot,height=calulatePlotHeight(length(newDTvalues$newDateAndTime$parmToProcess()) * 2)) %>% plotly::layout(legend = list(orientation = "h", x = 0.4, y = -0.4))
                 })
               }
          } else {
                   basePlot <-  draw_uploaded_file_stats()
                   if(!is.null(basePlot)) {
                     output$display_time_series <-  renderPlotly({
                       ggplotly(basePlot,height=calulatePlotHeight(length(input$dailyStats_ts_variable_name))) %>% plotly::layout(legend = list(orientation = "h", x = 0.4, y = -0.4))
                     })
                   }
             }          
            overridePotlyStyle("display_time_series")
            overridePotlyStyle("display_time_series_1")
            overridePotlyStyle("display_time_series_3")
            overridePotlyStyle("display_time_series_new")
            } else {
             shinyAlertUI("common_alert_msg", calculateDailyStats, "WARNING")
            }

  })  # observeEvent end
  
  overridePotlyStyle <- function(elementId) {
    shinyjs::removeClass(elementId, "html-fill-item-overflow-hidden")
    shinyjs::runjs("$('#display_time_series').removeAttr('style')")
    shinyjs::runjs("$('#display_time_series_1').removeAttr('style')")
    shinyjs::runjs("$('#display_time_series_3').removeAttr('style')")
    
  }

  ## close the alert messages
  observeEvent(input$alert_data_not_avail_for_ts,{
    #print(input$alert_no_date)
    shinyjs::runjs("swal.close();")
  })



  observeEvent(input$toggleLayout,{
    shinyjs::toggle(id="ts_sidePanel")
    shinyjs::toggle(id="ts_mainPanel")
  })


  #################  3:Time series - Annual overlays << All parameters #################
  uploaded_overlay_newData <- eventReactive(c(input$uploaded_overlay_newData_file),{
    my_data <- uploadFile(c(input$uploaded_overlay_newData_file), stopExecution = FALSE)
      return(my_data)
  })

  observeEvent(input$overlay_shading,{
    if (input$overlay_shading == 'newData'){
      shinyjs::show("cp_new_data_overlay")
    }else{
      shinyjs::hide("cp_new_data_overlay")
    }
  })

  observeEvent(input$uploaded_overlay_newData_file,{
    cols_avail <- colnames(uploaded_overlay_newData())
    #print(cols_avail)
    updateSelectizeInput(session,"overlay_newData_lower_col",label ="Select column to be used as lower bound",
                         choices=cols_avail,
                         selected=''
    )

    updateSelectizeInput(session,"overlay_newData_longterm_col",label ="Select column to be used as long-term reference line",
                         choices=cols_avail,
                         selected=''
    )

    updateSelectizeInput(session,"overlay_newData_upper_col",label ="Select column to be used as upper bound",
                         choices=cols_avail,
                         selected='',
                         options = list(hideSelected = FALSE))

    updateSelectizeInput(session,"overlay_newData_date_col",label ="Select month-day column",
                         choices=cols_avail,
                         selected='',
                         options = list(hideSelected = FALSE))
  })


  # output$display_time_series <- renderPlotly({(NULL)})
  # output$testSubgraph <- renderPlotly({(NULL)})
  
  # display_time_series_Proxy <- plotlyProxy("display_time_series")
  # gage_proxy <- plotlyProxy("plot_gage_ts")
  
  

  observeEvent(input$display_ts_overlay, {
    
    #plot_dailyStats_ts
    output$display_time_series_overlay <- renderUI({
      withSpinner(plotlyOutput("plot_dailyStats_ts_overlay",height="550px",width="1200px"),type=2)
    })

    output$display_time_series_overlay_1 <- renderUI({
      withSpinner(plotOutput("plot_dailyStats_ts_overlay_1",height="550px",width="1200px"),type=2)
    })

    myList <- processed$processed_dailyStats
    variable_to_plot <- input$dailyStats_ts_overlay_variable_name
    myData <- myList[[which(names(myList)==variable_to_plot)]]
    mean_col <- paste0(input$dailyStats_ts_overlay_variable_name,".",input$dailyStats_ts_overlay_metrics)
    cols_selected <- c("Date",mean_col)
    data_to_plot <- myData[cols_selected]
    
    #print(format(as.Date(data_to_plot$Date, format="%Y-%m-%d %H:/%M:%S"),"%Y"))
   
    data_to_plot[,"year"] <- format(as.Date(data_to_plot$Date, format="%Y-%m-%d %H:%M:%S"),"%Y")
    ## dynamically change the "date_breaks" based on the width of the time window

    if ((input$dailyStats_ts_overlay_metrics=="mean"|input$dailyStats_ts_overlay_metrics=="median")&input$overlay_shading=="overall"){
    ## calculate overall(include all years) monthly minimum and maximum values
      min_col <- paste0(input$dailyStats_ts_overlay_variable_name,".min")
      max_col <- paste0(input$dailyStats_ts_overlay_variable_name,".max")
      data_for_overlay <- myData[c("Date",min_col,max_col)]
      #data_for_overlay[,"MonthDay"] <- format(data_for_overlay[,"Date"],"%m-%d")
      data_for_overlay[,"MonthDay"] <- format(as.Date(data_for_overlay$Date, format="%Y-%m-%d %H:%M:%S"),"%m-%d")
    
      
      monthDay_min <- aggregate(data_for_overlay[,2],list(data_for_overlay$MonthDay),FUN=mean)
      monthDay_max <- aggregate(data_for_overlay[,3],list(data_for_overlay$MonthDay),FUN=mean)
      merged_overlay <- merge(monthDay_min,monthDay_max,by="Group.1")
      colnames(merged_overlay) <- c("MonthDay","min","max")
      #save(merged_overlay,file="test_overall_min_max_overlay.RData")
      output$plot_dailyStats_ts_overlay <- renderPlotly({
        isolate({
          p1 <- ggplot(data_to_plot)+
            geom_line(aes(x=as.Date(yday(Date),"2000-01-01"),y=isolate(!!sym(mean_col)),colour=year),size=0.8)+
            geom_ribbon(data=merged_overlay,aes(x=as.Date(yday(paste0("2000","-",MonthDay)),"2000-01-01"),
                                                        ymin=min,ymax=max,
                                                fill="overall minimum and maximum" ),alpha=0.5)+
            scale_x_date(date_breaks="1 month",limits=c(as.Date("2000-01-01"),as.Date("2000-12-31")),date_labels = "%m%d")+
            scale_fill_manual(" ",labels="overall minimum and maximum",values=c("grey80"="grey80"))+
            labs(title=isolate(input$dailyStats_ts_overlay_title),x = "MonthDay",y =mean_col)+
            theme_classic()+
            theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                  ,panel.border = element_rect(colour="black",fill=NA, size=0.5)
                  ,plot.title = element_text(hjust=0.5)
                  ,plot.background = element_rect(color="grey20",size=2)
                  ,legend.position = "right"
                  ,axis.text.x=element_text(angle=45, hjust=1))
          ggplotly(p1,dynamicTicks = FALSE)
          print(p1)
        })
      }) # renderPlot close

      output$plot_dailyStats_ts_overlay_1 <- renderPlot({
        isolate({
          p1 <- ggplot(data_to_plot)+
            geom_line(aes(x=as.Date(yday(Date),"2000-01-01"),y=isolate(!!sym(mean_col)),colour=year),size=0.8)+
            geom_ribbon(data=merged_overlay,aes(x=as.Date(yday(paste0("2000","-",MonthDay)),"2000-01-01"),
                                                ymin=min,ymax=max,
                                                fill="overall minimum and maximum" ),alpha=0.5)+
            scale_x_date(date_breaks="1 month",limits=c(as.Date("2000-01-01"),as.Date("2000-12-31")),date_labels = "%m%d")+
            scale_fill_manual(" ",labels="overall minimum and maximum",values=c("grey80"="grey80"))+
            labs(title=isolate(input$dailyStats_ts_overlay_title),x = "MonthDay",y =mean_col)+
            theme_classic()+
            theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                  ,panel.border = element_rect(colour="black",fill=NA, size=0.5)
                  ,plot.title = element_text(hjust=0.5)
                  ,plot.background = element_rect(color="grey20",size=2)
                  ,legend.position = "right"
                  ,axis.text.x=element_text(angle=45, hjust=1))
          #ggplotly(p1,dynamicTicks = FALSE)
          print(p1)
        })
      }) # renderPlot close

    }else if ((input$dailyStats_ts_overlay_metrics=="mean"|input$dailyStats_ts_overlay_metrics=="median")&input$overlay_shading=="newData") {
      # This option is removed from the front end for now
      overlay_data <- uploaded_overlay_newData()


     # save(data_to_add_as_overlay,file="test_data_to_add_as_overlay.RData")

      output$plot_dailyStats_ts_overlay <- renderPlotly({
        isolate({

          p1 <- ggplot(data_to_plot)+
            geom_line(aes(x=as.Date(yday(Date),"2000-01-01"),y=!!sym(mean_col),colour=year),size=0.8)

          if((input$overlay_newData_lower_col!='')&(input$overlay_newData_upper_col!='')){
            overlay_cols_selected <- c(input$overlay_newData_date_col,input$overlay_newData_lower_col,input$overlay_newData_upper_col)
            data_to_add_as_overlay <- overlay_data[overlay_cols_selected]
             p1<- p1 +
             geom_ribbon(data=data_to_add_as_overlay,aes(x=as.Date(yday(paste0("2000","-",month_day)),"2000-01-01"),
                                                      ymin=!!sym(input$overlay_newData_lower_col),ymax=!!sym(input$overlay_newData_upper_col),
                                                      fill=input$overlay_newData_name),alpha=0.5)+
             scale_fill_manual(" ",labels=input$overlay_newData_name,values=c("grey80"="grey80"))
          }

          if(input$overlay_newData_longterm_col!=''){
          data_to_add_as_longterm <- overlay_data[c(input$overlay_newData_date_col,input$overlay_newData_longterm_col)]
          print(input$overlay_newData_longterm_col)
          p1 <- p1 +
            geom_line(data=data_to_add_as_longterm,aes(x=as.Date(yday(paste0("2000","-",month_day)),"2000-01-01"),y=!!sym(input$overlay_newData_longterm_col),color="USGS long-term"),size=0.8,color="black")
          }

         p1 <- p1+
          scale_x_date(date_breaks="1 month",limits=c(as.Date("2000-01-01"),as.Date("2000-12-31")),date_labels = "%m%d")+
          labs(title=isolate(input$dailyStats_ts_overlay_title),x = "MonthDay",y =mean_col)+
          theme_classic()+
          theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                ,panel.border = element_rect(colour="black",fill=NA, size=0.5)
                ,plot.title = element_text(hjust=0.5)
                ,plot.background = element_rect(color="grey20",size=2)
                ,legend.position = "right"
                ,axis.text.x=element_text(angle=45, hjust=1))
         p1<- plotly::ggplotly(p1)
         print(p1)

      })
      }) # renderPlot close

      output$plot_dailyStats_ts_overlay_1 <- renderPlot({
        isolate({
          p1 <- ggplot(data_to_plot)+
            geom_line(aes(x=as.Date(yday(Date),"2000-01-01"),y=!!sym(mean_col),colour=year),size=0.8)
            if((input$overlay_newData_lower_col!='')&(input$overlay_newData_upper_col!='')){
              overlay_cols_selected <- c(input$overlay_newData_date_col,input$overlay_newData_lower_col,input$overlay_newData_upper_col)
              data_to_add_as_overlay <- overlay_data[overlay_cols_selected]
              p1 <- p1+
              geom_ribbon(data=data_to_add_as_overlay,aes(x=as.Date(yday(paste0("2000","-",month_day)),"2000-01-01"),
                                                          ymin=!!sym(input$overlay_newData_lower_col),ymax=!!sym(input$overlay_newData_upper_col),
                                                          fill=input$overlay_newData_name),alpha=0.5)+
              scale_fill_manual(" ",labels=input$overlay_newData_name,values=c("grey80"="grey80"))
            }

            if(input$overlay_newData_longterm_col!=''){
            data_to_add_as_longterm <- overlay_data[c(input$overlay_newData_date_col,input$overlay_newData_longterm_col)]
            p1 <- p1 + geom_line(data=data_to_add_as_longterm,aes(x=as.Date(yday(paste0("2000","-",month_day)),"2000-01-01"),y=!!sym(input$overlay_newData_longterm_col),color="USGS long-term"),size=0.8,color="black")
            }

          p1 <- p1+
            scale_x_date(date_breaks="1 month",limits=c(as.Date("2000-01-01"),as.Date("2000-12-31")),date_labels = "%m%d")+
            labs(title=isolate(input$dailyStats_ts_overlay_title),x = "MonthDay",y =mean_col)+
            theme_classic()+
            theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                  ,panel.border = element_rect(colour="black",fill=NA, size=0.5)
                  ,plot.title = element_text(hjust=0.5)
                  ,plot.background = element_rect(color="grey20",size=2)
                  ,legend.position = "right"
                  ,axis.text.x=element_text(angle=45, hjust=1))
          #p1<- plotly::ggplotly(p1)
          print(p1)

        })
      }) # renderPlot close

    }else{
      if (!all(is.na(data_to_plot[,mean_col]))){

        output$plot_dailyStats_ts_overlay <- renderPlotly({
          p1 <- ggplot(data_to_plot,aes(x=as.Date(yday(Date),"2000-01-01"),y=!!sym(mean_col)))+
            geom_line(aes(colour=year),size=0.8)+
            scale_x_date(date_breaks="1 month",limits=c(as.Date("2000-01-01"),as.Date("2000-12-31")),date_labels = "%m%d")+
            labs(title=isolate(input$dailyStats_ts_overlay_title),x = "MonthDay",y =mean_col)+
            theme_classic()+
            theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                  ,panel.border = element_rect(colour="black",fill=NA, size=0.5)
                  ,plot.title = element_text(hjust=0.5)
                  ,plot.background = element_rect(color="grey20",size=2)
                  ,legend.position = "right"
                  ,axis.text.x=element_text(angle=45, hjust=1))
            ggplotly(p1,dynamicTicks = FALSE) %>% plotly::layout()
        })  # renderPlot close
      }else{
        shinyalert("Warning","No data available to plot for the selected variable!"
                   ,closeOnClickOutside = TRUE
                   ,closeOnEsc = TRUE
                   ,confirmButtonText="OK"
                   ,inputId = "alert_data_not_avail_for_ts")
      }##inner if else loop close
    } ## outer if else loop close

  }) ##observeEvent end

  #################  4:Boxplots << All parameters #################

  observeEvent(input$display_box, {

    output$display_box_plots <- renderUI({
      withSpinner(plotlyOutput("plot_dailyStats_box",height="600px",width="1200px"),type=2)
    })

    myList <- processed$processed_dailyStats
    variable_to_plot <- input$boxplot_variable_name
    myData <- myList[[which(names(myList)==variable_to_plot)]]
    mean_col <- paste0(input$boxplot_variable_name,".",input$boxplot_metrics)
    if(input$box_group=="year"){
      #myData[,input$box_group] <- format(myData[,"Date"],"%Y")
      myData[,input$box_group] <- format(as.Date(myData$Date, format="%Y-%m-%d %H:%M:%S"),"%Y")
    cols_selected = c("Date",input$box_group,mean_col)
    }else if(input$box_group=="month"){
      #myData[,input$box_group] <- format(myData[,"Date"],"%m")
      myData[,input$box_group] <- format(as.Date(myData$Date, format="%Y-%m-%d %H:%M:%S"),"%m")
    cols_selected = c("Date",input$box_group,mean_col)
    }else if(input$box_group=="season"){
    myData <- addSeason(myData)
    cols_selected = c("Date",input$box_group,mean_col)
    }else if(input$box_group=="month2"){
    #myData[,"year"] <- format(myData[,"Date"],"%Y")
    myData[,"year"] <- format(as.Date(myData$Date, format="%Y-%m-%d %H:%M:%S"),"%Y")
    #myData[,"month"] <- format(myData[,"Date"],"%m")
    myData[,"month"] <- format(as.Date(myData$Date, format="%Y-%m-%d %H:%M:%S"),"%m")
    cols_selected = c("Date","year","month",mean_col)
    }else if(input$box_group=="season2"){
    myData <- addSeason(myData)
    cols_selected = c("Date","year","season",mean_col)
    }

    data_to_plot <- myData[cols_selected]
    if (!all(is.na(data_to_plot[,mean_col]))&input$box_group!="month2"&input$box_group!="season2"){
    output$plot_dailyStats_box <- renderPlotly({

      p2 <- ggplot(data=data_to_plot,aes(x=!!sym(isolate(input$box_group)),y=!!sym(isolate(mean_col)))) +
        geom_boxplot()+
        labs(title=isolate(input$box_title),x = isolate(input$box_group),y = isolate(input$boxplot_variable_name))+
        theme_bw()+
        theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
              ,plot.title = element_text(hjust=0.5)
              ,axis.text.x = element_text(angle=0, hjust=1))
      p2 <- ggplotly(p2)
      print(p2)
    })
    } else if(!all(is.na(data_to_plot[,mean_col]))&input$box_group=="month2"){
      output$plot_dailyStats_box <- renderPlotly({

        p2 <- ggplot(data=data_to_plot,aes(x=month,y=!!sym(isolate(mean_col)),fill=year)) +
          geom_boxplot(position=position_dodge(width=0.1))+
          labs(title=isolate(input$box_title),x = "month",y = isolate(input$boxplot_variable_name))+
          theme_bw()+
          theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                ,plot.title = element_text(hjust=0.5)
                ,axis.text.x = element_text(angle=0, hjust=1))
        ggplotly(p2) %>% plotly::layout(boxmode="group")
      })
    } else if(!all(is.na(data_to_plot[,mean_col]))&input$box_group=="season2"){
      output$plot_dailyStats_box <- renderPlotly({

        data_to_plot$season = reorderSeason(data_to_plot$season)
        p2 <- ggplot(data=data_to_plot,aes(x=season,y=!!sym(isolate(mean_col)),fill=year)) +
          geom_boxplot(position=position_dodge(width=0.1))+
          labs(title=isolate(input$box_title),x = "season",y = isolate(input$boxplot_variable_name))+
          theme_bw()+
          theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                ,plot.title = element_text(hjust=0.5)
                ,axis.text.x = element_text(angle=0, hjust=1))
        ggplotly(p2) %>% plotly::layout(boxmode="group")
      })
    }else{
      shinyalert("Warning","No data available to plot for the selected variable!",closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                 confirmButtonText="OK",inputId = "alert_data_not_avail_for_box")
    }

  })  #observeEvent end

  ## close the alert messages
  observeEvent(input$alert_data_not_avail_for_box,{
    #print(input$alert_no_date)
    shinyjs::runjs("swal.close();")
  })


  #################  5:CDF << All parameters  #################

  observeEvent(input$run_CDF, {

    output$display_plot_CDF <- renderUI({
      withSpinner(plotlyOutput("plot_CDF",height="600px",width="1200px"),type=2)
    })


    myList <- processed$processed_dailyStats
    variable_to_plot <- input$CDF_variable_name
    myData.all <- myList[[which(names(myList)==variable_to_plot)]]

    if (input$CDF_select_year=="All"){
      myData <- myData.all
    }else{
      myData.all[,"year"] <- format(myData.all[,"Date"],"%Y")
      myData <- myData.all[myData.all$year==input$CDF_select_year,]
    }
    mean_col <- paste0(input$CDF_variable_name,".mean")
    if (input$CDF_shading=="quantiles"){
      upper_col <- paste0(input$CDF_variable_name,".q.75%")
      lower_col <- paste0(input$CDF_variable_name,".q.25%")
    }else if (input$CDF_shading=="minMax"){
      lower_col <- paste0(input$CDF_variable_name,".min")
      upper_col <- paste0(input$CDF_variable_name,".max")
    }
    cols_selected = c("Date",mean_col,lower_col,upper_col)
    data.plot <- myData[cols_selected]

    if (input$CDF_select_season=="All"){
      season.choice = NULL
    }else{
      season.choice = input$CDF_select_season
    }

    output$plot_CDF <- renderPlotly({

      # g <- ggplot(data=data.plot,aes(x=!!sym(mean_col)))+
      #      geom_step(stat="ecdf")
      # inside <- ggplot_build(g)
      # matched <- merge(inside$data[[1]],data.frame(x=data.plot[,names(data.plot)==mean_col]
      #                                              ,data.plot[,names(data.plot)==lower_col]
      #                                              ,data.plot[,names(data.plot)==upper_col]),by=("x"))
      # names(matched)[ncol(matched)] <- "data.plot.max"
      # names(matched)[ncol(matched)-1] <-"data.plot.min"
      # CDF_plot <- g+geom_ribbon(data=matched,aes(x=x,ymin=ecdf(data.plot.min)(x),ymax=ecdf(data.plot.max)(x)),alpha=0.5,fill="green")

      CDF_plot <- CompSiteCDF.updated(file.input = NULL
                                     , dir.input = getwd()
                                     , dir.output = getwd()
                                     , Param.Name = mean_col
                                     , Shaded.Names = c(lower_col,upper_col)
                                     , Plot.title = isolate(input$CDF_title)
                                     , Plot.season = isolate(season.choice)
                                     , hist.columnName = NULL
                                     , df.input = data.plot)
      CDF_plot <- ggplotly(CDF_plot) %>% plotly::layout(legend = list(orientation = "h", x = 0.4, y = -0.3))
      if (is.null(CDF_plot)){
        shinyalert("Warning","No data available to plot for the selected variable/year/season!",closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                   confirmButtonText="OK",inputId = "alert_data_not_avail_for_CDF")
      }
      print(CDF_plot)
    })


  }) # observeEvent close

  ## close the alert messages
  observeEvent(input$alert_data_not_avail_for_CDF,{
    #print(input$alert_no_date)
    shinyjs::runjs("swal.close();")
  })


  #################  6:Raster graphs << All parameters #################
  observeEvent(input$run_raster, {
    output$display_raster_graphs <- renderUI({
      withSpinner(plotOutput("plot_dailyStats_raster",height="550px",width="1200px"),type=2)
    })

    myMonth <- seq(as.Date("2020-01-01"),as.Date("2020-12-31"),by="1 month")
    month_numeric <- lubridate::yday(myMonth)/365*52+1
    month_label <- lubridate::month(myMonth,label=TRUE)
    myList <- processed$processed_dailyStats
    variable_to_plot <- input$dailyStats_raster_variable_name
    myData <- myList[[which(names(myList)==variable_to_plot)]]
    mean_col <- paste0(input$dailyStats_raster_variable_name,".",input$dailyStats_raster_metrics)
    cols_selected <- c("Date",mean_col)
    data_to_plot <- myData[cols_selected]
    data_to_plot[,"year"] <- format(data_to_plot[,"Date"],"%Y")
    if (input$raster_plot_color=="hcl"){
      colorV <- hcl.colors(12)
    }else if(input$raster_plot_color=="rainbow"){
      colorV <- rainbow(12)
    }else if(input$raster_plot_color=="terrain"){
      colorV <- terrain.colors(12)
    }else if (input$raster_plot_color=="heat"){
      colorV <- heat.colors(12)
    }else if (input$raster_plot_color=="topo"){
      colorV <- topo.colors(12)
    }
    #data_to_plot[,"yday"] <- lubridate::yday(as.Date(data_to_plot[,"Date"],format="%Y-%m-%d"))
    if (!all(is.na(data_to_plot[,mean_col]))){
    output$plot_dailyStats_raster <- renderPlot({
         p1 <- ggplot(data_to_plot,aes(x=as.Date(yday(Date),"2000-01-01"),y=year))+
           geom_raster(aes(fill=!!sym(mean_col)))+
           coord_equal()+
           scale_fill_gradientn(name=mean_col,na.value="white",colours=colorV)+
           scale_x_date(date_breaks="1 month",date_labels = "%b")+
           scale_colour_manual(values=NA)+
           labs(title=isolate(input$dailyStats_raster_title), x = "month",y = "year")+
           guides(color=guide_legend("No data",override.aes = list(fill="white")))+
           theme_classic()+
           theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                 ,panel.border = element_rect(colour="black",fill=NA, size=0.5)
                 ,plot.title = element_text(hjust=0.5)
                 ,aspect.ratio = isolate(input$raster_plot_aspect_ratio)
                 ,plot.background = element_rect(color="grey20",size=2)
                 ,legend.position = "right"
                 )
         #ggplotly(p1)
         print(p1)
       })  # renderPlot close

  }else{
    shinyalert("Warning","No data available to plot for the selected variable!"
               ,closeOnClickOutside = TRUE
               ,closeOnEsc = TRUE
               ,confirmButtonText="OK"
               ,inputId = "alert_data_not_avail_for_ts")
  }##inner if else loop close

  }) ##observeEvent end

  #################  1: Thermal Statistics << Temperature  #################

  observeEvent(input$display_thermal, {
    
      hide("help_text_thermal_statistics")
      output$display_thermal_table_1 <- renderUI({
        withSpinner(dataTableOutput("thermal_statistics_table_1"))
      })
    
      output$display_thermal_table_2 <- renderUI({
        dataTableOutput("thermal_statistics_table_2")
      })
    
      output$display_thermal_table_3 <- renderUI({
        dataTableOutput("thermal_statistics_table_3")
      })
    
      output$display_thermal_table_4 <- renderUI({
        dataTableOutput("thermal_statistics_table_4")
      })
    
      output$display_thermal_table_5 <- renderUI({
        withSpinner(dataTableOutput("thermal_statistics_table_5"))
      })
    
      myData <- uploaded_data()
      streamThermal_exported <- Export.StreamThermal(formated_raw_data$derivedDF
                                                     ,fun.col.SiteID = input$thermal_SiteID_name
                                                     ,fun.col.Date = "date.formatted"
                                                     ,fun.col.Temp = input$thermal_Temp_name
      )
    
      ##save(streamThermal_exported, file="test_streamThermal_exported.RData")
      ST.freq <- T_frequency(streamThermal_exported) %>% mutate_if(is.numeric,round,digits=2)
      ST.mag  <- T_magnitude(streamThermal_exported) %>% mutate_if(is.numeric,round,digits=2)
      ST.roc  <- T_rateofchange(streamThermal_exported) %>% mutate_if(is.numeric,round,digits=2)
      ST.tim  <- T_timing(streamThermal_exported) %>% mutate_if(is.numeric,round,digits=2)
      ST.var  <- T_variability(streamThermal_exported) %>% mutate_if(is.numeric,round,digits=2)
    
      processed$ST.freq <- ST.freq
      processed$ST.mag <- ST.mag
      # processed$ST.roc <- ST.roc
      # processed$ST.tim <- ST.tim
      # processed$ST.var <- ST.var
    
      thermal.statistics.table.options <- list(
        scrollX = TRUE, #allow user to scroll wide tables horizontally
        stateSave = FALSE,
        pageLength = 15,
        dom = 'Bt',
        buttons = list('copy','print',list(extend = 'collection',buttons = c('csv','excel','pdf'),text='Download')),
        columnDefs = list(list(className="dt-center",targets="_all"))
      )
    
      output$thermal_statistics_table_1 <- DT::renderDataTable({
       table.title.1 <- "Frequency"
        myTable <- DT::datatable(
          ST.freq,
          caption = htmltools::tags$caption(table.title.1,style="color:black;font-size:16px;font-weight:bold;text-align:left;"),
          extensions ="Buttons",
          rownames = FALSE,
          options = thermal.statistics.table.options
        ) # dataTable end
        print(myTable)
      })  # renderDT end
    
    
      output$thermal_statistics_table_2 <- DT::renderDataTable({
        table.title.2 <- "Magnitude"
        myTable <- DT::datatable(
          ST.mag,
          caption = htmltools::tags$caption(table.title.2,style="color:black;font-size:16px;font-weight:bold;text-align:left;"),
          extensions ="Buttons",
          rownames = FALSE,
          options = thermal.statistics.table.options
        ) # dataTable end
        print(myTable)
      })  # renderDT end
    
      output$thermal_statistics_table_3 <- DT::renderDataTable({
        table.title.3 <- "Rate of Change"
        myTable <- DT::datatable(
          ST.roc,
          caption = htmltools::tags$caption(table.title.3,style="color:black;font-size:16px;font-weight:bold;text-align:left;"),
          extensions ="Buttons",
          rownames = FALSE,
          options = thermal.statistics.table.options
        ) # dataTable end
        print(myTable)
      })  # renderDT end
    
      output$thermal_statistics_table_4 <- DT::renderDataTable({
        table.title.4 <- "Timing"
        myTable <- DT::datatable(
          ST.tim,
          caption = htmltools::tags$caption(table.title.4,style="color:black;font-size:16px;font-weight:bold;text-align:left;"),
          extensions ="Buttons",
          rownames = FALSE,
          options = thermal.statistics.table.options
        ) # dataTable end
        print(myTable)
      })  # renderDT end
    
      output$thermal_statistics_table_5 <- DT::renderDataTable({
        table.title.5 <- "Variability"
        myTable <- DT::datatable(
          ST.var,
          caption = htmltools::tags$caption(table.title.5,style="color:black;font-size:16px;font-weight:bold;text-align:left;"),
          extensions ="Buttons",
          rownames = FALSE,
          options = thermal.statistics.table.options
        ) # dataTable end
        print(myTable)
      })  # renderDT end
    
      require(XLConnect)
    
      # Descriptions
      #
      Desc.freq <- "Frequency metrics indicate numbers of days in months or seasons
      that key events exceed user-defined temperatures. "
      #
      Desc.mag <- "Magnitude metrics characterize monthly and seasonal averages and
      the maximum and minimum from daily temperatures as well as 3-, 7-, 14-, 21-,
      and 30-day moving averages for mean and maximum daily temperatures."
      #
      Desc.roc <- "Rate of change metrics include monthly and seasonal rate of
      change, which indicates the difference in magnitude of maximum and minimum
      temperatures divided by number of days between these events."
      #
      Desc.tim <- "Timing metrics indicate Julian days of key events including
      mean, maximum, and minimum temperatures; they also indicate Julian days of
      mean, maximum, and minimum values over moving windows of specified size."
      #
      Desc.var <- "Variability metrics summarize monthly and seasonal range in
      daily mean temperatures as well as monthly coefficient of variation of daily
      mean, maximum, and minimum temperatures. Variability metrics also include
      moving averages for daily ranges and moving variability in extreme
      temperatures, calculated from differences in average high and low
      temperatures over various time periods"
      #
      Group.Desc <- c(Desc.freq, Desc.mag, Desc.roc, Desc.tim, Desc.var)
      df.Groups <- as.data.frame(cbind(c("freq","mag","roc","tim","var")
                                       ,Group.Desc))
      SiteID <- processed$ST.freq[1,1]
      myDate <- format(Sys.Date(),"%Y%m%d")
      myTime <- format(Sys.time(),"%H%M%S")
    
      Notes.User <- Sys.getenv("USERNAME")
      Notes.Names <- c("Dataset (SiteID)", "Analysis.Date (YYYYMMDD)"
                       , "Analysis.Time (HHMMSS)", "Analysis.User")
      Notes.Data <- c(SiteID, myDate, myTime, Notes.User)
      df.Notes <- as.data.frame(cbind(Notes.Names, Notes.Data))
      ## New File Name
      fileName <- paste("StreamThermal"
                    , loaded_data$name
                    , SiteID
                    , myDate
                    , "xlsx"
                    , sep=".")
      ## Copy over template with Metric Definitions
      file.copy(file.path(path.package("ContDataQC")
                          ,"extdata"
                          ,"StreamThermal_MetricList.xlsx")
                , fileName)
      ## load workbook, create if not existing
      wb <- loadWorkbook(fileName, create = TRUE)
      # create sheets
      createSheet(wb, name = "NOTES")
      createSheet(wb, name = "freq")
      createSheet(wb, name = "mag")
      createSheet(wb, name = "roc")
      createSheet(wb, name = "tim")
      createSheet(wb, name = "var")
      # write to worksheet
      writeWorksheet(wb, df.Notes, sheet = "NOTES", startRow=1)
      writeWorksheet(wb, df.Groups, sheet="NOTES", startRow=10)
      writeWorksheet(wb, processed$ST.freq, sheet = "freq")
      writeWorksheet(wb, processed$ST.mag, sheet = "mag")
      writeWorksheet(wb, processed$ST.roc, sheet = "roc")
      writeWorksheet(wb, processed$ST.tim, sheet = "tim")
      writeWorksheet(wb, processed$ST.var, sheet = "var")
      # save workbook
      to_download$wb <- wb
      to_download$fileName <- fileName
  

  }) #observeEvent end

  ## when use actionButton to save thermal statistics to excel file

#   observeEvent(input$save_thermal, {
#     require(XLConnect)
#
#     # Descriptions
#     #
#     Desc.freq <- "Frequency metrics indicate numbers of days in months or seasons
# that key events exceed user-defined temperatures. "
#     #
#     Desc.mag <- "Magnitude metrics characterize monthly and seasonal averages and
# the maximum and minimum from daily temperatures as well as 3-, 7-, 14-, 21-,
# and 30-day moving averages for mean and maximum daily temperatures."
#     #
#     Desc.roc <- "Rate of change metrics include monthly and seasonal rate of
# change, which indicates the difference in magnitude of maximum and minimum
# temperatures divided by number of days between these events."
#     #
#     Desc.tim <- "Timing metrics indicate Julian days of key events including
# mean, maximum, and minimum temperatures; they also indicate Julian days of
# mean, maximum, and minimum values over moving windows of specified size."
#     #
#     Desc.var <- "Variability metrics summarize monthly and seasonal range in
# daily mean temperatures as well as monthly coefficient of variation of daily
# mean, maximum, and minimum temperatures. Variability metrics also include
# moving averages for daily ranges and moving variability in extreme
# temperatures, calculated from differences in average high and low
# temperatures over various time periods"
#     #
#     Group.Desc <- c(Desc.freq, Desc.mag, Desc.roc, Desc.tim, Desc.var)
#     df.Groups <- as.data.frame(cbind(c("freq","mag","roc","tim","var")
#                                      ,Group.Desc))
#     SiteID <- processed$ST.freq[1,1]
#     myDate <- format(Sys.Date(),"%Y%m%d")
#     myTime <- format(Sys.time(),"%H%M%S")
#     Notes.User <- Sys.getenv("USERNAME")
#
#     Notes.Names <- c("Dataset (SiteID)", "Analysis.Date (YYYYMMDD)"
#                      , "Analysis.Time (HHMMSS)", "Analysis.User")
#     Notes.Data <- c(SiteID, myDate, myTime, Notes.User)
#     df.Notes <- as.data.frame(cbind(Notes.Names, Notes.Data))
#     ## New File Name
#     if (!file.exists("Output/saved_streamThermal/")) dir.create(file.path("Output/saved_streamThermal"),showWarnings = FALSE, recursive = TRUE)
#     name_in_file <- loaded_data$name
#     myFile.XLSX <- paste("Output/saved_streamThermal/StreamThermal"
#                          , name_in_file
#                          , SiteID
#                          , myDate
#                          , myTime
#                          , "xlsx"
#                          , sep=".")
#     ## Copy over template with Metric Definitions
#     file.copy(file.path(path.package("ContDataQC")
#                         ,"extdata"
#                         ,"StreamThermal_MetricList.xlsx")
#               , myFile.XLSX)
#     ## load workbook, create if not existing
#     wb <- loadWorkbook(myFile.XLSX, create = TRUE)
#     # create sheets
#     createSheet(wb, name = "NOTES")
#     createSheet(wb, name = "freq")
#     createSheet(wb, name = "mag")
#     createSheet(wb, name = "roc")
#     createSheet(wb, name = "tim")
#     createSheet(wb, name = "var")
#     # write to worksheet
#     writeWorksheet(wb, df.Notes, sheet = "NOTES", startRow=1)
#     writeWorksheet(wb, df.Groups, sheet="NOTES", startRow=10)
#     writeWorksheet(wb, processed$ST.freq, sheet = "freq")
#     writeWorksheet(wb, processed$ST.mag, sheet = "mag")
#     writeWorksheet(wb, processed$ST.roc, sheet = "roc")
#     writeWorksheet(wb, processed$ST.tim, sheet = "tim")
#     writeWorksheet(wb, processed$ST.var, sheet = "var")
#     # save workbook
#     saveWorkbook(wb, myFile.XLSX)
#
#   }) # observeEvent close
#

  output$save_thermal <- downloadHandler(


    filename = function(){
      to_download$fileName
    },

    content = function(file){

    saveWorkbook(to_download$wb,file)

    }

  ) # downloadHandler close



  #################  2: Thermal Sensitivity << Temperature  #################
  observeEvent(input$exclude_data_points,{
    if (input$exclude_data_points == 'Yes'){
      shinyjs::show("cp_air_temp")
    }else{
      shinyjs::hide("cp_air_temp")
    }
  })


  observeEvent(input$display_thermal_sensitivity, {

    hide("help_text_air_water")

    output$display_thermal_sensitivity_plot_1 <- renderUI({
      withSpinner(plotOutput("thermal_sensitivity_plot_1"))
    })

    myList <- processed$processed_dailyStats
    ## check if both of "Air.Temp.C" and "Water.Temp.C" are available
    if(all(names(myList) %in% c(input$air_temp_name,input$water_temp_name))){
      myData.Air <- myList[[which(names(myList)==input$air_temp_name)]]
      myData.Water <- myList[[which(names(myList)==input$water_temp_name)]]
      mean_col_air <- paste0(input$air_temp_name,".mean")
      mean_col_water <- paste0(input$water_temp_name,".mean")
      data_air_to_plot <- myData.Air[c("Date",mean_col_air)]
      data_water_to_plot <- myData.Water[c("Date",mean_col_water)]
      data_to_plot <- merge(data_air_to_plot,data_water_to_plot,by="Date")
      if (input$exclude_data_points=="Yes"){
        data_to_plot <- data_to_plot[data_to_plot$Air.Temp.C.mean>input$air_limit_temp,]
      }
      data_to_model <- data_to_plot
      names(data_to_model)[match(mean_col_water,names(data_to_model))] <- "y"
      names(data_to_model)[match(mean_col_air,names(data_to_model))] <- "x"
      myModel <- lm(y ~ x,data_to_model,na.action=na.exclude)
      myEquation <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                               list(a = format(unname(coef(myModel)[1]), digits = 2),
                                    b = format(unname(coef(myModel)[2]), digits = 2),
                                    r2 = format(summary(myModel)$r.squared, digits = 3)))

      output$thermal_sensitivity_plot_1 <- renderPlot({
        p1 <- ggplot(data_to_plot,aes(x=!!sym(mean_col_air),y=!!sym(mean_col_water)))+
          geom_point(alpha=0.5,size=1.5)+
          geom_smooth(method="loess",se=FALSE,color="black")+
          geom_smooth(method="lm",se=FALSE,color="cornflowerblue",linetype="dashed",size=2)+
          geom_text(x=(min(data_to_plot[,mean_col_air],na.rm=TRUE)+5)
                    ,y=(max(data_to_plot[,mean_col_water],na.rm=TRUE)-1.5)
                    ,label=as.character(as.expression(myEquation))
                    ,color="cornflowerblue"
                    ,size=8
                    ,parse= TRUE)+
          labs(x = "Air Temperature",y = "Water Temperature")+
          theme_minimal()+
          theme(text=element_text(size=16,face = "bold", color="cornflowerblue"),
                plot.background = element_rect(color="grey20",size=2),
                legend.position = "right",
          )
        #ggplotly(p1)
        print(p1)
      })  # renderPlot close

    }else{
      shinyalert("Warning","We need both of air temperature and water temperature data to run thermal sensitivity. Please check."
                 ,closeOnClickOutside = TRUE,closeOnEsc = TRUE,confirmButtonText="OK",inputId = "alert_data_not_val_for_thermal")
    } ## outer if else loop close

  }) ## observeEvent end

  observeEvent(input$alert_data_not_val_for_thermal,{
    shinyjs::runjs("swal.close();")
  })


  #################  4: Thermal classification << Temperature  #################
  observeEvent(input$display_water_class, {
    hide("help_text_water_temp_class")

    output$display_water_temp_class_table <- renderUI({
      withSpinner(dataTableOutput("water_temp_class_table"))
    })
    myList <- processed$processed_dailyStats
    myData.Water <- myList[[which(names(myList)==input$water_temp_name_in_class)]]
    mean_col_water <- paste0(input$water_temp_name_in_class,".mean")
    data_water_to_calculate <- myData.Water[c("Date",mean_col_water)]
    #save(data_water_to_calculate,file="test_data_water_class.RData")
    ## calculate the July/August mean for each year
    all.years <- unique(format(data_water_to_calculate$Date,format="%Y"))
    #print(all.years)
    calculated.mean <- data.frame(matrix(ncol=3,nrow=0))

    for (i in 1:length(all.years)){
      year.now = all.years[i]
      to.select <- data_water_to_calculate$Date >= as.Date(paste0(year.now,"-07-01")) & data_water_to_calculate$Date <= as.Date(paste0(year.now,"-08-31"))
      mean.this.year <- mean(data_water_to_calculate[to.select,2],na.rm=TRUE)
      if(is.nan(mean.this.year)){
        class.this.year <- "NaN"
      }else if (mean.this.year<10){
        class.this.year <- "Very cold"
      }else if(mean.this.year>=10&mean.this.year<15){
        class.this.year <- "Cold"
      }else if(mean.this.year>=15&mean.this.year<18){
        class.this.year <- "Cold-cool"
      }else if(mean.this.year>=18&mean.this.year<21){
          class.this.year <- "Cool"
      }else if(mean.this.year>=21&mean.this.year<=24){
        class.this.year <- "Cool-warm"
      }else if(mean.this.year>24){
        class.this.year <- "Warm"
      }
      calculated.mean[i,] <- c(year.now,round(mean.this.year,digits=1),class.this.year)
    } # for loop end
    second_col_name <- paste0("Mean July/Aug water temperature(C)")
    colnames(calculated.mean) <- c("Year",second_col_name,"Class")

    output$water_temp_class_table <- DT::renderDataTable({
      myTable <- DT::datatable(
        calculated.mean,
        extensions ="Buttons",
        rownames = FALSE,
        options = list(
          scrollX = FALSE, #allow user to scroll wide tables horizontally
          stateSave = FALSE,
          pageLength = 15,
          dom = 'Bt',
          buttons = list('copy','print',list(extend = 'collection',buttons = c('csv','excel','pdf'),text='Download')),
          columnDefs = list(list(className="dt-center",targets="_all"))
        )
      ) # dataTable end
      print(myTable)
    })  ## renderDataTable ebd

  })  ##observeEvent end


  #################  1: IHA << Hydrology  ####
  observeEvent(input$display_IHA, {

    hide("help_text_IHA")

    output$display_IHA_table_1 <- renderUI({
      withSpinner(dataTableOutput("IHA_table_1"))
    })

    output$display_IHA_plot_button_1 <-renderUI({
      fluidRow(column(width=12,align="right",
                      actionButton(inputId="display_IHA_plot_1", label="Show/hide plot",class="btn btn-primary")
      )) # column and fluidRow close
    })

    output$display_IHA_table_2 <- renderUI({
      dataTableOutput("IHA_table_2")
    })

    output$display_IHA_plot_button_2 <-renderUI({
      fluidRow(column(width=12,align="right",
                      actionButton(inputId="display_IHA_plot_2", label="Show/hide plot",class="btn btn-primary")
      )) # column and fluidRow close
    })

    output$display_IHA_table_3 <- renderUI({
      dataTableOutput("IHA_table_3")
    })

    output$display_IHA_plot_button_3 <-renderUI({
      fluidRow(column(width=12,align="right",
                      actionButton(inputId="display_IHA_plot_3", label="Show/hide plot",class="btn btn-primary")
      )) # column and fluidRow close
    })

    output$display_IHA_table_4 <- renderUI({
      dataTableOutput("IHA_table_4")
    })

    output$display_IHA_plot_button_4 <-renderUI({
      fluidRow(column(width=12,align="right",
                      actionButton(inputId="display_IHA_plot_4", label="Show/hide plot",class="btn btn-primary")
      )) # column and fluidRow close
    })

    output$display_IHA_table_5 <- renderUI({
      dataTableOutput("IHA_table_5")
    })

    output$display_IHA_plot_button_5 <-renderUI({
      fluidRow(column(width=12,align="right",
                      actionButton(inputId="display_IHA_plot_5", label="Show/hide plot",class="btn btn-primary")
      )) # column and fluidRow close
    })

    myList <- processed$processed_dailyStats

    if (length(myList)>0){
    variable_to_IHA <- input$parameter_name
    myData <- myList[[which(names(myList)==variable_to_IHA)]]
    mean_col <- paste0(input$parameter_name,".mean")
    myData <- myData[c('Date',mean_col)]
    myData.IHA<- read.zoo(myData,format="%Y-%m-%d")
    processed$myData.IHA <- myData.IHA
    }else{
    myData <- uploaded_data()
    print(paste0("the file name is:",loaded_data$name))
    }

    IHA.table.options <- list(
      scrollX = TRUE, #allow user to scroll wide tables horizontally
      stateSave = FALSE,
      pageLength = 15,
      dom = 'Bt',
      buttons = list('copy','print',list(extend = 'collection',buttons = c('csv','excel','pdf'),text='Download')),
      columnDefs = list(list(className="dt-center",targets="_all"))
    )

    myYr <- "calendar"
    ## IHA parameters group 1; Magnitude of monthly water conditions
    Analysis.Group.1 <- group1(myData.IHA, year=myYr)
    #save(Analysis.Group.1,file="IHA_group_1.RData")
    Analysis.Group.1 <- as.data.frame(Analysis.Group.1) %>% mutate_if(is.numeric,round,digits=2)
    processed$IHA.group.1 <- Analysis.Group.1

    output$IHA_table_1 <- DT::renderDataTable({
      table.title.1 <- "Group 1: Magnitude of monthly water conditions"
      myTable <- DT::datatable(
        Analysis.Group.1,
        caption = htmltools::tags$caption(table.title.1,style="color:black;font-size:16px;font-weight:bold;text-align:left;"),
        extensions ="Buttons",
        rownames = TRUE,
        options = IHA.table.options
      ) # dataTable end
      print(myTable)
    })  # renderDT end


   ## IHA parameters group 2: Magnitude of monthly water condition and include 12 parameters
    Analysis.Group.2 <- group2(myData.IHA, year=myYr)
    #save(Analysis.Group.2,file="IHA_group_2.RData")
    Analysis.Group.2 <- as.data.frame(Analysis.Group.2) %>% mutate_if(is.numeric,round,digits=2)
    processed$IHA.group.2 <- Analysis.Group.2
    output$IHA_table_2 <- DT::renderDataTable({
      table.title.1 <- "Group 2: Magnitude of monthly water condition and include 12 parameters"
      myTable <- DT::datatable(
        Analysis.Group.2,
        caption = htmltools::tags$caption(table.title.1,style="color:black;font-size:16px;font-weight:bold;text-align:left;"),
        extensions ="Buttons",
        rownames = FALSE,
        options = IHA.table.options
      ) # dataTable end
      print(myTable)
    })  # renderDT end

    ## IHA parameters group 3:Timing of annual extreme water conditions
    Analysis.Group.3 <- group3(myData.IHA, year=myYr)
    #save(Analysis.Group.3,file="IHA_group_3.RData")
    processed$IHA.group.3 <- Analysis.Group.3
    output$IHA_table_3 <- DT::renderDataTable({
      table.title.3 <- "Group 3: Timing of annual extreme water conditions"
      myTable <- DT::datatable(
        Analysis.Group.3,
        caption = htmltools::tags$caption(table.title.3,style="color:black;font-size:16px;font-weight:bold;text-align:left;"),
        extensions ="Buttons",
        rownames = TRUE,
        options = IHA.table.options
      ) # dataTable end
      print(myTable)
    })  # renderDT end

    ## IHA parameters group 4; Frequency and duration of high and low pulses
    # defaults to 25th and 75th percentiles
    Analysis.Group.4 <- group4(myData.IHA, year=myYr)
    #save(Analysis.Group.4,file="IHA_group_4.RData")
    processed$IHA.group.4 <- Analysis.Group.4
    output$IHA_table_4 <- DT::renderDataTable({
      table.title.4 <- "Group 4: Frequency and duration of high and low pulses"
      myTable <- DT::datatable(
        Analysis.Group.4,
        caption = htmltools::tags$caption(table.title.4,style="color:black;font-size:16px;font-weight:bold;text-align:left;"),
        extensions ="Buttons",
        rownames = TRUE,
        options = IHA.table.options
      ) # dataTable end
      print(myTable)
    })  # renderDT end


    ## IHA parameters group 5; Rate and frequency of water condition changes
    Analysis.Group.5 <- group5(myData.IHA, year=myYr)
    #save(Analysis.Group.5,file="IHA_group_5.RData")
    processed$IHA.group.5 <- Analysis.Group.5
    Analysis.Group.5 <- as.data.frame(Analysis.Group.5) %>% mutate_if(is.numeric,round,digits=2)
    output$IHA_table_5 <- DT::renderDataTable({
      table.title.5 <- "Group 5: Rate and frequency of water condition changes"
      myTable <- DT::datatable(
        Analysis.Group.5,
        caption = htmltools::tags$caption(table.title.5,style="color:black;font-size:16px;font-weight:bold;text-align:left;"),
        extensions ="Buttons",
        rownames = TRUE,
        options = IHA.table.options
      ) # dataTable end
      print(myTable)
    })  # renderDT end

    ## create Excel Workbook
    require(XLConnect)
    Group.Desc <- c("Magnitude of monthly water conditions"
                    ,"Magnitude of monthly water condition and include 12 parameters"
                    ,"Timing of annual extreme water conditions"
                    ,"Frequency and duration of high and low pulses"
                    ,"Rate and frequency of water condition changes")
    df.Groups <- as.data.frame(cbind(paste0("Group",1:5),Group.Desc))
    myDate <- format(Sys.Date(),"%Y%m%d")
    Notes.User <- Sys.getenv("USERNAME")
    myYr <- "calendar"
    Notes.Names <- c("Dataset (SiteID)","IHA.Year","Analysis.Date (YYYYMMDD)"
                     ,"Analysis.Time (HHMMSS)","Analysis.User")
    Notes.Data <- c(loaded_data$name, myYr, myDate, Notes.User)
    df.Notes <- as.data.frame(cbind(Notes.Names,Notes.Data))
    # Open/Create file
    myFile.XLSX <- paste("IHA",loaded_data$name, myYr, myDate, "xlsx", sep=".")
    Notes.Summary <- summary(processed$myData.IHA)
    wb <- loadWorkbook(myFile.XLSX, create = TRUE) # load workbook, create if not existing
    # create sheets
    createSheet(wb, name = "NOTES")
    createSheet(wb, name = "Group1")
    createSheet(wb, name = "Group2")
    createSheet(wb, name = "Group3")
    createSheet(wb, name = "Group4")
    createSheet(wb, name = "Group5")
    # write to worksheet
    writeWorksheet(wb, df.Notes, sheet = "NOTES", startRow=1)
    writeWorksheet(wb, Notes.Summary, sheet = "NOTES", startRow=10)
    writeWorksheet(wb, df.Groups, sheet="NOTES", startRow=25)
    writeWorksheet(wb, processed$IHA.group.1, sheet = "Group1",rownames=c("year",rownames(processed$IHA.group.1)))
    writeWorksheet(wb, processed$IHA.group.2, sheet = "Group2")
    writeWorksheet(wb, processed$IHA.group.3, sheet = "Group3",rownames=c("year",rownames(processed$IHA.group.3)))
    writeWorksheet(wb, processed$IHA.group.4, sheet = "Group4",rownames=c("year",rownames(processed$IHA.group.4)))
    writeWorksheet(wb, processed$IHA.group.5, sheet = "Group5",rownames=c("year",rownames(processed$IHA.group.5)))
    to_download$wb_IHA <- wb
    to_download$fileName_IHA <- myFile.XLSX

  }) #observeEvent end


  ### changed the actionButton "save_IHA" to downloadButton

  # observeEvent(input$save_IHA, {
  #   require(XLConnect)
  #   if (!file.exists("Output/saved_IHA/")) dir.create(file.path("Output/saved_IHA"),showWarnings = FALSE, recursive = TRUE)
  #
  #   Group.Desc <- c("Magnitude of monthly water conditions"
  #                   ,"Magnitude of monthly water condition and include 12 parameters"
  #                   ,"Timing of annual extreme water conditions"
  #                   ,"Frequency and duration of high and low pulses"
  #                   ,"Rate and frequency of water condition changes")
  #   df.Groups <- as.data.frame(cbind(paste0("Group",1:5),Group.Desc))
  #   myDate <- format(Sys.Date(),"%Y%m%d")
  #   myTime <- format(Sys.time(),"%H%M%S")
  #   Notes.User <- Sys.getenv("USERNAME")
  #   myYr <- "calendar"
  #   Notes.Names <- c("Dataset (SiteID)","IHA.Year","Analysis.Date (YYYYMMDD)"
  #                    ,"Analysis.Time (HHMMSS)","Analysis.User")
  #   Notes.Data <- c(loaded_data$name, myYr, myDate, myTime, Notes.User)
  #   df.Notes <- as.data.frame(cbind(Notes.Names,Notes.Data))
  #   # Open/Create file
  #   myFile.XLSX <- paste("Output/saved_IHA/IHA",loaded_data$name, myYr, myDate, myTime, "xlsx", sep=".")
  #   Notes.Summary <- summary(processed$myData.IHA)
  #   wb <- loadWorkbook(myFile.XLSX, create = TRUE) # load workbook, create if not existing
  #   # create sheets
  #   createSheet(wb, name = "NOTES")
  #   createSheet(wb, name = "Group1")
  #   createSheet(wb, name = "Group2")
  #   createSheet(wb, name = "Group3")
  #   createSheet(wb, name = "Group4")
  #   createSheet(wb, name = "Group5")
  #   # write to worksheet
  #   writeWorksheet(wb, df.Notes, sheet = "NOTES", startRow=1)
  #   writeWorksheet(wb, Notes.Summary, sheet = "NOTES", startRow=10)
  #   writeWorksheet(wb, df.Groups, sheet="NOTES", startRow=25)
  #   writeWorksheet(wb, processed$IHA.group.1, sheet = "Group1",rownames=c("year",rownames(processed$IHA.group.1)))
  #   writeWorksheet(wb, processed$IHA.group.2, sheet = "Group2")
  #   writeWorksheet(wb, processed$IHA.group.3, sheet = "Group3",rownames=c("year",rownames(processed$IHA.group.3)))
  #   writeWorksheet(wb, processed$IHA.group.4, sheet = "Group4",rownames=c("year",rownames(processed$IHA.group.4)))
  #   writeWorksheet(wb, processed$IHA.group.5, sheet = "Group5",rownames=c("year",rownames(processed$IHA.group.5)))
  #   # save workbook
  #   saveWorkbook(wb, myFile.XLSX)
  # })# observeEvent end

  output$save_IHA <- downloadHandler(

    filename = function(){
      to_download$fileName_IHA
    },

    content = function(file){
      saveWorkbook(to_download$wb_IHA,file)
    }

  )

  observeEvent(input$display_IHA_plot_1, {
    output$IHA_plot_1 <- renderUI({
      plotOutput("IHA_plot_1_to_show")
    })

    if(input$display_IHA_plot_1 %% 2 !=0){

       shinyjs::show("IHA_plot_1_panel")
       data_to_plot <- cbind(rownames(processed$IHA.group.1),data.frame(processed$IHA.group.1,row.names = NULL))
       colnames(data_to_plot)[1]<-'Year'
       data_for_plot_1 <- data_to_plot %>% gather(key,value,-Year) ## convert into long format
       data_for_plot_1$Year <- as.numeric(as.character(data_for_plot_1$Year))
       data_for_plot_1$key <- factor(data_for_plot_1$key, levels = c("January","February","March","April","May",
                                                                   "June","July","August","September","October","November","December"))
       output$IHA_plot_1_to_show <- renderPlot({
       p1 <- ggplot(data_for_plot_1)+
             geom_line(aes(x=Year,y=value,colour=key),size=0.8,linetype="dashed")+
             labs(x = "Year",y = paste0("Magnitude of monthly water conditions"),color="Month")+
             theme_minimal()+
             scale_x_continuous(breaks=unique(data_for_plot_1$Year),labels = unique(data_for_plot_1$Year))+
             theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                   ,plot.background = element_rect(color="grey20",size=2)
                   ,legend.position = "right"
                   )
       print(p1)

      }) # renderPlot end
    }else{
      shinyjs::hide("IHA_plot_1_panel")
    } # if else loop end
  }) #observeEvent to display IHA plot 1 end

  observeEvent(input$display_IHA_plot_2, {
    output$IHA_plot_2a <- renderUI({
      plotOutput("IHA_plot_2a_to_show")
    })

    output$IHA_plot_2b <- renderUI({
      plotOutput("IHA_plot_2b_to_show")
    })

    if(input$display_IHA_plot_2 %% 2 !=0){

      shinyjs::show("IHA_plot_2_panel")
      data_to_plot <- processed$IHA.group.2[,1:11]
      column_names <- colnames(data_to_plot)
      min_cols_to_select <- c("year",column_names[str_detect(column_names,"Min")])
      max_cols_to_select <- c("year",column_names[str_detect(column_names,"Max")])
      data_to_plot_min <- data_to_plot[min_cols_to_select]
      data_to_plot_max <- data_to_plot[max_cols_to_select]
      data_for_plot_2a <- data_to_plot_min %>% gather(key,value,-year) ## convert into long format
      data_for_plot_2b <- data_to_plot_max %>% gather(key,value,-year) ## convert into long format
      output$IHA_plot_2a_to_show <- renderPlot({
        p1 <- ggplot(data_for_plot_2a,aes(x=year,y=value,fill=key))+
              geom_bar(stat="identity",position="dodge")+
              labs(x = "Year",y = paste0("Magnitude of water condition"),fill="parameters")+
          theme_minimal()+
          scale_x_continuous(breaks=unique(data_for_plot_2a$year),labels = unique(data_for_plot_2a$year))+
          theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                ,plot.background = element_rect(color="grey20",size=2)
                ,legend.position = "right"
          )
        print(p1)

      }) # renderPlot end

      output$IHA_plot_2b_to_show <- renderPlot({
        p2 <- ggplot(data_for_plot_2b,aes(x=year,y=value,fill=key))+
          geom_bar(stat="identity",position="dodge")+
          labs(x = "Year",y = paste0("Magnitude of water condition"),fill="parameters")+
          theme_minimal()+
          scale_x_continuous(breaks=unique(data_for_plot_2b$year),labels = unique(data_for_plot_2b$year))+
          theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                ,plot.background = element_rect(color="grey20",size=2)
                ,legend.position = "right"
          )
        print(p2)

      }) # renderPlot end
    }else{
      shinyjs::hide("IHA_plot_2_panel")
    } # if else loop end
  }) #observeEvent to display IHA plot 2 end


  observeEvent(input$display_IHA_plot_3, {
    output$IHA_plot_3 <- renderUI({
      plotOutput("IHA_plot_3_to_show")
    })

    if(input$display_IHA_plot_3 %% 2 !=0){

      shinyjs::show("IHA_plot_3_panel")
      data_to_plot <- cbind(rownames(processed$IHA.group.3),data.frame(processed$IHA.group.3,row.names = NULL))
      colnames(data_to_plot)[1]<-'year'
      data_for_plot_3 <- data_to_plot %>% gather(key,value,-year) ## convert into long format
      data_for_plot_3$year <- as.numeric(as.character(data_for_plot_3$year))
      output$IHA_plot_3_to_show <- renderPlot({
        p1 <- ggplot(data_for_plot_3,aes(x=year,y=value,fill=key))+
          geom_bar(stat="identity",position="dodge")+
          labs(x = "Year",y = paste0("Julian days"),fill="parameters")+
          theme_minimal()+
          scale_x_continuous(breaks=unique(data_for_plot_3$year),labels = unique(data_for_plot_3$year))+
          theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                ,plot.background = element_rect(color="grey20",size=2)
                ,legend.position = "right"
          )
        print(p1)

      }) # renderPlot end
    }else{
      shinyjs::hide("IHA_plot_3_panel")
    } # if else loop end
  }) #observeEvent to display IHA plot 3 end


  observeEvent(input$display_IHA_plot_4, {
    output$IHA_plot_4a <- renderUI({
      plotOutput("IHA_plot_4a_to_show")
    })

    output$IHA_plot_4b <- renderUI({
      plotOutput("IHA_plot_4b_to_show")
    })

    if(input$display_IHA_plot_4 %% 2 !=0){

      shinyjs::show("IHA_plot_4_panel")
      data_to_plot <- cbind(rownames(processed$IHA.group.4),data.frame(processed$IHA.group.4,row.names = NULL))
      colnames(data_to_plot)[1]<-'year'
      column_names <- colnames(data_to_plot)
      number_cols_to_select <- c("year",column_names[str_detect(column_names,"number")])
      length_cols_to_select <- c("year",column_names[str_detect(column_names,"length")])
      data_to_plot_number <- data_to_plot[number_cols_to_select]
      data_to_plot_length <- data_to_plot[length_cols_to_select]
      data_for_plot_4a <- data_to_plot_number %>% gather(key,value,-year) ## convert into long format
      data_for_plot_4b <- data_to_plot_length %>% gather(key,value,-year) ## convert into long format
      data_for_plot_4a$year <- as.numeric(as.character(data_for_plot_4a$year))
      data_for_plot_4b$year <- as.numeric(as.character(data_for_plot_4b$year))

      output$IHA_plot_4a_to_show <- renderPlot({
        p1 <- ggplot(data_for_plot_4a,aes(x=year,y=value,fill=key))+
          geom_bar(stat="identity",position="dodge")+
          labs(x = "Year",y = paste0("Frequency"),fill="parameters")+
          theme_minimal()+
          scale_x_continuous(breaks=unique(data_for_plot_4a$year),labels = unique(data_for_plot_4a$year))+
          theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                ,plot.background = element_rect(color="grey20",size=2)
                ,legend.position = "right"
          )
        print(p1)

      }) # renderPlot end

      output$IHA_plot_4b_to_show <- renderPlot({
        p2 <- ggplot(data_for_plot_4b,aes(x=year,y=value,fill=key))+
          geom_bar(stat="identity",position="dodge")+
          labs(x = "Year",y = paste0("Duration"),fill="parameters")+
          theme_minimal()+
          scale_x_continuous(breaks=unique(data_for_plot_4b$year),labels = unique(data_for_plot_4b$year))+
          theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                ,plot.background = element_rect(color="grey20",size=2)
                ,legend.position = "right"
          )
        print(p2)

      }) # renderPlot end
    }else{
      shinyjs::hide("IHA_plot_4_panel")
    } # if else loop end
  }) #observeEvent to display IHA plot 4 end

  observeEvent(input$display_IHA_plot_5, {
    output$IHA_plot_5 <- renderUI({
      plotOutput("IHA_plot_5_to_show")
    })

    if(input$display_IHA_plot_5 %% 2 !=0){

      shinyjs::show("IHA_plot_5_panel")
      data_to_plot <- cbind(rownames(processed$IHA.group.5),data.frame(processed$IHA.group.5,row.names = NULL))
      colnames(data_to_plot)[1]<-'Year'
      data_to_plot <- data_to_plot[,1:3]
      data_for_plot_1 <- data_to_plot %>% gather(key,value,-Year) ## convert into long format
      data_for_plot_1$Year <- as.numeric(as.character(data_for_plot_1$Year))

      output$IHA_plot_5_to_show <- renderPlot({
        p1 <- ggplot(data_for_plot_1)+
          geom_line(aes(x=Year,y=value,colour=key),size=0.8,linetype="dashed")+
          labs(x = "Year",y = paste0("Rate"),color="Month")+
          theme_minimal()+
          scale_x_continuous(breaks=unique(data_for_plot_1$Year),labels = unique(data_for_plot_1$Year))+
          theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                ,plot.background = element_rect(color="grey20",size=2)
                ,legend.position = "right"
          )
        print(p1)

      }) # renderPlot end
    }else{
      shinyjs::hide("IHA_plot_5_panel")
    } # if else loop end
  }) #observeEvent to display IHA plot 5 end


  ################### "Create report"####

  observeEvent(input$createReport,{

    showModal(modalDialog("Creating the report now...",footer=NULL))

    #if (!file.exists("Output/reports")) dir.create(file.path("Output/reports"),showWarnings = FALSE, recursive = TRUE)
    ## copy template file to a temporary directory so that the output file will be saved there
    ## in case user do not have written permission to the app directory when deployed
    tempRMD <- file.path(tempdir(),"SiteSummary.Rmd")
    file.copy("_moved/SiteSummary.Rmd",tempRMD,overwrite= TRUE)
    build_summary_updated(dir_data="Output/to_report/"
                  ,file_main="_Captions_SiteX.xlsx"
                  ,sheet_main="metadata"
                  ,file_prefix_sep="_"
                  ,rmd_template=tempRMD
                  ,output_format=input$report_format
                  ,output_file=paste0(input$report_name,".",input$report_format))

    to_download$fileName_report <- paste0(tempdir(),"/",input$report_name,".",input$report_format)
    print(to_download$fileName_report)
    removeModal()

  })


  output$downloadReport <- downloadHandler(

    filename = function(){
      to_download$fileName_report
    },
    content = function(file){
      file.copy(to_download$fileName_report,file)
    }
  )

  calculate_time_range <- function(baseData) {
    time_range <- difftime(max(as.POSIXct(baseData$Date,format="%Y-%m-%d"),na.rm = TRUE),min(as.POSIXct(baseData$Date,format="%Y-%m-%d"),na.rm = TRUE),units="days")
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
  
  # calculate_time_range_dis <- function(baseData) {
  #   #modified for continuous and discreate data
  #   baseMin <- min(as.Date(baseData$Date), na.rm = TRUE)
  #   baseMax <- max(as.Date(baseData$Date), na.rm = TRUE)
  # 
  #   disMin <- min(as.Date(baseData$discrete_Date), na.rm = TRUE)
  #   disMax <- max(as.Date(baseData$discrete_Date), na.rm = TRUE)
  # 
  #   newMin <- max(baseMin, disMin)
  #   newMax <- max(baseMax, disMax)
  # 
  # 
  #   time_range <- difftime(max(as.POSIXct(newMax,format="%Y-%m-%d %H:%M:%S"),na.rm = TRUE),min(as.POSIXct(newMin,format="%Y-%m-%d %H:%M:%S"),na.rm = TRUE),units="days")
  #   time_range <- difftime(max(as.POSIXct(baseData$Date,format="%Y-%m-%d %H:%M:%S"),na.rm = TRUE),min(as.POSIXct(baseData$Date,format="%Y-%m-%d %H:%M:%S"),na.rm = TRUE),units="days")
  #   if (as.numeric(time_range)<365*2){
  #     myBreaks = paste0(1," months")
  #     x_date_label = "%Y-%m-%d"
  #     return(list(myBreaks, x_date_label))
  #   }else if(as.numeric(time_range)>=365*2&as.numeric(time_range)<365*5){
  #     myBreaks = paste0(2," months")
  #     x_date_label = "%Y-%m-%d"
  #     return(list(myBreaks, x_date_label))
  #   }else{
  #     myBreaks = paste0(6," months")
  #     x_date_label = "%Y-%m"
  #     return(list(myBreaks, x_date_label))
  #   }
  # }
  
 
  
   getMapTitle <- function(shandingName, userTitle, lowerColumn, upperColumn) {
    shadingText <- " \n <span style='font-size:10px'>(Shading between daily 25th percentiles and 75th percentiles)</span>"
    if (shandingName=="quantiles"){
      shadingText <- " \n <span style='font-size:10px'>(Shading between daily 25th percentiles and 75th percentiles)</span>"
    } else if (shandingName=="minMax"){
      shadingText <- " \n <span style='font-size:10px'>(Shading between daily minimum and maximum values)</span>"
    } else if (shandingName=="dynamic"){
      shadingText <- paste("\n <span style='font-size:10px'>(Shading between" , lowerColumn, "and", upperColumn, ")</span>", sep=" ")
    }
    
    if(userTitle != "") {
      shading_text = paste0(userTitle, shadingText)
    } else {
      shading_text =  paste0("Uploaded File metrics", shadingText)
    }
    return(shading_text)
  }
  
  changeDateFormat <- function(uploadedDate, uploadedFormat) {
    newFormat = ""
    print(nchar(uploadedFormat))
    if(nchar(uploadedFormat) > 8) {
      newFormat = substr(uploadedFormat,1,8)
    } else {
      newFormat = uploadedFormat
    }
      tryCatch({   
        tmpDate <- as.Date(uploadedDate, newFormat)
        tmpDateChar = format(tmpDate, format="%Y-%m-%d")
        print(tmpDateChar)
        },error = function(err) {FALSE})  
  }
  
  display_gage_stats <- function() {
    gageStatsPlot <- NULL
    if(input$gage_id != "" && length(input$gage_id) > 0 && nrow(gageRawData$gagedata) > 0) {
      gageStatsPlot <-  fun.gageStatsPlot (
                                          fun.gage.id = input$gage_id,
                                          fun.gage.data = gageRawData$gagedata,
                                          fun.gage.vars.to.process = input$gaze_params,
                                          fun.stats.column = input$dailyStats_ts_metrics,
                                          fun.internal = TRUE
                                        )
      return(gageStatsPlot)
    } else {
      if (input$gage_id != "" & input$gage_id > 0 & nrow(gageRawData$gagedata) == 0) {
        shinyAlertUI("common_alert_msg", noGagaDataDownloaded, "WARNING")
      }
    }
  }

  
  observeEvent(input$display_gage_raw, {
    if (input$gage_id != "" & length(input$gage_id) > 0 & nrow(gageRawData$gagedata) > 0 & length(input$gaze_params) > 0) {
       gageRawPlot <- fun.gageRawPlot(fun.gage.data = gageRawData$gagedata,
                             fun.gage.vars.to.process = input$gaze_params,
                             fun.internal = TRUE)

      output$display_downloaded_data <- renderPlotly({
        ggplotly(gageRawPlot, height = calulatePlotHeight(length(input$gaze_params) * 2)) %>% plotly::layout(legend = list(orientation = "h", x = 0.4, y = -0.3))
      })
      overridePotlyStyle("display_downloaded_data")
    } else {
      if (input$gage_id != "" & input$gage_id > 0 & nrow(gageRawData$gagedata) == 0) {
        shinyAlertUI("common_alert_msg", noGagaDataDownloaded, "WARNING")
      } else if (input$gage_id != "" & input$gage_id > 0 & length(input$gaze_params) == 0){
        shinyAlertUI("common_alert_msg", selectGageVars, "WARNING")
      } else if (input$gage_id == "") {
        shinyAlertUI("common_alert_msg", noGageIdFound, "WARNING")
      }
    }
  })
  
  
  observeEvent(input$get_daymet_data, {
    Sys.sleep(0.5)
    if (input$daymet_lat != "" && length(input$daymet_lat) > 0 && input$daymet_long != "" && length(input$daymet_long) > 0) {

      withProgress(message = "Getting DayMet data", value = 0, {
        incProgress(0, detail = paste("Retrieving records for Latitude and Longitude ", input$daymet_lat, input$daymet_long))
        
        startYear <- format(dateRange$min, format='%Y')
        endYear <- format(dateRange$max, format='%Y')

        #Actually gets the gage data from the USGS NWIS system
        rawResult <- fun.dayMetData(
                               fun.lat <- input$daymet_lat,
                               fun.lon <- input$daymet_long,
                               fun.year.start <-  as.numeric(startYear),
                               fun.year.end <-  as.numeric(endYear),
                               fun.internal <-  TRUE
                               )
        

        dayMetRawData$dayMetData <- rawResult$dayMetData
        dyametCols <- rawResult$daymetColumns
        dayMetRawData$daymetColumns <- rawResult$daymetColumns
        updateSelectizeInput(session, 'daymet_params', choices = dyametCols, selected = dyametCols[1])
        print(dayMetRawData$dayMetData)
        shinyjs::show(id="daymetVarsDiv")
       
        #Fills in the progress bar once the operation is complete
        incProgress(1/1, detail = paste("Retrieved records for Latitude and Longitude ",input$daymet_lat, input$daymet_long))
      })
    } else {
      shinyAlertUI("common_alert_msg", noDaymetDataDownloaded, "WARNING")
    }
    
  })
  
  observeEvent(input$display_daymet_raw, {
    dayMetPlotRaw <- draw_daymet_raw("DayMet Raw Data Plot")
    if (!is.null(dayMetPlotRaw) & length(input$daymet_params) > 0) {
      #output$display_time_series_3 <- renderPlotly({
      output$display_downloaded_data <- renderPlotly({
        ggplotly(dayMetPlotRaw, height = calulatePlotHeight(length(input$daymet_params) * 2)) %>% plotly::layout(legend = list(orientation = "h", x = 0.4, y = -0.3))
      })
      overridePotlyStyle("display_daymet_data")
    } else {
      alertMsg <- NULL
      if (is.null(dayMetRawData$dayMetData)) {
        alertMsg <- downlaodDaymetData
      } else if(!is.null(dayMetRawData$dayMetData) & length(input$daymet_params) == 0) {
        alertMsg <- selectDaymetVars
      }
      shinyAlertUI("common_alert_msg", alertMsg, "WARNING")
    }
 }) 
  

  
 draw_uploaded_file_stats <- function() {
   basePlot <- NULL
   mainMapTitle <- getMapTitle(input$dailyStats_shading, input$dailyStats_ts_title, lowerColumn = NULL, upperColumn = NULL)
   variable_to_plot <- input$dailyStats_ts_variable_name 
   statsCols <- paste(variable_to_plot, input$dailyStats_ts_metrics, sep=".")
   statsList <- processed$processed_dailyStats
   mainData <- Reduce(full_join, statsList)
   myData  <- mainData %>% select(statsCols, "Date") %>%
     gather(key = "parameter", value = "value", -Date)
   
   main_range = calculate_time_range(as.list(myData))
   mainBreaks = main_range[[1]]
   main_x_date_label = main_range[[2]]
   
   basePlot <- ggplot(data = myData, aes(x=as.POSIXct(Date,format="%Y-%m-%d"), y = value)) +
     geom_line(aes(colour=parameter)) +
     #scale_x_datetime(date_labels="%Y-%m-%d",date_breaks=paste0(1," month"))+
     scale_x_datetime(date_labels=main_x_date_label,date_breaks=mainBreaks)+  
     labs(title=mainMapTitle, x="Date", y="Parameters")+
     theme_bw()+
     facet_grid(parameter ~ ., scales = "free_y")+
     theme(
       strip.background = element_blank()
       ,strip.text.y = element_blank()
       #,strip.text.y = element_text(angle = 35)
       ,strip.placement = "outside"
       ,text=element_text(size=10,face = "bold", color="cornflowerblue")
       ,plot.title = element_text(hjust=0.5)
       ,legend.position="bottom"
       ,axis.text.x=element_text(angle=65, hjust=10)
     )
   return(basePlot)
   
 } 

 # draw_new_updated_file_stats <- function(renderStatus=FALSE){
 #   mainPlot <- NULL
 # 
 #   if (validateUserInputs(
 #     dateColumnNums="dtNumOfCols1",
 #     parmToProcess="parameters_to_process2_new",
 #     dateFieldNameId="selectedDateFieldName_new",
 #     dateFormatId="selectedDateFormat_new",
 #     timeFieldNameId="selectedTimeFieldName_new",
 #     timeFormatId="selectedTimeFormat_new",
 #     elementId="display_validation_msgs_new"
 #   ) == FALSE){
 #     tryCatch({
 #       
 #       variable_to_plot <- sort(input$parameters_to_process2_new, decreasing = FALSE)
 #       base_vars_to_plot <- sort(input$dailyStats_ts_variable_name, decreasing = FALSE)
 #       if(identical(variable_to_plot,base_vars_to_plot)) {
 #        discrete_data <- fun.ConvertDateFormat(fun.userDateFormat = input$selectedDateFormat_new
 #                                              ,fun.userTimeFormat =input$selectedTimeFormat_new
 #                                              ,fun.userTimeZone = input$selectedTimeZone_new
 #                                              ,fun.userDateFieldName = input$selectedDateFieldName_new
 #                                              ,fun.userTimeFieldName = input$selectedTimeFieldName_new
 #                                              ,fun.rawData = uploaded_newData()
 #                                              ,fun.date.org = input$dtNumOfCols1)
 #     
 #     
 #       if(nrow(discrete_data) != nrow(discrete_data[is.na(discrete_data$date.formatted),])) {
 #         base_data <- formated_raw_data$derivedDF
 #           if (!is.null(base_vars_to_plot) & nrow(base_data) != nrow(base_data[is.na(base_data$date.formatted),])){
 #             
 #               mergedData <- NULL
 #               for(varName in input$dailyStats_ts_variable_name) {
 #                    step1 <- base_data %>% select("continuous_value"=all_of(varName),"Date" = c(date.formatted))
 #                    step2 <- discrete_data %>% select("discrete_value" = all_of(varName), "discrete_Date" = c(date.formatted))
 #                    tempdf <- as.data.frame(qpcR:::cbind.na(step1,step2))
 #                    mergedData[[varName]] <- tempdf
 #               }
 #                combinded_df <- bind_rows(mergedData, .id="df")
 #                print(combinded_df)
 #                print(colnames(combinded_df))
 # 
 #                # shared x axis so calculate using base data file
 #                mainMapTitle <- "Discrete and continuous data"
 #                main_range = calculate_time_range(as.list(combinded_df))
 #                mainBreaks = main_range[[1]]
 #                main_x_date_label = main_range[[2]]
 # 
 #               mainPlot <- prepareDiscretePlot(combinded_df, mapTitle=mainMapTitle, xDateLabel=main_x_date_label, xDateBrakes= mainBreaks,base_vars_to_plot)
 #               
 #                if(renderStatus == FALSE) {
 #                   return(mainPlot)
 #                 } else {
 #                   if(!is.null(mainPlot) & length(input$parameters_to_process2_new) > 0){
 #                     shinyjs::runjs("$('#dateTimeBoxButton_new').click()")
 #                     output$display_time_series_new <-  renderPlotly({
 #                       ggplotly(mainPlot,height=calulatePlotHeight(length(input$parameters_to_process2_new))) %>% plotly::layout(legend = list(orientation = "h", x = 0.4, y = -0.4))
 #                     })
 #                   }
 #                }
 #           }
 # 
 #         } 
 #       } else {
 #         shinyAlertUI("common_alert_msg", discreteVarMismatch, "ERROR")
 #         return(mainPlot)
 #       }
 #     },error = function(parsingMsg) {
 #       print(parsingMsg)
 #       output$display_validation_msgs_new <- renderUI({
 #         prepareDateFormatErrorMsg(parsingMsg)
 #       })
 #     }, warning = function(parsingMsg){
 #       print(parsingMsg)
 #       output$display_validation_msgs_new <- renderUI({
 #         prepareDateFormatErrorMsg(parsingMsg)
 #       })
 #     }, message = function(parsingMsg) {
 #       print(parsingMsg)
 #       output$display_validation_msgs_new <- renderUI({
 #         prepareDateFormatErrorMsg(parsingMsg)
 #       })
 #     }, finally = {
 #        return(mainPlot)
 #     })
 #   }
 # }
 # geom_line(aes(x=as.POSIXct(Date,format="%Y-%m-%d"), y=continuous_value, colour=df))+
 #   geom_point(aes(x=(as.POSIXct(discrete_Date,format="%Y-%m-%d")), y=discrete_value, shape=discrete, colour="black"))+
 
 prepareDiscretePlot <- function(mergedDataSet, mapTitle, xDateLabel, xDateBrakes, baseVarsToPlot) {
   mainPlot <- NULL
   discrete <- mergedDataSet$df 
     mainPlot <- ggplot(data=mergedDataSet, dynamicTicks = TRUE, aes(name=bothValues, group=df)) +
     geom_line(inherit.aes = FALSE, aes(x=as.POSIXct(Date), y=continuous_value, colour=df))+
     geom_point(inherit.aes = TRUE, aes(x=as.POSIXct(discrete_Date), y=discrete_value, shape=discrete, colour="black"))+
     labs(title=mapTitle, x="Date", y="Parameters")+
     scale_x_datetime(date_labels=xDateLabel,date_breaks=xDateBrakes)+
     theme_bw()+
     facet_grid(df ~ ., scales = "free_y")+
     scale_color_discrete(name="continuous")+
     theme(
        strip.background = element_blank()
       ,legend.title=element_blank() 
       #,strip.text.y = element_blank()
       ,strip.placement = "outside"
       ,text=element_text(size=10,face = "bold", color="cornflowerblue")
       ,plot.title = element_text(hjust=0.5)
       ,legend.position="bottom"
       ,axis.text.x=element_text(angle=65, hjust=1, vjust=1)
     ) 
   return(mainPlot)
 }
 
   draw_uploaded_file_ts <- function(){
    mainPlot <- NULL
    mainMapTitle <- getMapTitle(input$dailyStats_shading, input$dailyStats_ts_title,lowerColumn = NULL, upperColumn = NULL)
    statsList <- processed$processed_dailyStats
    variable_to_plot <- input$dailyStats_ts_variable_name
    
    mainData <- Reduce(full_join, statsList)
      #shadingText <- paste0(variable_to_plot, " between daily 25th percentiles and 75th percentiles")
      mainList <- list()
      for(varName in variable_to_plot) {
        if(input$dailyStats_shading=="quantiles" | input$dailyStats_shading=="newData") {
          mainList[[paste(varName,input$dailyStats_ts_metrics, sep=".")]] <- as.data.frame(mainData %>% select(value=paste(varName,input$dailyStats_ts_metrics, sep="."), lower_col= paste(varName, "q.25%", sep="."), upper_col=paste(varName, "q.75%", sep="."), Date=Date))
        } else if(input$dailyStats_shading=="minMax"){
          mainList[[paste(varName,input$dailyStats_ts_metrics, sep=".")]] <- as.data.frame(mainData %>% select(value=paste(varName,input$dailyStats_ts_metrics, sep="."), lower_col= paste(varName, "min", sep="."), upper_col=paste(varName, "max", sep="."), Date=Date))
        } 
      }
      main_range = calculate_time_range(as.list(bind_rows(mainList, .id="df")))
      mainBreaks = main_range[[1]]
      main_x_date_label = main_range[[2]]
      
      mainPlot <- prepareBasePlot(dataList= mainList, mapTitle=mainMapTitle, xDateLabel=main_x_date_label, xDateBrakes= mainBreaks)
    return(mainPlot)
  }


  prepareBasePlot <- function(dataList, mapTitle, xDateLabel, xDateBrakes) {
    mainPlot <- NULL
    mainPlot <- ggplot(bind_rows(dataList, .id="df"), dynamicTicks = TRUE) +
      labs(title=mapTitle, x="Date", y="Parameters")+
      geom_ribbon(na.rm=TRUE, show.legend=TRUE, aes(ymin=lower_col,ymax=upper_col,x=as.POSIXct(Date,format="%Y-%m-%d",fill="df")),alpha=0.3, inherit.aes = FALSE)+
      geom_line(aes(x=as.POSIXct(Date,format="%Y-%m-%d"), y=value, colour=df))+
      scale_x_datetime(date_labels=xDateLabel,date_breaks=xDateBrakes)+
      theme_bw()+
      facet_grid(df ~ ., scales = "free_y")+
      scale_color_discrete(name="")+
      theme(
        strip.background = element_blank()
        ,legend.title=element_blank()
        ,strip.text.y = element_blank()
        ,strip.placement = "outside"
        ,text=element_text(size=10,face = "bold", color="cornflowerblue")
        ,plot.title = element_text(hjust=0.5)
        ,legend.position="bottom"
        ,axis.text.x=element_text(angle=65, hjust=1, vjust=1)
      ) 
    return(mainPlot)
  }
  
  #Need to test below to remove unknown timezone error
  checkTimeZone <- function (x) 
  {
    tzone <- attr(x, "tzone")[[1]]
    if (is.null(tzone) && !is.POSIXt(x)) 
      return("UTC")
    if (is.character(tzone) && nzchar(tzone)) 
      return(tzone)
    tzone <- attr(as.POSIXlt(x[1]), "tzone")[[1]]
    if (is.null(tzone)) 
      return("UTC")
    tzone
  }
 
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

  prepareDateFormatErrorMsg <- function(errorMsg, tab="") {
    if(tab == "homePage") {
      updateWorkFlowState("step3", "error")
    }
    changeButtonState(state="disable", btnList=c("calculateDailyStatistics","saveDailyStatistics"))
    if(grepl("All formats failed to parse. No formats found.",errorMsg[1], fixed = TRUE ) 
       | grepl( "failed to parse", errorMsg[1], fixed = TRUE)) {
       formattedError = "There is a mismatch between uploaded file date format and selected date format, please correct and try again."
      return(formattedError)
    } else {
      return(errorMsg)
    }
  }
  
  # validateSiteId <- function(raw_data) {
  #   my_colnames <- names(raw_data)
  #   idx_ID_col <- str_detect(my_colnames, regex('site', ignore_case = T))
  #   total_siteids_found <- sum(idx_ID_col, na.rm=TRUE)
  #   print(total_siteids_found)
  # 
  #   # loaded_data$siteID <- unique(raw_data[idx_ID_col])
  #   if (total_siteids_found > 1) {
  #     shinyAlertUI("common_alert_msg", siteIdNotOne, "WARNING")
  #   } else if (total_siteids_found  == 0) {
  #     shinyAlertUI("common_alert_msg", noSiteIdFound, "WARNING")
  #   }
  # }
  
  validateNewLowerAndUpper <- function(elementId){
    missingInputs <- FALSE
    if (input$newData_lower_col == "" | input$newData_upper_col == ""){
      missingInputs <- TRUE
      output[[elementId]] <- renderUI({
        shiny::validate(
          shiny::need(input$newData_lower_col != "", 'Please lower bound column.'),
          shiny::need(
            input$newData_upper_col != "", 'Please select upper bound column.'
          )
        )
      })
    }
    return(missingInputs)
  }

  
  validateUserInputs <- function(dateColumnNums,parmToProcess, dateFieldNameId,dateFormatId,timeFieldNameId,timeFormatId,elementId,tab=""){
    missingInputs <- FALSE
    # shinyjs::alert(input[[dateColumnNums]])
    # shinyjs::alert(input[[parmToProcess]])
    # shinyjs::alert(input[[dateFormatId]])
    # shinyjs::alert(input[[dateFieldNameId]])
    # shinyjs::alert(input[[timeFieldName]])
    

    if (input[[dateColumnNums]] == "combined" &
        (
          is.null(input[[parmToProcess]]) |
          input[[dateFormatId]] == "" |
          input[[dateFieldNameId]] == ""
        )) {
      missingInputs <- TRUE
      shinyjs::runjs(paste0("$('#",elementId,"').empty()"))
      output[[elementId]] <- renderUI({
        shiny::validate(
          shiny::need(input[[dateFormatId]] != "", 'Please select date format'),
          shiny::need(
            input[[dateFieldNameId]] != "",
            'Please select date field name.'
          ),
          shiny::need(
            !is.null(input[[parmToProcess]]),
            'Please select parameters to process.'
          )
        )
      })
    } else if (input[[dateColumnNums]] == 'separate' &
               (
                 is.null(input[[parmToProcess]]) |
                 input[[dateFormatId]] == "" |
                 input[[dateFieldNameId]] == "" |
                 input[[timeFieldNameId]] == ""
               )) {
      missingInputs <- TRUE
      shinyjs::runjs(paste0("$('#",elementId,"').empty()"))
      output[[elementId]] <- renderUI({
        shiny::validate(
          shiny::need(input[[dateFormatId]] != "", 'Please select date format'),
          shiny::need(
            input[[dateFieldNameId]] != "",
            'Please select date field name.'
          ),
          shiny::need(
            !is.null(input[[parmToProcess]]),
            'Please select parameters to process.'
          ),
          shiny::need(
            input[[timeFieldNameId]] != "",
            'Please select time field name.'
          )
        )
      })
    }
    if(missingInputs == FALSE & tab == "homePage" & workflowStatus$finish==FALSE) {
      updateWorkFlowState("step2", "success")
    } else if(missingInputs == TRUE & tab == "homePage") {
      updateWorkFlowState("step2", "error")
    }
    return(missingInputs)
  }
  
  calulatePlotHeight <- function(varNum) {
    plotHeight <- 400
    if(varNum > 2) {
      plotHeight <- plotHeight + ((varNum - 2) * 82)
    }
    return(plotHeight)
  }
  fig <- function(width, heigth){
    options(repr.plot.width = width, repr.plot.height = heigth)
  }
  
  # debug_msg <- function(...) {
  #   is_local <- Sys.getenv('SHINY_PORT') == ""
  #   in_shiny <- !is.null(shiny::getDefaultReactiveDomain())
  #   txt <- toString(list(...))
  #   if (is_local) message(txt)
  #   if (in_shiny) shinyjs::runjs(sprintf("console.debug(\"%s\")", txt))
  # }
  
  uploadFile <- function(uploadedFile, stopExecution=FALSE, tab="") {
    my_data <- NULL
    loaded_data$name <- uploadedFile$name
    if(grepl("csv$",uploadedFile$datapath)){
      my_data<-import_raw_data(uploadedFile$datapath,"csv",has_header=TRUE)
    } else if(grepl("xlsx$",uploadedFile$datapath)){
      my_data<-import_raw_data(uploadedFile$datapath,"xlsx",has_header=TRUE)
    }
    
    if(!is.null(my_data)) {
      if(tab == "homePage") {
        workflowStatus$finish = FALSE
        updateWorkFlowState(elementId= "step1", state="success")
      }
      return(my_data)
    } else {
      if(tab == "homePage") {
         workflowStatus$finish = FALSE
         updateWorkFlowState(elementId= "step1", state="error")
      }
      shinyAlertUI("common_alert_msg", wrongDataFormat, "ERROR")
      if(stopExecution== TRUE) {
        stop()
      }
    }
  }
  
  observeEvent(input$common_alert_msg,{
    shinyjs::runjs("swal.close();")
  })
  shinyAlertUI <- function(id,msg,type) {
	  shinyalert(inputId=id,type,msg,closeOnClickOutside = TRUE,closeOnEsc = TRUE,confirmButtonText="OK")
  }
  

  updateWorkFlowState <- function(elementId, state) {
 
    if(elementId == "step1" & state == "success") {
      removeCssClasses()
      addSuccessClass(c("step1"))
      addPrimaryClass(c("step2","step3","step4","step5"))
      changeButtonState(state="disable", btnList=c("runQS","display_raw_ts","calculateDailyStatistics","saveDailyStatistics"))
      changeButtonState(state="enable", btnList=c("display_raw_ts"))
    } else if(elementId == "step1" & state == "error") {
      removeCssClasses()
      shinyjs::addClass("step1", "btn-danger")
      addPrimaryClass(c("step2","step3","step4","step5"))
      changeButtonState(state="disable", btnList=c("display_raw_ts","runQS","calculateDailyStatistics","saveDailyStatistics"))
      workflowStatus$finish=FALSE
    }
    
    else if(elementId == "step2" & state == "success") {
      removeCssClasses()
      addPrimaryClass(c("step3","step4","step5"))
      addSuccessClass(c("step1","step2"))
      changeButtonState(state="enable", btnList=c("display_raw_ts","runQS","display_raw_ts"))
    } else if(elementId == "step2" & state == "error") {
      removeCssClasses()
      shinyjs::addClass("step2", "btn-danger")
      addPrimaryClass(c("step3","step4","step5"))
      addSuccessClass(c("step1"))
      changeButtonState(state="disable", btnList=c("display_raw_ts","calculateDailyStatistics","saveDailyStatistics"))
      changeButtonState(state="enable", btnList=c("runQS","display_raw_ts"))
      workflowStatus$finish=FALSE
    }
    
    else if(elementId == "step3" & state == "success") {
      removeCssClasses()
      addSuccessClass(c("step1","step2","step3"))
      addPrimaryClass(c("step4","step5"))
      changeButtonState(state="enable", btnList=c("display_raw_ts","runQS"))
      js$enableTab("downloadData")
      js$enableTab("discreateDataEx")
    } else if(elementId == "step3" & state == "error") {
      removeCssClasses()
      shinyjs::addClass("step3", "btn-danger")
      addPrimaryClass(c("step4","step5"))
      addSuccessClass(c("step1","step2"))
      changeButtonState(state="disable", btnList=c("calculateDailyStatistics","saveDailyStatistics"))
      changeButtonState(state="enable", btnList=c("runQS","display_raw_ts"))
      workflowStatus$finish=FALSE
    }
    
    else if(elementId == "step4" & state == "success") {
      removeCssClasses()
      addSuccessClass(c("step1","step2","step3","step4"))
      changeButtonState(state="enable", btnList=c("display_raw_ts","runQS","calculateDailyStatistics","saveDailyStatistics"))
      addPrimaryClass(c("step5"))
    } else if(elementId == "step4" & state == "error") {
      removeCssClasses()
      shinyjs::addClass("step4", "btn-danger")
      addSuccessClass(c("step1","step2","step3"))
      addPrimaryClass(c("step5"))
      changeButtonState(state="disable", btnList=c("calculateDailyStatistics","saveDailyStatistics"))
      changeButtonState(state="enable", btnList=c("display_raw_ts","runQS"))
      workflowStatus$finish=FALSE
    }
    
    else if(elementId == "step5" & state == "success") {
      removeCssClasses()
      addSuccessClass(c("step1","step2","step3","step4","step5"))
      js$enableTab("DataExploration")
      # js$enableTab("CreateReport")
      changeButtonState(state="enable", btnList=c("display_raw_ts","runQS","calculateDailyStatistics","saveDailyStatistics"))
      workflowStatus$finish = TRUE
    } else if(elementId == "step4" & state == "error") {
      removeCssClasses()
      shinyjs::addClass("step5", "btn-danger")
      addSuccessClass(c("step1","step2","step3","step4"))
      changeButtonState(state="disable", btnList=c("calculateDailyStatistics","saveDailyStatistics"))
      changeButtonState(state="enable", btnList=c("display_raw_ts","runQS","display_raw_ts"))
      workflowStatus$finish=FALSE
    }
    if(elementId %in% c("step1","step2","step3","step4")) {
        js$disableTab("DataExploration")
    }
    if(elementId %in% c("step1","step2")) {
      js$disableTab("downloadData")
      js$disableTab("discreateDataEx")
    }
    
  }
  
  changeButtonState <- function(state, btnList) {
    if(state == "enable") {
      for(b in btnList) {
        enable(b)
      }
    } else if(state =="disable"){
      for(b in btnList) {
        disable(b)
      }
    }
  }
  
  removeCssClasses <- function(){
    cssClasses <- c("btn-success","btn-primary","btn-danger")
    elementIds <- c("step1","step2","step3", "step4","step5")
    for(e in elementIds) { 
      for (c in cssClasses) {
        shinyjs::removeClass(e, c)      }
    }
  }
  addPrimaryClass <- function(elmentIds){
    for(e in elmentIds) { 
      shinyjs::addClass(e, "btn-primary")
    }
  }
  addSuccessClass <- function(elmentIds) {
    for(e in elmentIds) { 
      shinyjs::addClass(e, "btn-success")
    }
  }
  
}