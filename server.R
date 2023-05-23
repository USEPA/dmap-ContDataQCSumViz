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

  useShinyjs()
  conflict_prefer("box", "shinydashboard")
  conflict_prefer("dataTableOutput", "DT")
  conflict_prefer("yday", "data.table")
  conflict_prefer("select", "dplyr")
  
  loaded_data <- reactiveValues()
  raw_data_columns<-reactiveValues()
  dateRange <- reactiveValues()
  workflowStatus <- reactiveValues(finish=FALSE,
                                   elementId="step1",
                                   state="init")


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
  
  dailyStatusCalculated <- reactiveValues(status='unfinished')
  readyForCalculation <- reactiveValues(status=FALSE)
  renderDataExp <- reactiveValues(render=FALSE)
  renderSummaryTables <- reactiveValues(render=FALSE)
  renderTSOverlay <- reactiveValues(render=FALSE)
  renderTSBoxPlot <- reactiveValues(render=FALSE)
  renderCDFPlot <- reactiveValues(render=FALSE)
  renderRasterPlot <- reactiveValues(render=FALSE)
  
  #Temperature
  renderThermalStats <- reactiveValues(render=FALSE)

  #  Upload Data##############################
  #EWL
  if (file.exists("_moved/File_Format.rds")) file.remove("_moved/File_Format.rds")
  do.call(file.remove, list(list.files("Selected_Files", full.names = TRUE)))
 
  #init modules, reactive values will reflect when the buttons are actually clicked
  #shinyAlertModuleServer("common_alert_msg")
  progressWorkflowModuleUIServer("statusWorkflow", workflowStatus)
  calculateDailyStatsModuleServer("calculateDailyStats", formated_raw_data, homeDTvalues, metaHomeValues,loaded_data, dailyStatusCalculated,processed,readyForCalculation)
  
  ############ Continuous Data Exploration >>  All parameters ############
  
  #Continuous Data Exploration > All Parameters >  Summary tables tab
  SummaryTablesModuleServer(id="DataExpSummaryTbls", dailyStats=processed, renderSummaryTables)
  #Continuous Data Exploration > All Parameters >  Time Series plots tab
  DataExplorationTSModuleServer(id="dataExpTS", dailyStats=processed, renderDataExp)
  #Continuous Data Exploration > All Parameters >  Time series - Annual overlays tab
  TsOverlayModuleServer(id="tsOverlayTab", dailyStats=processed, renderTSOverlay)
  #Continuous Data Exploration > All Parameters > Box plots tab
  TsBoxPlotModuleServer(id="tsBoxPlot", dailyStats=processed, renderTSBoxPlot)
  #Continuous Data Exploration > All Parameters > CDFs tab
  TsCDFPlotModuleServer(id="tsCDFPlot", dailyStats=processed, renderCDFPlot)
  #Continuous Data Exploration > All Parameters > Raster graphs tab
  TsRasterPlotModuleServer(id="tsRasterPlot", dailyStats=processed, renderRasterPlot)
  
  
   #Home page file upload
  uploaded_data<-eventReactive(c(input$uploaded_data_file),{
        readyForCalculation$status <- FALSE
        dailyStatusCalculated$status <- "unfinished"
        my_data <- uploadFile(c(input$uploaded_data_file), stopExecution=FALSE, tab="homePage")
         # drop all rows where all the columns are empty
       if (length(my_data) > 0 ) {
         my_data <- my_data[rowSums(is.na(my_data) | is.null(my_data) | my_data == "") != ncol(my_data),]
         my_colnames <- colnames(my_data)
         shinyjs::show(id="displayidLeft")
         parmsToProcess <- fun.findVariableToProcess(my_colnames, getDateCols= FALSE)
         workflowStatus$elementId="step1"
         workflowStatus$state="success"
        
         shinyjs::show(id="dateTimeBoxButton")
         
         #goes to left panel
         output$display_runmetasummary <-
           renderUI({
             tagList(
             hr(),
             actionButton(inputId="runQS", label="Step 3: Run meta summary",class="btn btn-primary")
             )
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
                   div(id="dateAndTimeError"),
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
                   id = "display_all_raw_ts_div", style = "height:100%;width:100%;display:none",
                   column(width = 12, rawTSModuleUI("displayRawTS"))
                 ) # end of div
               ) # fluidRow end
           ) # end of mainBox
         })
       }
       
     return(my_data)
  })
  
  #init server modules
  #Continuous Data Exploration >  Temperature > Thermal statistics tab
  ThermalStatsModuleServer("thermalStats", uploaded_data(), formated_raw_data, dailyStats=processed, loaded_data, to_download, renderThermalStats)
  
  
  
  observeEvent(uploaded_data(), {
    homeDTvalues$homeDateAndTime <- dateAndTimeServer(id = "homePage", uploaded_data())
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
      },error = function(parsingMsg) {
        processErrors(parsingMsg, tab = tabName, elementId=errorDivId)
      }, warning = function(parsingMsg){
        processErrors(parsingMsg, tab = tabName, elementId=errorDivId)
      }, message = function(parsingMsg) {
        processErrors(parsingMsg, tab = tabName, elementId=errorDivId)
      }) #end of tryCatch
    return(userDataL)
  }
  
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
    radioButtons("disp", "Display file information", choices = c(Head = "head",Tail="tail",ColumnNames="Column names"),
                 selected = "head"),
    hr(),
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

  observeEvent(input$showrawTS,{
    shinyjs::show(id="display_all_raw_ts_div")
    shinyjs::removeClass("dateAndTimeError", "alert alert-danger")
    raw_data <- uploaded_data()
    homeDTvalues$homeDateAndTime <- dateAndTimeServer(id = "homePage", uploaded_data())
    showRawDateAndTime <- homeDTvalues$homeDateAndTime
  
    #display_validation_msgs dateBox
    if (showRawDateAndTime$isTimeValid() & showRawDateAndTime$isDateAndtimeValid()) {
      tryCatch({
        # if error had occured then on fix reset the step
        shinyjs::removeClass("statusWorkflow-step3", "btn-danger")
        shinyjs::addClass("statusWorkflow-step3", "btn-primary")
        formated_raw_data$derivedDF <- getFormattedRawData(showRawDateAndTime, raw_data, tabName = "homePage", errorDivId = "dateAndTimeError")
        rawTSModuleServer("displayRawTS", showRawDateAndTime, formated_raw_data)
        workflowStatus$elementId="step2"
        workflowStatus$state="success"
      },error = function(parsingMsg) {
          processErrors(parsingMsg, tab = "homePage", elementId="dateAndTimeError")
          workflowStatus$elementId="step2"
          workflowStatus$state="error"
      }, warning = function(parsingMsg){
          processErrors(parsingMsg, tab = "homePage", elementId="dateAndTimeError")
          workflowStatus$elementId="step2"
          workflowStatus$state="error"
      }, message = function(parsingMsg) {
          processErrors(parsingMsg, tab = "homePage", elementId="dateAndTimeError")
          workflowStatus$elementId="step2"
          workflowStatus$state="error"
      }) #end of tryCatch
    } #end of validation check
  })  ## observeEvent end
  
  
  processErrors <- function (errorMsg, tabName="", elementId) {
    processedMsg <- prepareDateFormatErrorMsg(errorMsg, tab = tabName)
    shinyjs::runjs(paste0("$('#",elementId,"').text('",processedMsg,"')"))
    shinyjs::addClass(elementId, "alert alert-danger")
  }
  

  observe({
    if(dailyStatusCalculated$status == "finished") {
      workflowStatus$elementId="step4"
      workflowStatus$state="success"
      workflowStatus$elementId="step5"
      workflowStatus$state="success"
    } else if(dailyStatusCalculated$status == "error") {
      workflowStatus$elementId="step4"
      workflowStatus$state="error"
    }
  })

 
  observeEvent(input$runQS,{
    tryCatch({
      raw_data <- uploaded_data()
      homeDTvalues$homeDateAndTime <- dateAndTimeServer(id = "homePage", uploaded_data())
      localHomeDateAndTime <- homeDTvalues$homeDateAndTime
      #display_validation_msgs dateBox
      if (localHomeDateAndTime$isTimeValid() & localHomeDateAndTime$isDateAndtimeValid()) {
         # output$display_quick_summary_table <- renderUI({
         #    column(12, align = "center", withSpinner(tableOutput("quick_summary_table")))
         #  })
          # update the reactiveValues
          #All the variables are selected
          workflowStatus$elementId="step2"
          workflowStatus$state="success"
          raw_data <- getFormattedRawData(localHomeDateAndTime, raw_data, tabName="homePage", errorDivId="dateAndTimeError")
          #now shorten the varname
          if ("date.formatted" %in% colnames(raw_data) & !is.null(localHomeDateAndTime$parmToProcess()) & nrow(raw_data) != nrow(raw_data[is.na(raw_data$date.formatted), ])) {
             print("passed fun.ConvertDateFormat")
            
             #set dateRange for other modules
             formated_raw_data$derivedDF <- raw_data
             dateRange$min <- min(as.Date(raw_data$date.formatted), na.rm = TRUE)
             dateRange$max <- max(as.Date(raw_data$date.formatted), na.rm = TRUE)
             raw_data_columns$date_column_name = "date.formatted"
             
             # now shorten the varname
             raw_data <- formated_raw_data$derivedDF
              metaHomeValues$metaVal <-  metaDataServer("metaDataHome", localHomeDateAndTime$parmToProcess(), formatedUploadedData=raw_data, uploadData=uploaded_data())
              raw_data_columns$date_column_name = "date.formatted"
              output$display_fill_data <- renderUI({
                metaDataUI("metaDataHome")
              })

              shinyjs::runjs("$('#dateTimeBoxButton').click()")
            
            if(workflowStatus$finish==FALSE) {
                workflowStatus$elementId="step3"
                workflowStatus$state="success"
                readyForCalculation$status <- TRUE
            }
              output$display_actionButton_calculateDailyStatistics <-
                renderUI({calculateDailyStatsModuleUI("calculateDailyStats", readyForCalculation)})
           
          } else {
            #shinyAlertUI("common_alert_msg" , invalidDateFormt, "ERROR")
            print("it should have updated users on the UI")
          }     
        }
      },error = function(parsingMsg) {
        processErrors(parsingMsg, tab = "homePage", elementId="dateAndTimeError")
        readyForCalculation$status <- FALSE
      }, warning = function(parsingMsg){
        processErrors(parsingMsg, tab = "homePage", elementId="dateAndTimeError")
        readyForCalculation$status <- FALSE
      }, message = function(parsingMsg) {
        processErrors(parsingMsg, tab = "homePage", elementId="dateAndTimeError")
        readyForCalculation$status <- FALSE
      }) #end of tryCatch
 
  })  ## observeEvent end

  ## close the warning messages inside the above oberveEvent

  observeEvent(input$get_the_year,{
    #print(input$alert_no_date)
    shinyjs::runjs("swal.close();")
  })


  observeEvent(input[["tabset"]], {
    ### DE, ALL, summary table ####

    #USGS Gage
    output$gage_panel <- renderUI({
      div(class="panel panel-default", style="padding:10px;margin-top:20px;",
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




  ################# 2:Time series plot << All parameters #################

  
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
              div(uiOutput("disDateAndTimeError"), style="padding:4px;"),
              box(width="100%",class="displayed",id="dateBox_discrete",
                  div(
                    style = "margin-left:10px",
                    dateAndTimeUI(id = "discretePage", paramChoices = cols_avail, uploadedCols = cols_avail)
                  ),
                  hr(style="margin:0px;padding:0px;"),
                  fluidRow(
                    div(style = "padding:2px;",
                    span(width = "85%", actionButton(inputId = "display_discrete_data", label = "Display", class = "btn btn-primary"), style = "margin:5px 15px 5px 25px;"),
                    span("Note: Red border denotes required fields.", style = "font-weight:bold;color:#b94a48;")
                    )
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
    discreteDTvalues$disDateAndTime <- dateAndTimeServer(id = "discretePage", uploaded_discreteData())
    localDiscreteDateAndTime <- discreteDTvalues$disDateAndTime
    mainPlot <- NULL
    if (localDiscreteDateAndTime$isTimeValid() & localDiscreteDateAndTime$isDateAndtimeValid()) {
      tryCatch({
        variable_to_plot <- sort(localDiscreteDateAndTime$parmToProcess(), decreasing = FALSE)
        base_vars_to_plot <- sort(input$discreteBaseId, decreasing = FALSE)
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
        processErrors(parsingMsg,  elementId="disDateAndTimeError")
      }, warning = function(parsingMsg){
        processErrors(parsingMsg,  elementId="disDateAndTimeError")
      }, message = function(parsingMsg) {
        processErrors(parsingMsg,  elementId="disDateAndTimeError")
      }) #end of tryCatch
    }
  })
 
  # End of Discrete data related functions
  
  
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
          #,strip.text.y = element_blank()
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

  #################  4:Boxplots << All parameters #################

  ## close the alert messages
  observeEvent(input$alert_data_not_avail_for_box,{
    #print(input$alert_no_date)
    shinyjs::runjs("swal.close();")
  })



  ## close the alert messages
  observeEvent(input$alert_data_not_avail_for_CDF,{
    #print(input$alert_no_date)
    shinyjs::runjs("swal.close();")
  })



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
        daymetCols <- rawResult$daymetColumns
        dayMetRawData$daymetColumns <- rawResult$daymetColumns
        updateSelectizeInput(session, 'daymet_params', choices = daymetCols, selected = daymetCols[1])
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
  



 prepareDiscretePlot <- function(mergedDataSet, mapTitle, xDateLabel, xDateBrakes, baseVarsToPlot) {
   mainPlot <- NULL
   discrete <- mergedDataSet$df 
     mainPlot <- ggplot(data=mergedDataSet, dynamicTicks = TRUE, aes(name=bothValues, group=df)) +
     geom_line(inherit.aes = FALSE, aes(x=as.POSIXct(Date), y=continuous_value, colour=df))+
     geom_point(inherit.aes = TRUE, aes(x=as.POSIXct(discrete_Date), y=discrete_value, shape=discrete, colour="discrete"))+
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
      workflowStatus$elementId="step3"
      workflowStatus$state="error"
      dailyStatusCalculated$status <- "unfinished"
      readyForCalculation$status <- FALSE
    }
    readyForCalculation$status <- FALSE
    if(grepl("All formats failed to parse. No formats found.",errorMsg[1], fixed = TRUE ) 
       | grepl( "failed to parse", errorMsg[1], fixed = TRUE)) {
       formattedError = "There is a mismatch between uploaded file date format and selected date format, please correct and try again."
      return(formattedError)
    } else {
      return(errorMsg)
    }
  }

  
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
  

  uploadFile <- function(uploadedFile, stopExecution=FALSE, tab="") {
    my_data <- NULL
    otherExtension <- FALSE
    loaded_data$name <- uploadedFile$name
    if(grepl("csv$",uploadedFile$datapath)){
      my_data<-import_raw_data(uploadedFile$datapath,"csv",has_header=TRUE)
    } else if(grepl("xlsx$",uploadedFile$datapath)){
      my_data<-import_raw_data(uploadedFile$datapath,"xlsx",has_header=TRUE)
    } else {
      otherExtension <- TRUE
    }
    
    if(!is.null(my_data)) {
      if(tab == "homePage") {
        workflowStatus$finish = FALSE
        workflowStatus$elementId="step1"
        workflowStatus$state="success"
      }
      return(my_data)
      
    } else {
      if(tab == "homePage") {
         workflowStatus$finish = FALSE
         workflowStatus$elementId="step1"
         workflowStatus$state="error"
      }
      if(otherExtension == TRUE) {
        shinyAlertUI("common_alert_msg", "Uploaded your data in .csv format.", "ERROR")
      } else {
        shinyAlertUI("common_alert_msg", wrongDataFormat, "ERROR")
      }
      if(stopExecution == TRUE) {
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
  
  
  
  # temperatue subtabs
  observe({
    if(input$temp_subtabs == "sb1") {
      renderThermalStats$render <- TRUE
    }
  })
  
# All Parameters subtabs
  observe({ 
    if(input$all_parameters_subtabs == "tab_time_series") {
      renderDataExp$render <- TRUE
    } else if( input$all_parameters_subtabs == "tab_summary_tables") {
      renderSummaryTables$render <- TRUE
    } else if (input$all_parameters_subtabs == "tab_time_series_overlay") {
      renderTSOverlay$render <- TRUE
    } else if (input$all_parameters_subtabs == "tab_box") {
      renderTSBoxPlot$render <- TRUE
    } else if (input$all_parameters_subtabs == "tab_CDF") {
      renderCDFPlot$render <- TRUE
    } else if (input$all_parameters_subtabs == "tab_raster") {
      renderRasterPlot$render <- TRUE
    } 
  })
  
}