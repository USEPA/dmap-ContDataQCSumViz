ThermalStatsModuleUI <- function(id) {
  ns <- NS(id)
  shinyjs::useShinyjs()
  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(class="panel panel-default",style="margin:10px;",
          div(class="panel-heading"),
          div(class="panel-body",
              uiOutput(ns("thermal_input_1")),
              uiOutput(ns("thermal_input_2")),
              uiOutput(ns("thermal_input_3")),
              uiOutput(ns("display_run_thermal_button")),
              hr(),
              uiOutput(ns("display_save_thermal_button")),
          )#end of panel body
      ) # end of panel
    ),
    mainPanel(
      width = 9,
      column(
        width = 12,
        uiOutput(ns("display_help_text_thermal_statistics")),
        DT::dataTableOutput(ns("display_thermal_table_1")),
        br(),
        DT::dataTableOutput(ns("display_thermal_table_2")),
        br(),
        DT::dataTableOutput(ns("display_thermal_table_3")),
        br(),
        DT::dataTableOutput(ns("display_thermal_table_4")),
        br(),
        DT::dataTableOutput(ns("display_thermal_table_5"))
      )
    ) # mainPanel end
  ) # sidebarLayout end

}

ThermalStatsModuleServer <- function(id, uploaded_data, formated_raw_data, dailyStats, loaded_data, to_download, renderThermalStats) {
 
  localStats <- reactiveValues(stats=list())
  variables_avail <- reactiveVal()
  
  moduleServer(
    id,
    function(input, output, session) {
          ns <- session$ns
           observe({
            localStats <- dailyStats
            variables_avail <- names(uploaded_data)
            if(renderThermalStats$render == TRUE) {
              
              output$thermal_input_1 <- renderUI({
                site_keys_in_favor_order <- c("Site","SITE","SiteID","SITEID")
                possible_site_columns <- site_keys_in_favor_order[site_keys_in_favor_order %in% variables_avail]
                if (length(possible_site_columns)==0){
                  site_to_select <- variables_avail[grep('site',variables_avail,ignore.case=TRUE)][1]
                }else{
                  site_to_select <- possible_site_columns[1]
                }
                selectizeInput(ns("thermal_SiteID_name"),label ="Select SiteID Column",
                               choices=variables_avail,
                               multiple = FALSE,
                               selected=site_to_select,
                               options = list(hideSelected = FALSE))
              })
              
              output$thermal_input_3 <- renderUI({
                temp_keys_in_favor_order <- c("Water.Temp.C","WATER.TEMP.C","Water_Temp_C",
                                              "WATER_TEMP_C","Air.Temp.C","AIR.TEMP.C","Air_Temp_C","AIR_TEMP_C")
                possible_temp_columns <- temp_keys_in_favor_order[temp_keys_in_favor_order %in% variables_avail]
                if (length(possible_temp_columns)==0){
                  temp_to_select <- variables_avail[grep('temp',variables_avail,ignore.case=TRUE)][1]
                }else{
                  temp_to_select <- possible_temp_columns[1]
                }
                selectizeInput(ns("thermal_Temp_name"),label ="Select Temperature Column",
                               choices=variables_avail,
                               multiple = FALSE,
                               selected= c(temp_to_select),
                               options = list(hideSelected = FALSE))
              })
              
              output$display_run_thermal_button <- renderUI({
                actionButton(inputId=ns("display_thermal"), label="Display Stream Thermal",class="btn btn-primary")
              })
              
              output$display_save_thermal_button <- renderUI({
                downloadButton(outputId=ns("save_thermal"), label="Save thermal statistics to excel",class="btn btn-primary")
              })
              
              output$display_help_text_thermal_statistics <- renderUI({
                verbatimTextOutput(ns("help_text_thermal_statistics"))
              })
              
              output$help_text_thermal_statistics <- renderText({
                filePath <- "help_text_files/Temperature_ThermalStatistics.txt"
                fileText <- paste(readLines(filePath,encoding="UTF-8"),collapse="\n")
                fileText
              })
              
            }
          })
            
            observeEvent(input$display_thermal, {
              shinyjs::hide("help_text_thermal_statistics")
              
              myData <- uploaded_data
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
              
              localStats$ST.freq <- ST.freq
              localStats$ST.mag <- ST.mag
              localStats$ST.roc <- ST.roc
              localStats$ST.tim <- ST.tim
              localStats$ST.var <- ST.var
              
              thermal.statistics.table.options <- list(
                scrollX = TRUE, #allow user to scroll wide tables horizontally
                stateSave = FALSE,
                pageLength = 15,
                dom = 'Bt',
                buttons = list('copy','print',list(extend = 'collection',buttons = c('csv','excel','pdf'),text='Download')),
                columnDefs = list(list(className="dt-center",targets="_all"))
              )
              #output$thermal_statistics_table_1 <- DT::renderDataTable({
              
              output$display_thermal_table_1 <- DT::renderDataTable({
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
              
              
              output$display_thermal_table_2 <- DT::renderDataTable({
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
              
              output$display_thermal_table_3 <- DT::renderDataTable({
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

              output$display_thermal_table_4 <- DT::renderDataTable({
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

              output$display_thermal_table_5 <- DT::renderDataTable({
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
              
              
              #Desc.freq, Desc.mag, Desc.roc, Desc.tim, Desc.var descriptions are defined in the constants.R file
              Group.Desc <- c(Desc.freq, Desc.mag, Desc.roc, Desc.tim, Desc.var)
              df.Groups <- as.data.frame(cbind(c("freq","mag","roc","tim","var"),Group.Desc))
              SiteID <- localStats$ST.freq[1,1]
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
              writeWorksheet(wb, localStats$ST.freq, sheet = "freq")
              writeWorksheet(wb, localStats$ST.mag, sheet = "mag")
              writeWorksheet(wb, localStats$ST.roc, sheet = "roc")
              writeWorksheet(wb, localStats$ST.tim, sheet = "tim")
              writeWorksheet(wb, localStats$ST.var, sheet = "var")
              # save workbook
              to_download$wb <- wb
              to_download$fileName <- fileName
            }) #observeEvent end
            
            
            observeEvent(input$save_thermal, {
                require(XLConnect)

                #Desc.freq, Desc.mag, Desc.roc, Desc.tim, Desc.var descriptions are defined in the constants.R file
                Group.Desc <- c(Desc.freq, Desc.mag, Desc.roc, Desc.tim, Desc.var)
                df.Groups <- as.data.frame(cbind(c("freq","mag","roc","tim","var")
                                                 ,Group.Desc))
                SiteID <- localStats$ST.freq[1,1]
                myDate <- format(Sys.Date(),"%Y%m%d")
                myTime <- format(Sys.time(),"%H%M%S")
                Notes.User <- Sys.getenv("USERNAME")

                Notes.Names <- c("Dataset (SiteID)", "Analysis.Date (YYYYMMDD)"
                                 , "Analysis.Time (HHMMSS)", "Analysis.User")
                Notes.Data <- c(SiteID, myDate, myTime, Notes.User)
                df.Notes <- as.data.frame(cbind(Notes.Names, Notes.Data))
                ## New File Name
                if (!file.exists("Output/saved_streamThermal/")) dir.create(file.path("Output/saved_streamThermal"),showWarnings = FALSE, recursive = TRUE)
                name_in_file <- loaded_data$name
                myFile.XLSX <- paste("Output/saved_streamThermal/StreamThermal"
                                     , name_in_file
                                     , SiteID
                                     , myDate
                                     , myTime
                                     , "xlsx"
                                     , sep=".")
                ## Copy over template with Metric Definitions
                file.copy(file.path(path.package("ContDataQC")
                                    ,"extdata"
                                    ,"StreamThermal_MetricList.xlsx")
                          , myFile.XLSX)
                ## load workbook, create if not existing
                wb <- loadWorkbook(myFile.XLSX, create = TRUE)
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
                writeWorksheet(wb, localStats$ST.freq, sheet = "freq")
                writeWorksheet(wb, localStats$ST.mag, sheet = "mag")
                writeWorksheet(wb, localStats$ST.roc, sheet = "roc")
                writeWorksheet(wb, localStats$ST.tim, sheet = "tim")
                writeWorksheet(wb, localStats$ST.var, sheet = "var")
                # save workbook
                saveWorkbook(wb, myFile.XLSX)

              }) # observeEvent close

            
            output$save_thermal <- downloadHandler(
              filename = function(){
                to_download$fileName
              },
              content = function(file){
                saveWorkbook(to_download$wb,file)
              }
            ) # downloadHandler close
            
  
          
    }) # end of module server
}
