#
# This is the user-interface definition of ContDataSumViz Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

## Moved to Global ***
# library(shiny)
# library(shinyjs)
# library(shinyalert)
# library(shinythemes)
# library(shinydashboard)
# library(ggplot2)
# library(ggthemes)
# library(DT)
# library(plotly)
# #library(shinycustomloader)
# library(shinycssloaders)
app_jscode <-
  "shinyjs.disableTab = function(name) {
    var tab = $('.nav li a[data-value=' + name + ']');
    $(tab).css({'visibility' : 'hidden' })
   // $(tab).hide();
  }
  shinyjs.enableTab = function(name) {
    var tab = $('.nav li a[data-value=' + name + ']');
    $(tab).css({'visibility' : 'visible' })
    // $(tab).show();
  }"

# Define UI for application
options(spinner.color.background = "#ffffff", spinner.size = 1)
shinyUI(fluidPage(
  theme = "styles.css",
  useShinyjs(),
  shinyjs::extendShinyjs(text = app_jscode, functions = c("disableTab","enableTab")),
  tags$head(tags$script(src="script.js")),
  tags$head( tags$link(rel="stylesheet", type="text/css", href="app.css")),
  mainPanel(
    width = 12,
    # spacing
    fluidRow(id = "one", ),
    # top controls
    fluidRow(
      div(id = "customBusy", class = "loading-modal")
      ),
    fluidRow(
      column(
        width = 12,
        div(img(src="headerImage.png", class="headerImgRes"), style="display: flex;justify-content:center")
       )
    ),
    fluidRow(
      column(
        width = 12,
        br(),
        div("Please complete below steps before proceeding to 'Data Exploration' :", class="text-info", style="font-weight:bold"),
        br()
      )
    ),
    fluidRow(
      column(width = 12, progressWorkflowModuleUI("statusWorkflow"))
    ),
    fluidRow(
      p(),
      tabsetPanel(id="mainTabs",
        tabPanel(
          title="Upload Data",
          value="uploadData",
          fluidPage(
            fluidRow(
              column(
                width = 12,
                sidebarLayout(
                  sidebarPanel(
                    width = 3,
                    div(class="panel panel-default", style="margin:10px;",
                        div(class="panel-heading", "Step 1: Upload File", style="font-weight:bold;", icon("info-circle", style = "color:#2fa4e7", id="fileHelp")),
                        div(class="panel-body",
                            tagList(
                              bsPopover(id="fileHelp", title=HTML("<b>Helpful Hints</b>"), content = HTML("Files must contain only one mandatory header row containing column names.</br></br>  Microsoft Excel corrupts .csv files when reopened by double clicking its icon or by using the File Open dialog. You can avoid this by using the Text or Data Import Wizard from the Excel Data Tab"), 
                                        placement = "right", trigger = "hover"),
                              fileInput("uploaded_data_file",
                                        label = HTML("<b>Upload your data in .csv format</b>"),
                                        multiple = FALSE,
                                        buttonLabel=list(tags$b("Browse"),tags$i(class = "fa-solid fa-folder")),
                                        accept = c(
                                          "text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv"
                                        )
                              )),
                            uiOutput("displayFC")
                        )),
                    tagList(
                      uiOutput("display_runmetasummary"),
                      uiOutput("display_actionButton_calculateDailyStatistics"),
                      uiOutput("display_actionButton_saveDailyStatistics")
                    )
                  ),
                  mainPanel(
                    width = 9,
                    uiOutput("display_raw_ts")
                  ) # mainPanel end
                ) # sidebarLayout end
              )# column close
            )
          ) # fluidPage close
        ), # tabPanel end
        tabPanel(
          title="USGS & Daymet Exploration",
          value="downloadData",
          fluidPage(
            fluidRow(
              column(
                width = 12,
                sidebarLayout(
                    sidebarPanel(
                      width = 3,
                        uiOutput("gage_panel"),
                        uiOutput("daymet_panel"),
                        uiOutput("base_gage_daymet_panel")
                    ),
                    mainPanel(
                      width = 9,
                      column(width = 12, plotlyOutput("display_downloaded_data"))
                    ) # mainPanel end
                  ) # sidebarLayout end
                )# column close
            ) # raw
          ) # page
        ),
        tabPanel(
          title="Discrete Data Exploration",
          value="discreateDataEx",
          column(
            width = 12,
            sidebarLayout(
              sidebarPanel(
                width = 3,
                div(class="panel panel-default",style="margin:10px;",
                    div(class="panel-heading", "Upload discrete data in .csv format", style="font-weight:bold;", icon("info-circle", style = "color: #2fa4e7", id="discreteHelp")),
                    div(class="panel-body",
                        tagList(
                          bsPopover(id="discreteHelp", title="Discrete data rules", content = "\\'continuous parameters to process\\' and \\'discrete parameters to process\\' must match.", 
                                    placement = "right", trigger = "hover"),
                          fileInput("uploaded_discrete_file",
                                    label = NULL,
                                    multiple = FALSE,
                                    buttonLabel=list(tags$b("Browse"),tags$i(class = "fa-solid fa-folder")),
                                    accept = c(
                                      "text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv"
                                    ),
                                    
                          ),
                          hr(),
                          uiOutput("baseParameters"))
                    )),
              ),
              mainPanel(
                width = 9,
                column(width = 12, uiOutput("discreteDateAndTimeBox")),
                fluidRow(column(width = 12, plotlyOutput("display_time_series_discrete"))),
              ) # mainPanel end
            ) # sidebarLayout end
          ), # column close
        ),
        # Data Exploration ----
        tabPanel(
          title="Continuous Data Exploration",
          value="DataExploration",
          fluidPage(
            fluidRow(
              tabsetPanel(
                id = "tabset",
                tags$head(tags$style(HTML(".radio-inline {margin-right: 40px;}"))),
                ## DE, All Parameters ----
                tabPanel("All parameters",
                  value = "all_parameters_tab", br(),
                  tabsetPanel(
                    id = "all_parameters_subtabs",
                    ### DE, All, Summary Tables ----
                    tabPanel("Summary tables",
                      value = "tab_summary_tables",
                      br(),
                      br(),
                      column(
                        width = 12,
                        SummaryTablesModuleUI("DataExpSummaryTbls")
                      ), # column close
                      br(),
                    ), # tabPanel 1 end
                    ### DE, All, TS Plots ----
                    tabPanel("Time series plots",
                      value = "tab_time_series", br(),
                      column(
                        width = 12,
                        DataExplorationTSModuleUI(id="dataExpTS")
                      ), # column close
                     
                    ), # tabPanel 2 end

                    ### DE, All, TS Annual----
                    tabPanel("Time series - Annual overlays",
                      value = "tab_time_series_overlay", br(),
                      column(
                        width = 12,
                        TsOverlayModuleUI(id="tsOverlayTab")
                      ), # column close
                    ), # tabPanel 3 end

                    ### DE, All, Box Plots----
                    tabPanel("Box plots",
                      value = "tab_box", br(),
                      column(
                        width = 12, TsBoxPlotModuleUI(id="tsBoxPlot")
                      ), # column close
                    ), # tabPanel 4 end

                    ### DE, All, CDFs ----
                    tabPanel("CDFs",
                      value = "tab_CDF", br(),
                      column(
                        width = 12, TsCDFPlotModuleUI(id="tsCDFPlot")
                      ), # column close
                      br(),
                    ), # tabPanel 5 end

                    ### DE, All, Raster Graphs ----
                    tabPanel("Raster graphs",
                      value = "tab_raster", br(),
                      column(
                        width = 12, TsRasterPlotModuleUI(id="tsRasterPlot")
                      ), # column close
                    ), # tabPanel 6 end Raste

                    ### DE, All, Climate Spiral ----
                    # tabPanel("Climate spiral", value="tab_climate",br(),
                    #          column(width = 12,
                    #                 sidebarLayout(
                    #                   sidebarPanel(width=3,
                    #                                hr(),
                    #                                uiOutput("climate_input_1"),
                    #                                hr(),
                    #                                uiOutput("climate_input_2"),
                    #
                    #                   ),
                    #                   mainPanel(width=9,
                    #                             column(width=9,uiOutput("display_climate_spiral"))
                    #
                    #                   ) # mainPanel end
                    #
                    #                 ) # sidebarLayout end
                    #
                    #          ), #column close
                    #
                    # ), #tabPanel 7 end Climate Spiral
                  ) # inner tabsetPanel end
                ), # tabPanel end


                # DE, Temperature ----
                tabPanel("Temperature",
                  value = "temp_tab",
                  tabsetPanel(
                    id = "temp_subtabs",
                    ### DE, Temp, Thermal Stats----
                    tabPanel("Thermal statistics",
                      value = "sb1", br(),
                      column(
                        width = 12,ThermalStatsModuleUI("thermalStats")
                      ), # column close
                    ),
                    ### DE, Temp, Air v Water ----
                    tabPanel("Air vs Water",
                      value = "sb2", br(),
                      column(
                        width = 12, AirVsWaterModuleUI("airVsWater")
                      ) # column close
                    ), # AW end

                    ### DE, Temp, GDD ----
                    tabPanel("Growing degree days",
                      value = "sb3", br(),
                       GrowingDegreeModuleUI("growingDegree")
                    ), # GDD, end
                    ### DE, Temp, Therm Class ----
                    tabPanel("Thermal classification",
                      value = "sb4", br(),
                      br(),
                      column(
                        width = 12,ThermalClassificationModuleUI(id="thermalClassification")
                      ) # column close
                    ) # Termal class, end
                  )
                ), # outer tabPanel end temperature

                ## DE, Hydrology ----
                tabPanel("Hydrology",
                  value = "hydro_tab",
                  tabsetPanel(
                    id = "hydro_subtabs",
                    ### DE, Hydro, IHA----
                    tabPanel("IHA",
                      value = "IHA_tab", br(),
                      column(
                        width = 12, IHAModuleUI("IHATab")
                        # sidebarLayout(
                        #   sidebarPanel(
                        #     width = 3,
                        #     div(class="panel panel-default",style="margin:10px;",
                        #         div(class="panel-heading"),
                        #         div(class="panel-body",
                        #             uiOutput("IHA_input_1"),
                        #             uiOutput("IHA_input_2"),
                        #             uiOutput("IHA_input_3"),
                        #             uiOutput("IHA_input_4"),
                        #             uiOutput("display_IHA_button"),
                        #             hr(),
                        #             uiOutput("display_save_IHA_button")
                        #         ) # end of panel body
                        #     ) # end of panel
                        #   ),
                        #   mainPanel(
                        #     width = 9,
                        #     column(
                        #       width = 12,
                        #       uiOutput("display_help_text_IHA"),
                        #       uiOutput("display_IHA_table_1"),
                        #       uiOutput("display_IHA_plot_button_1"),
                        #       shinyjs::hidden(
                        #         div(
                        #           id = "IHA_plot_1_panel",
                        #           conditionalPanel(
                        #             condition = "input$display_IHA_plot_1 %%2 !=0",
                        #             hr(),
                        #             uiOutput("IHA_plot_1"),
                        #           ), # conditionalPanel end
                        #         ) # div end
                        #       ) # shinyjs:: hidden end
                        #       , br(),
                        #       uiOutput("display_IHA_table_2"),
                        #       uiOutput("display_IHA_plot_button_2"),
                        #       shinyjs::hidden(
                        #         div(
                        #           id = "IHA_plot_2_panel",
                        #           conditionalPanel(
                        #             condition = "input$display_IHA_plot_2 %%2 !=0",
                        #             hr(),
                        #             uiOutput("IHA_plot_2a"),
                        #             br(),
                        #             uiOutput("IHA_plot_2b")
                        #           ), # conditionalPanel end
                        #         ) # div end
                        #       ) # shinyjs:: hidden end
                        #       , br(),
                        #       uiOutput("display_IHA_table_3"),
                        #       uiOutput("display_IHA_plot_button_3"),
                        #       shinyjs::hidden(
                        #         div(
                        #           id = "IHA_plot_3_panel",
                        #           conditionalPanel(
                        #             condition = "input$display_IHA_plot_3 %%2 !=0",
                        #             hr(),
                        #             uiOutput("IHA_plot_3")
                        #           ), # conditionalPanel end
                        #         ) # div end
                        #       ) # shinyjs:: hidden end
                        #       , br(),
                        #       uiOutput("display_IHA_table_4"),
                        #       uiOutput("display_IHA_plot_button_4"),
                        #       shinyjs::hidden(
                        #         div(
                        #           id = "IHA_plot_4_panel",
                        #           conditionalPanel(
                        #             condition = "input$display_IHA_plot_4 %%2 !=0",
                        #             hr(),
                        #             uiOutput("IHA_plot_4a"),
                        #             br(),
                        #             uiOutput("IHA_plot_4b")
                        #           ), # conditionalPanel end
                        #         ) # div end
                        #       ) # shinyjs:: hidden end
                        #       , br(),
                        #       uiOutput("display_IHA_table_5"),
                        #       uiOutput("display_IHA_plot_button_5"),
                        #       shinyjs::hidden(
                        #         div(
                        #           id = "IHA_plot_5_panel",
                        #           conditionalPanel(
                        #             condition = "input$display_IHA_plot_5 %%2 !=0",
                        #             hr(),
                        #             uiOutput("IHA_plot_5"),
                        #             br()
                        #           ), # conditionalPanel end
                        #         ) # div end
                        #       ) # shinyjs:: hidden end
                        #     )
                        #   ) # mainPanel end
                        # ) # sidebarLayout end
                      ) # column close
                    ), # tabpanel, end, IHA

                    ### DE, Hydro, Flashiness ----
                    tabPanel("Flashiness",
                      value = "Flashiness_tab", br(),
                      br(),
                      column(
                        width = 12, FlashinessModuleUI("flashinessTab")
                        # sidebarLayout(
                        #   sidebarPanel(
                        #     width = 3,
                        #     hr(),
                        #     uiOutput("flashiness_input_1"),
                        #     hr(),
                        #     uiOutput("display_flashiness_button"),
                        #   ),
                        #   mainPanel(
                        #     width = 9,
                        #     column(
                        #       width = 12,
                        #       uiOutput("display_help_text_flashiness"),
                        #       uiOutput("display_flashiness_table")
                        #     )
                        #   ) # mainPanel end
                        # ) # sidebarLayout end
                      ) # column close
                    ) # tab panel Hydro flash end
                  )
                ) # Hydro, end
              ) # tabsetPanel end
            ) # fluidRow close
          ) # fluidPage close
        )# tabPanel end Data exploration


        # Create Report----
        # tabPanel(
        #   title="Create Report",
        #   value="CreateReport",
        #   fluidPage(
        #     fluidRow(
        #       tabsetPanel(
        #         id = "report_subtabs",
        #         ### CR, Single ----
        #         tabPanel("SingleSite",
        #           value = "SingleSite_tab", br(),
        #           column(
        #             width = 12,
        #             sidebarLayout(
        #               sidebarPanel(
        #                 width = 3,
        #                 hr(),
        #                 radioButtons("report_format",
        #                   "Select report format",
        #                   choices = c("pdf" = "pdf", "html" = "html", "word" = "docx"),
        #                   selected = "html"
        #                 ),
        #                 hr(),
        #                 textInput(
        #                   inputId = "report_name",
        #                   label = "Report file name",
        #                   value = "myReport"
        #                 ),
        #                 hr(),
        #                 actionButton("createReport", "Create report"),
        #                 hr(),
        #                 downloadButton("downloadReport", "Download Report")
        #               ),
        #               mainPanel(
        #                 width = 9,
        #                 column(
        #                   width = 12,
        #                   uiOutput("display_report_content_1"),
        #                   br(),
        #                   uiOutput("display_report_content_2")
        #                 )
        #               ) # mainPanel end
        #             ) # sidebarLayout end
        #           ), # column close
        #           br(),
        #           column(width = 12, uiOutput("display_table_single_site"))
        #         ), # tabPanel close singlesite
        # 
        #         ### CR, Multi----
        #         tabPanel("MultiSites",
        #           value = "MultiSites_tab", br(),
        #           br(),
        #           fluidPage(h4(id = "big-heading", "Coming later")),
        #           column(width = 12, uiOutput("display_table_multiple_sites"))
        #         ) # tabPanel Multi close
        #       ) # tabsetPanel end
        #     ) # fluidRow end
        #   ) # fluidPage end
        # ) # tabPanel end Create Report
      ) # tabsetPanel close
    ),
    fluidRow(column(width=12))
  )
))
