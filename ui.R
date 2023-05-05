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
        div(img(src="headerImg.png", class="headerImgRes"), style="display: flex;justify-content:center")
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
      column(
        width = 1,
        div(class="rectangle",
            span(circleButton(inputId="step1",icon = icon("upload"),status="primary", size="xs")),
            span("Step 1: Upload file", style="font-weight:bold; word-wrap: break-word;margin-right:30px;"),
            # span(HTML('<i id="arrow1" class="fas fa-arrow-right" role="presentation" aria-label="arrow-right icon"></i>'))
           
          )
        ),
        column(
        width = 1),
        column(
          width = 1,
          div(class="rectangle",
                span(circleButton(inputId = "step2",icon = icon("calendar"),status = "primary", size="xs")),
                span("Step 2: Select date and time",style="font-weight:bold; word-wrap: break-word;margin-right:30px;"),
                # span(HTML('<i id="arrow2" class="fas fa-arrow-right" role="presentation" aria-label="arrow-right icon"></i>'))
          )
        ),# end of column
        column(width=1),
        column(
          width = 1,
          div(class="rectangle",
            span(circleButton(inputId = "step3",icon = icon("tasks"),status = "primary", size="xs")),
            span("Step 3: Run meta summary",style="font-weight:bold; word-wrap: break-word;margin-right:30px;"),
            # span(HTML('<i id="arrow3" class="fas fa-arrow-right" role="presentation" aria-label="arrow-right icon"></i>'))
          )
        ),# end of column
        column(width=1),
        column(
          width = 1,
          div(class="rectangle", style="width:280px;",
            span(circleButton(inputId = "step4",icon = icon("calculator"),status = "primary",size="xs")),
            span("Step 4: Calculate daily statistics",style="font-weight:bold; word-wrap: break-word;margin-right:30px;"),
            # span(HTML('<i id="arrow4" class="fas fa-arrow-right" role="presentation" aria-label="arrow-right icon"></i>'))
          )
        ),# end of column
      column(width=1),
      column(
        width = 1,
        div(class="rectangle",style="margin-left:30px;",
          span(circleButton(inputId = "step5",icon = icon("check"),status = "primary", size="xs")),
          span("Step 5: Visualize data",style="font-weight:bold; word-wrap: break-word;")
        )
      )# end of column
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
                              bsPopover(id="fileHelp", title="Microsoft Excel and .csv files known issues", content = "Microsoft Excel corrupts .csv files when reopened by double clicking its icon or by using the File Open dialog. You can avoid this by using the Text or Data Import Wizard from the Excel Data Tab.", 
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
                        sidebarLayout(
                          sidebarPanel(
                            width = 3,
                            # EWL, Start
                            div(class = "panel panel-default",width = "100%",style="margin:10px;",
                                div(class = "panel-heading",
                                    p("You must have completed step 1 to step 4 to use Summary Tables", style='font-weight:bold;font-family: Helvetica Neue, Helvetica, Arial, sans-serif;')
                                ),
                                div(class="panel-body", 
                                    uiOutput("summary_table_input_1"),
                                    uiOutput("summary_table_input_2"),
                                    uiOutput("summary_table_input_3"),
                                    hr(),
                                    uiOutput("summary_table_input_4"),
                                )
                            ),
                          ),
                          mainPanel(
                            width = 9,
                            column(width = 9, uiOutput("display_summary_table_1")),
                            column(width = 9, uiOutput("display_summary_table_2"))
                          ) # mainPanel end
                        ) # sidebarLayout end
                      ), # column close
                      br(),
                    ), # tabPanel 1 end
                    ### DE, All, TS Plots ----
                    tabPanel("Time series plots",
                      value = "tab_time_series", br(),
                      column(
                        width = 12,
                        sidebarLayout(
                          sidebarPanel(
                            width = 3,
                            div(class = "panel panel-default",width = "100%",style="margin:10px;",
                                div(class = "panel-heading"),
                                div(class = "panel-body",
                                  uiOutput("time_series_input_1"),
                                  uiOutput("time_series_input_2"),
                                  div(
                                      id = "cp_shaded_region",
                                      uiOutput("time_series_input_3"),
                                  ), # div end
                                  shinyjs::hidden(
                                    div(
                                      id = "cp_new_data",
                                      conditionalPanel(
                                        condition = "input$dailyStats_shading == 'newData' ",
                                        hr(),
                                        fileInput("uploaded_newData_file",
                                          label = "Upload your new data", multiple = FALSE,
                                          accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                                        )
                                      ) # conditionalPanel end
                                    ) # div end
                                  ), # shinyjs:: hidden end
                                  uiOutput("time_series_input_4"),
                                  hr(),
                                  fluidRow(
                                    column(
                                      width = 9,
                                      uiOutput("time_series_input_5")
                                    ),
                                    column(
                                      width = 6, align = "right",
                                      uiOutput("time_series_input_6")
                                    )
                                  ) # fluidRow close
                                )# end of panel body
                            ) #end of panel
                          ),
                          mainPanel(
                            width = 9,
                            fluidRow(column(width = 12, uiOutput("dateAndTimeBox"))),
                            br(),
                            fluidRow(column(width = 12, plotlyOutput("display_time_series"))),
                            br(),
                            fluidRow(column(width = 12, plotlyOutput("display_time_series_new"))),
                            br(),
                            fluidRow(column(width = 12, plotlyOutput("display_time_series_1"))),
                            br(),
                            fluidRow(column(width = 12, uiOutput("display_time_series_2"))),
                            br(),
                            fluidRow(column(width = 12, plotlyOutput("display_time_series_3")))
                          ) # mainPanel end
                        ) # sidebarLayout end
                      ), # column close
                      br(),
                      column(
                        width = 12,
                        uiOutput("another_time_series_UI")
                      )
                    ), # tabPanel 2 end

                    ### DE, All, TS Annual----
                    tabPanel("Time series - Annual overlays",
                      value = "tab_time_series_overlay", br(),
                      column(
                        width = 12,
                        sidebarLayout(
                          sidebarPanel(
                            width = 3,
                            div(class="panel panel-default",style="margin:10px;",
                                div(class="panel-heading"),
                                div(class="panel-body",
                                    uiOutput("time_series_overlay_input_1"),
                                    uiOutput("time_series_overlay_input_2"),
                                    uiOutput("time_series_overlay_input_3"),
                                    uiOutput("time_series_overlay_input_4"),
                                    shinyjs::hidden(
                                      div(
                                        id = "cp_new_data_overlay",
                                        conditionalPanel(
                                          condition = "input$overlay_shading == 'newData' ",
                                          fileInput("uploaded_overlay_newData_file",
                                            label = "Upload your new data", multiple = FALSE,
                                            accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                                          ),
                                          selectizeInput("overlay_newData_lower_col",
                                            label = "Select column to be used as lower bound",
                                            choices = NULL,
                                            multiple = FALSE,
                                            selected = NULL,
                                            options = list(hideSelected = FALSE)
                                          ),
                                          selectizeInput("overlay_newData_longterm_col",
                                            label = "Select column to be used as long-term reference line",
                                            choices = NULL,
                                            multiple = FALSE,
                                            selected = NULL,
                                            options = list(hideSelected = FALSE)
                                          ),
                                          selectizeInput("overlay_newData_upper_col",
                                            label = "Select column to be used as upper bound",
                                            choices = NULL,
                                            multiple = FALSE,
                                            selected = NULL,
                                            options = list(hideSelected = FALSE)
                                          ),
                                          selectizeInput("overlay_newData_date_col",
                                            label = "Select month-day column",
                                            choices = NULL,
                                            multiple = FALSE,
                                            selected = NULL,
                                            options = list(hideSelected = FALSE)
                                          ),
                                          textInput("overlay_newData_name",
                                            label = "New data name",
                                            value = "USGS"
                                          )
                                        ), # conditionalPanel end
                                      ) # div end
                                    ), # shinyjs:: hidden end
                              hr(),
                              uiOutput("time_series_overlay_input_5"),
                            ) # end of panel body
                            ) # end of panel
                          ),
                          mainPanel(
                            width = 9,
                            fluidRow(column(width = 9, uiOutput("display_time_series_overlay"))),
                            br(),
                            fluidRow(column(width = 9, uiOutput("display_time_series_overlay_1"))),
                            br()
                          ) # mainPanel end
                        ) # sidebarLayout end
                      ), # column close
                    ), # tabPanel 3 end

                    ### DE, All, Box Plots----
                    tabPanel("Box plots",
                      value = "tab_box", br(),
                      column(
                        width = 12,
                        sidebarLayout(
                          sidebarPanel(
                            width = 3,
                           div(class="panel panel-default",style="margin:10px;",
                               div(class="panel-heading"),
                               div(class="panel-body",
                                      uiOutput("box_input_1"),
                                      uiOutput("box_input_2"),
                                      uiOutput("box_input_3"),
                                      uiOutput("box_input_4"),
                                      uiOutput("box_input_5")
                               )#end of panel body
                           )# end of panel
                          ),
                          mainPanel(
                            width = 9,
                            fluidRow(column(width = 9, uiOutput("display_box_plots"))),
                            br(),
                            br(),
                          ) # mainPanel end
                        ) # sidebarLayout end
                      ), # column close
                    ), # tabPanel 4 end

                    ### DE, All, CDFs ----
                    tabPanel("CDFs",
                      value = "tab_CDF", br(),
                      column(
                        width = 12,
                        sidebarLayout(
                          sidebarPanel(
                            width = 3,
                            div(class="panel panel-default",style="margin:10px;",
                              div(class="panel-heading"),
                              div(class="panel-body",
                                uiOutput("CDF_input_1"),
                                uiOutput("CDF_input_2"),
                                uiOutput("CDF_input_3"),
                                uiOutput("CDF_input_4"),
                                uiOutput("CDF_input_5"),
                                hr(),
                                uiOutput("display_CDF_button")
                              )#end of panel body
                            )#end of panel
                          ),
                          mainPanel(
                            width = 9,
                            fluidRow(column(width = 9, uiOutput("display_plot_CDF"))),
                            br(),
                            br(),
                          ) # mainPanel end
                        ) # sidebarLayout end
                      ), # column close
                      br(),
                    ), # tabPanel 5 end

                    ### DE, All, Raster Graphs ----
                    tabPanel("Raster graphs",
                      value = "tab_raster", br(),
                      column(
                        width = 12,
                        sidebarLayout(
                          sidebarPanel(
                            width = 3,
                           div(class="panel panel-default",style="margin:10px;",
                                div(class="panel-heading"),
                                div(class="panel-body",
                                    uiOutput("raster_input_1"),
                                    uiOutput("raster_input_2"),
                                    uiOutput("raster_input_3"),
                                    uiOutput("raster_input_4"),
                                    uiOutput("raster_input_5"),
                                    hr(),
                                    uiOutput("raster_input_6")
                               ) # end of panel body
                           ) #end of panel
                          ),
                          mainPanel(
                            width = 9,
                            column(width = 9, uiOutput("display_raster_graphs"))
                          ) # mainPanel end
                        ) # sidebarLayout end
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
                    tags$head(tags$style("#help_text_air_water,#help_text_water_temp_class,#help_text_thermal_statistics,#help_text_growing_degree_days
                                                                                          {font-size:16px;color:black;font-style:bold;display:block; }")),
                    ### DE, Temp, Thermal Stats----
                    tabPanel("Thermal statistics",
                      value = "sb1", br(),
                      column(
                        width = 12,
                        sidebarLayout(
                          sidebarPanel(
                            width = 3,
                            div(class="panel panel-default",style="margin:10px;",
                                div(class="panel-heading"),
                                div(class="panel-body",
                                    uiOutput("thermal_input_1"),
                                    uiOutput("thermal_input_2"),
                                    uiOutput("thermal_input_3"),
                                    uiOutput("display_run_thermal_button"),
                                    hr(),
                                    uiOutput("display_save_thermal_button"),
                                )#end of panel body
                            ) # end of panel
                          ),
                          mainPanel(
                            width = 9,
                            column(
                              width = 12,
                              uiOutput("display_help_text_thermal_statistics"),
                              uiOutput("display_thermal_table_1"),
                              br(),
                              uiOutput("display_thermal_table_2"),
                              br(),
                              uiOutput("display_thermal_table_3"),
                              br(),
                              uiOutput("display_thermal_table_4"),
                              br(),
                              uiOutput("display_thermal_table_5")
                            )
                          ) # mainPanel end
                        ) # sidebarLayout end
                      ), # column close
                    ),
                    ### DE, Temp, Air v Water ----
                    tabPanel("Air vs Water",
                      value = "sb2", br(),
                      column(
                        width = 12,
                        sidebarLayout(
                          sidebarPanel(
                            width = 3,
                            div(class="panel panel-default",style="margin:10px;",
                                div(class="panel-heading"),
                                div(class="panel-body",
                                      uiOutput("air_vs_water_input_1"),
                                      uiOutput("air_vs_water_input_2"),
                                      radioButtons("exclude_data_points",
                                        "Limit the data points with air temperature",
                                        choices = c("No" = "No", "Yes" = "Yes"),
                                        selected = "No"
                                      ),
                                      shinyjs::hidden(
                                        div(
                                          id = "cp_air_temp",
                                          conditionalPanel(
                                            condition = "input$exclude_data_points == 'Yes' ",
                                            uiOutput("air_vs_water_input_4"),
                                          ), # conditionalPanel end
                                        ) # div end
                                      ), # shinyjs:: hidden end
                                      hr(),
                                      uiOutput("display_thermal_sensitivity_button")
                                )#end of panel body
                            ) #end of panel
                          ),
                          mainPanel(
                            width = 9,
                            column(
                              width = 12,
                              uiOutput("display_help_text_air_water"),
                              uiOutput("display_thermal_sensitivity_plot_1"),
                              br(),
                              uiOutput("display_thermal_sensitivity_plot_2")
                            )
                          ) # mainPanel end
                        ) # sidebarLayout end
                      ) # column close
                    ), # AW end

                    ### DE, Temp, GDD ----
                    tabPanel("Growing degree days",
                      value = "sb3", br(),
                      br(),
                      #column(
                        #width = 12,
                            # sidebarLayout(
                            #   sidebarPanel(
                            # width = 3,
                            # hr(),
                            # uiOutput("growing_degree_days_input_1"),
                            # hr(),
                            # uiOutput("display_growing_degree_days_button"),
                          #),
                          #mainPanel(
                           # width = 11,
                            column(
                              width = 12,
                              uiOutput("display_help_text_growing_degree_days"),
                              div(
                                id = "hyper_link_panel",
                                conditionalPanel(
                                  condition = "0==0",
                                  a("Download R script", href = "GDD.R")
                                ), # conditionalPanel end
                              ) # div end
                            ),
                            column(
                              width = 12,
                              uiOutput("display_growing_degree_days_table")
                            )
                          #) # mainPanel end
                        #) # sidebarLayout end
                      #) # column close
                    ), # GDD, end

                    ### DE, Temp, Therm Class ----
                    tabPanel("Thermal classification",
                      value = "sb4", br(),
                      br(),
                      column(
                        width = 12,
                        sidebarLayout(
                          sidebarPanel(
                            width = 3,
                            div(class = "panel panel-default",style="margin:10px;",
                                div(class="panel-heading"),
                                div(class="panel-body",
                                    uiOutput("water_temp_class_input_1"),
                                    hr(),
                                    uiOutput("display_water_temp_class_button")
                            )#endo of panel body
                            )#end of panel
                          ),
                          mainPanel(
                            width = 9,
                            column(
                              width = 12,
                              uiOutput("display_help_text_water_temp_class"),
                              uiOutput("display_water_temp_class_table")
                            )
                          ) # mainPanel end
                        ) # sidebarLayout end
                      ) # column close
                    ) # Termal class, end
                  )
                ), # outer tabPanel end temperature

                ## DE, Hydrology ----
                tabPanel("Hydrology",
                  value = "hydro_tab",
                  tabsetPanel(
                    id = "hydro_subtabs",
                    tags$head(tags$style("#help_text_IHA,#help_text_flashiness
                              {font-size:16px;color:black;font-style:bold;display:block; }")),
                    ### DE, Hydro, IHA----
                    tabPanel("IHA",
                      value = "IHA_tab", br(),
                      column(
                        width = 12,
                        sidebarLayout(
                          sidebarPanel(
                            width = 3,
                            div(class="panel panel-default",style="margin:10px;",
                                div(class="panel-heading"),
                                div(class="panel-body",
                                    uiOutput("IHA_input_1"),
                                    uiOutput("IHA_input_2"),
                                    uiOutput("IHA_input_3"),
                                    uiOutput("IHA_input_4"),
                                    uiOutput("display_IHA_button"),
                                    hr(),
                                    uiOutput("display_save_IHA_button")
                                ) # end of panel body
                            ) # end of panel
                          ),
                          mainPanel(
                            width = 9,
                            column(
                              width = 12,
                              uiOutput("display_help_text_IHA"),
                              uiOutput("display_IHA_table_1"),
                              uiOutput("display_IHA_plot_button_1"),
                              shinyjs::hidden(
                                div(
                                  id = "IHA_plot_1_panel",
                                  conditionalPanel(
                                    condition = "input$display_IHA_plot_1 %%2 !=0",
                                    hr(),
                                    uiOutput("IHA_plot_1"),
                                  ), # conditionalPanel end
                                ) # div end
                              ) # shinyjs:: hidden end
                              , br(),
                              uiOutput("display_IHA_table_2"),
                              uiOutput("display_IHA_plot_button_2"),
                              shinyjs::hidden(
                                div(
                                  id = "IHA_plot_2_panel",
                                  conditionalPanel(
                                    condition = "input$display_IHA_plot_2 %%2 !=0",
                                    hr(),
                                    uiOutput("IHA_plot_2a"),
                                    br(),
                                    uiOutput("IHA_plot_2b")
                                  ), # conditionalPanel end
                                ) # div end
                              ) # shinyjs:: hidden end
                              , br(),
                              uiOutput("display_IHA_table_3"),
                              uiOutput("display_IHA_plot_button_3"),
                              shinyjs::hidden(
                                div(
                                  id = "IHA_plot_3_panel",
                                  conditionalPanel(
                                    condition = "input$display_IHA_plot_3 %%2 !=0",
                                    hr(),
                                    uiOutput("IHA_plot_3")
                                  ), # conditionalPanel end
                                ) # div end
                              ) # shinyjs:: hidden end
                              , br(),
                              uiOutput("display_IHA_table_4"),
                              uiOutput("display_IHA_plot_button_4"),
                              shinyjs::hidden(
                                div(
                                  id = "IHA_plot_4_panel",
                                  conditionalPanel(
                                    condition = "input$display_IHA_plot_4 %%2 !=0",
                                    hr(),
                                    uiOutput("IHA_plot_4a"),
                                    br(),
                                    uiOutput("IHA_plot_4b")
                                  ), # conditionalPanel end
                                ) # div end
                              ) # shinyjs:: hidden end
                              , br(),
                              uiOutput("display_IHA_table_5"),
                              uiOutput("display_IHA_plot_button_5"),
                              shinyjs::hidden(
                                div(
                                  id = "IHA_plot_5_panel",
                                  conditionalPanel(
                                    condition = "input$display_IHA_plot_5 %%2 !=0",
                                    hr(),
                                    uiOutput("IHA_plot_5"),
                                    br()
                                  ), # conditionalPanel end
                                ) # div end
                              ) # shinyjs:: hidden end
                            )
                          ) # mainPanel end
                        ) # sidebarLayout end
                      ) # column close
                    ), # tabpanel, end, IHA

                    ### DE, Hydro, Flashiness ----
                    tabPanel("Flashiness",
                      value = "Flashiness_tab", br(),
                      br(),
                      column(
                        width = 12,
                        sidebarLayout(
                          sidebarPanel(
                            width = 3,
                            hr(),
                            uiOutput("flashiness_input_1"),
                            hr(),
                            uiOutput("display_flashiness_button"),
                          ),
                          mainPanel(
                            width = 9,
                            column(
                              width = 12,
                              uiOutput("display_help_text_flashiness"),
                              uiOutput("display_flashiness_table")
                            )
                          ) # mainPanel end
                        ) # sidebarLayout end
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
