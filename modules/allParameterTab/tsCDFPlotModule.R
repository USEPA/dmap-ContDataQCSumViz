TsCDFPlotModuleUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(class="panel panel-default",style="margin:10px;",
          div(class="panel-heading"),
          div(class="panel-body",
              uiOutput(ns("CDF_input_1")),
              uiOutput(ns("CDF_input_2")),
              uiOutput(ns("CDF_input_3")),
              uiOutput(ns("CDF_input_4")),
              uiOutput(ns("CDF_input_5")),
              hr(),
              uiOutput(ns("display_CDF_button"))
          )#end of panel body
      )#end of panel
    ),
    mainPanel(
      width = 9,
      fluidRow(column(width = 12, plotlyOutput(ns("display_plot_CDF"))))
    ) # mainPanel end
  ) # sidebarLayout end
}

TsCDFPlotModuleServer <- function(id, dailyStats, renderCDFPlot) {
 
  localStats <- reactiveValues(stats=list())
  variables_avail <- reactiveValues(params=list())
  variable_to_plot <- reactiveVal()
 
  
  moduleServer(
    id,
    function(input, output, session) {
          ns <- session$ns
          observe({
            print("ng observing in tsOverlay")
            localStats <- dailyStats
            variables_avail$params <- names(localStats$processed_dailyStats)
            localStats$stats <- localStats$processed_dailyStats
            print(localStats$stats)
          })
          
          observe({
           
            localStats <- dailyStats
            myList <- localStats$processed_dailyStats
            variables_avail$params <- names(localStats$processed_dailyStats)

           
            if(renderCDFPlot$render == TRUE) {
              output$CDF_input_1 <- renderUI({
                selectizeInput(ns("CDF_variable_name"),label ="Select variable name",
                               choices=variables_avail$params,
                               multiple = FALSE,
                               selected=variables_avail$params[1],
                               options = list(hideSelected = FALSE))
              })
              
              
              output$CDF_input_2 <- renderUI({
                div(
                  radioButtons(ns("CDF_shading"), "Add shading with", choices = c("25th & 75th percentiles"="quantiles",
                                                                              "minimum & maximum"="minMax"
                  ),
                  selected = "minMax"))
                
              })
  
              output$CDF_input_3 <- renderUI({
                variable_to_plot <- ifelse(is.null(input$CDF_variable_name), variables_avail$params[1], input$CDF_variable_name)
                myData.all <- myList[[which(names(myList)==variable_to_plot)]]
                myData.all[,"year"] <- format(myData.all[,"Date"],"%Y")
                selectizeInput(ns("CDF_select_year"),label ="Select year",
                               choices=c("All", unique(myData.all[,"year"])),
                               multiple = FALSE,
                               selected = "All",
                               options = list(hideSelected = FALSE))
              })
              
              output$CDF_input_4 <- renderUI({
                selectizeInput(ns("CDF_select_season"),label ="Select season",
                               choices=c("All","Fall", "Winter", "Spring","Summer" ),
                               multiple = FALSE,
                               selected = "All",
                               options = list(hideSelected = FALSE))
              })
              
              output$CDF_input_5 <- renderUI({
                
                textInput(inputId=ns("CDF_title"), label="Plot title",value="")
                
              })
              
              output$display_CDF_button <- renderUI({
                actionButton(inputId=ns("run_CDF"), label="Run and display",class="btn btn-primary")
              })

            }
          })
          
          observeEvent(input$run_CDF, {
            localStats <- dailyStats
            # output$display_plot_CDF <- renderUI({
            #   withSpinner(plotlyOutput("plot_CDF",height="600px",width="1200px"),type=2)
            # })
            myList <- localStats$processed_dailyStats
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
            
            output$display_plot_CDF <- renderPlotly({
              CDF_plot <- CompSiteCDF.updated(file.input = NULL
                                              , dir.input = getwd()
                                              , dir.output = getwd()
                                              , Param.Name = mean_col
                                              , Shaded.Names = c(lower_col,upper_col)
                                              , Plot.title = isolate(input$CDF_title)
                                              , Plot.season = isolate(season.choice)
                                              , hist.columnName = NULL
                                              , df.input = data.plot)
              
              if (is.null(CDF_plot)){
                shinyalert("Warning","No data available to plot for the selected variable/year/season!",closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                           confirmButtonText="OK",inputId="alert_data_not_avail_for_CDF")
              } else {
                CDF_plot <- ggplotly(CDF_plot) %>% plotly::layout(legend = list(orientation = "h", x = 0.4, y = -0.3))
              }
              print(CDF_plot)
            })
          }) # observeEvent close
          
          observeEvent(input$alert_data_not_avail_for_CDF,{
            #print(input$alert_no_date)
            shinyjs::runjs("swal.close();")
          })

    })# end of server module
}
