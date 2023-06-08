#' Continuous Data Exploration / Temperature / Thermal Classification (user interface side)
#'
#' @param id 
#'
ThermalClassificationModuleUI <- function(id) {
  ns <- NS(id)
  shinyjs::useShinyjs()
  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(class = "panel panel-default",style="margin:10px;",
          div(class="panel-heading"),
          div(class="panel-body",
              uiOutput(ns("water_temp_class_input_1")),
              hr(),
              uiOutput(ns("display_water_temp_class_button"))
          )#endo of panel body
      )#end of panel
    ),
    mainPanel(
      width = 9,
      column(
        width = 12,
        #uiOutput(ns("display_help_text_water_temp_class")),
        uiOutput(ns("errorDiv")),
        shinydashboard::box(id=ns("display_help_text_water_temp_class"), style="display:none;", width=12, class="well",
                            h4("Temperature – Thermal classification"),
                            br(),
                            div(style="width:100%;","COMING LATER ..."),
                            br(),
                            div(style="width:100%;", "When resources permit, we will add in functions that assign sites to thermal classes based on the national classification schemes in Maheu et al. (2015) and McManamay & DeRolph (2018)."),
                            br(),
                            div(style="width:100%;", "The Maheu et al. (2015) scheme has six annual thermal regime classes: stable cold, stable cool, variable cold, variable cool, highly variable cool and variable warm.
                                                      McManamay & DeRolph (2018) used two schemes: one based on Maheu et al. (2015) and the other based on the following ranges of mean July-August stream temperature:"),
                            tags$ul(
                              tags$li("Very cold <10°C"),
                              tags$li("Cold 10-15°C"),
                              tags$li("Cold-cool 15-18°C"),
                              tags$li("Cool 18-21°C"),
                              tags$li("Cool-warm 21-24°C"),
                              tags$li("Warm >24°C")
                             ),
                            br(),
                            div(style="width:100%", "Citations:
                                Maheu, Audrey & Poff, N. & St-Hilaire, André. 2015. A Classification of Stream Water Temperature Regimes in the Conterminous USA. River Research and Applications. 32. 10.1002/rra.2906."),
                            br(),
                            div(style="width:100%", "McKay, L., Bondelid, T., Dewald, T., Johnston, J., Moore, R., Reah, A., 2012. NHDPlus Version 2: User Guide. U.S. Environmental Protection Agency.",
                                a('https://nhdplus.com/NHDPlus/NHDPlusV2_home.php', href='https://nhdplus.com/NHDPlus/NHDPlusV2_home.php', target='_blank')),
                            br(),
                            div(style="width:100%", "McManamay, R. & C.A. DeRolph. 2019. A stream classification system for the conterminous United States. Sci Data 6, 190017.",
                                a('https://doi.org/10.1038/sdata.2019.17', href='https://doi.org/10.1038/sdata.2019.17', target='_blank'))


        ), # end of box
        DT::dataTableOutput(ns("display_water_temp_class_table"))
      )

    ) # mainPanel end
  ) # sidebarLayout end

}

#' Continuous Data Exploration / Temperature / Thermal Classification (server side)
#'
#' @param id 
#' @param dailyStats 
#' @param uploaded_data 
#' @param renderThermalClassification 
#'
ThermalClassificationModuleServer <- function(id, dailyStats,uploaded_data, renderThermalClassification) {
  localStats <- reactiveValues(stats=list())
  variables_avail <- reactiveValues()
  moduleServer(
    id,
    function(input, output, session) {
          ns <- session$ns
          #water_temp_name_in_class <- shiny::reactive(water_temp_name_in_class)

          # observeEvent(uploaded_data(), {
          #   water_to_select <- getSelectedVal(uploaded_data())
          #   variables_avail <- names(uploaded_data())
          #   #updateSelectInput(session,ns("water_temp_name_in_class"),choices = c("", variables_avail),selected=water_to_select)
          # })

       
          # observe({
          #   localStats <- dailyStats
          #   localStats$stats <- localStats$processed_dailyStats
          # })

           observe({
            if(renderThermalClassification$render == TRUE) {
              shinyjs::show(id=ns("display_help_text_water_temp_class"), asis=TRUE)
              localStats <- dailyStats
              localStats$stats <- localStats$processed_dailyStats
              #gets into errors when column is not found in the processed list
              variables_avail <- names(uploaded_data())
              output$water_temp_class_input_1 <- renderUI({
                water_to_select <- getSelectedVal(uploaded_data())
                selectizeInput(ns("water_temp_name_in_class"),label ="Select Water Temperature Column",
                               choices=variables_avail,
                               multiple = FALSE,
                               selected=water_to_select,
                               options = list(hideSelected = FALSE))
              })

              output$display_water_temp_class_button <- renderUI({
                actionButton(inputId=ns("display_water_class"), label="Display water temperature class",class="btn btn-primary")
              })

              #Nilima Gandhi - Remvoing old way, it is a overkill, but keeping the code, do not know if there is a future plan to use the file.
              # output$display_help_text_water_temp_class <- renderUI({
              #   verbatimTextOutput("help_text_water_temp_class")
              # })
              #
              # output$help_text_water_temp_class <- renderText({
              #   filePath <- "help_text_files/Temperature_Classification.txt"
              #   fileText <- paste(readLines(filePath,encoding="UTF-8"),collapse="\n")
              #   fileText
              # })

            }

          })
           
           getSelectedVal <- function(uploaded_data) {
             variables_avail <- names(uploaded_data)
             water_to_select <- NULL
             water_keys_in_favor_order <- c("Water.Temp.C","WATER.TEMP.C","Water_Temp_C","WATER_TEMP_C")
             possible_water_columns <- water_keys_in_favor_order[water_keys_in_favor_order %in% variables_avail]
             if (length(possible_water_columns)==0){
                water_to_select <- variables_avail[grep('water',variables_avail,ignore.case=TRUE)][1]
             }else{
               water_to_select <- possible_water_columns[1]
             }
             return(water_to_select)
           }

           observeEvent(input$display_water_class, {
             localStats <- dailyStats
             #remove previous error messages if any
             output$errorDiv <- renderUI({})
             output$display_water_temp_class_table <- DT::renderDataTable({})
             shinyjs::hide(id=ns("display_help_text_water_temp_class"), asis=TRUE)

             tryCatch({
             myList <- localStats$processed_dailyStats
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
             }, error=function(e){
                errorMsg <- print(paste0("Error in thermalClassification", e$message))
               if(e$message == "attempt to select less than one element in get1index") {
                   errorMsg[1] <- "Selected column is not found in the processed data list"
               } else {
                 errorMsg[1] <- e$message
                  
               }
                print(paste0("Error in thermalClassification", e$message))
                output$errorDiv <- renderUI({
                  div(h4(errorMsg), class="alert alert-danger")
                })

             })

             output$display_water_temp_class_table <- DT::renderDataTable({
               myTable <- DT::datatable(
                 calculated.mean,
                 extensions ="Buttons",
                 rownames = FALSE,
                 options = list(
                   scrollX = FALSE, #allow user to scroll wide tables horizontally
                   stateSave = FALSE,
                   pageLength = 15,
                   dom = 'Bt',
                   buttons = list(
                     list(extend='copy', text='Copy', className="btn btn-primary"),
                     list(extend='print', text='Print', className="btn btn-primary"),
                     list(extend='collection', buttons = c('csv','excel','pdf'), text='Download', className="btn btn-primary")
                   ),
                   columnDefs = list(list(className="dt-center",targets="_all")),
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'background-color': '#e3e3e3', 'color': '#000'});",
                     "$('.dt-buttons button').removeClass('dt-button');",
                     "}")
                 )
               ) # dataTable end
               print(myTable)
             })  ## renderDataTable ebd

           })  ##observeEvent end



    }) # end of module server
}
