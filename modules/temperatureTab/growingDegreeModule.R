#' Continuous Data Exploration / Temperature / Growing degree days (user interface side)
#'
#' @param id 
#'
GrowingDegreeModuleUI <- function(id) {
  ns <- NS(id)
  shinyjs::useShinyjs()
  
  #currently sidebarLayout is not needed so commentting it out
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
        shinydashboard::box(id=ns("display_help_text_growing_degree_days"), style="display:none;", width=12, class="well",
                h4("Temperature – Growing Degree Days"),
                div("COMING LATER…"),
                div(style="width:100%;", "When resources permit, we will add in a function to calculate Growing Degree Days (GDD), which are used to estimate the growth and development of insects during the growing season.
                    The basic concept is that development will only occur if the temperature exceeds some minimum development threshold, or base temperature, which varies depending on the type of organisms being studied."),
                br(),
                div(style="width:100%;", "Although we aren’t able to provide a GDD calculator at this time, one of the RMN partners, Tim Martin from Minnesota DNR (tim.martin@state.mn.us),
                has generously shared code that people with R software can use in the meantime. Click below to download the R script."),
                div(
                  id = "hyper_link_panel",
                  conditionalPanel(
                    condition = "0==0",
                    a("Download R script", href = "GDD.R")
                  ), # conditionalPanel end
                ) # div end
        ), # end of box
      )
  # column(
  #   width = 12,
  #   uiOutput("display_growing_degree_days_table")
  # )
  #) # mainPanel end
  #) # sidebarLayout end
  #) # column close


}

#' Continuous Data Exploration / Temperature / Growing degree days (server side)
#'
#' @param id 
#' @param renderGrowingDegree 
#'
GrowingDegreeModuleServer <- function(id, renderGrowingDegree) {
  moduleServer(
    id,
    function(input, output, session) {
          ns <- session$ns
           observe({
            if(renderGrowingDegree$render == TRUE) {
              shinyjs::show(id=ns("display_help_text_growing_degree_days"), asis=TRUE)
              
              #Nilima Gandhi - Remvoing old way, it is a overkill, but keeping the code, do not know if there is a future plan to use the file.
              
              # output$display_help_text_growing_degree_days <- renderUI({
              #   verbatimTextOutput(ns("help_text_growing_degree_days"))
              # })
              # 
              # output$help_text_growing_degree_days <- renderText({
              #   filePath <- "help_text_files/Temperature_GrowingDegreeDays.txt"
              #   fileText <- paste(readLines(filePath,encoding="UTF-8"),collapse="\n")
              #   fileText
              # })
              
            }

          })
    }) # end of module server
}
