#' Continuous Data Exploration / Hydrology / Flashiness (user interface side)
#' @param id 
#'
FlashinessModuleUI <- function(id) {
  ns <- NS(id)
  shinyjs::useShinyjs()
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
      column(
        width = 12,
        #uiOutput("display_help_text_flashiness")
        shinydashboard::box(id=ns("display_help_text_flashiness"), style="display:none;", width=12, class="well",
                            h4("Hydrology – Flashiness"),
                            div("COMING LATER…"),
                            div(style="width:100%;", "The Richards-Baker flashiness index (RBI) (Baker et al. 2004) reflects the frequency and rapidity of short-term changes in streamflow. 
                                It measures oscillations in discharge relative to total discharge, such that flashier streams receive higher scores. 
                                Results are scaled from 0 to 1 (most flashy)."),
                            br(),
                            div(style="width:100%;", "The calculation is based on mean daily flows and is calculated by dividing the sum of the absolute values of day-to-day changes in mean daily flow by total discharge during the specified time period."),
                            br(),
                            div(style="width:100%;", "The Shiny app calculation is based on mean daily values and calendar year. Those settings can be changed if you are using the R package instead of Shiny app. 
                                For more information, contact Erik Leppo (Erik.Leppo@tetratech.com)."),
                            br(),
                            div(style="width:100%;", "The RBI is intended to be used with discharge data but we’re experimenting with using it with sensor depth and water level data as well (since discharge data aren’t available for some of the RMN sites)."),
                            br(),
                            div(style="width:100%;", "Citation:"), 
                            div(style="width:100%;",    "Baker, D.B., Richards, R.P., Loftus, T.T. and J.K. Kramer. 2004. A New Flashiness Index: Characteristics and Applications to Midwestern Rivers and Streams. 
                                Journal of the American Water Resources Association 40(2): 503-522."),
                            a('https://doi.org/10.1111/j.1752-1688.2004.tb01046.x', href='https://doi.org/10.1111/j.1752-1688.2004.tb01046.x', target='_blank')

        ), # end of box
       
      )
  #   ) # mainPanel end
  # ) # sidebarLayout end
}

#' Continuous Data Exploration / Hydrology / Flashiness (server side)
#'
#' @param id 
#' @param renderFlashiness 
#'
FlashinessModuleServer <- function(id, renderFlashiness) {
  moduleServer(
    id,
    function(input, output, session) {
          ns <- session$ns
            observe({
            if(renderFlashiness$render == TRUE) {
              shinyjs::show(id=ns("display_help_text_flashiness"), asis=TRUE)
            }
          })
          
  }) # end of module server
}
