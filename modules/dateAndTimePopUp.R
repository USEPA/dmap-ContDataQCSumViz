# plot module ----
dateAndTime_ui <- function(id) {
  
  # fluidRow(
  #   column(11, plotOutput(NS(id, "plot"))),
  #   column( 1, downloadButton(NS(id, "dnld"), label = ""))
  # )
  showModal(dataModal())
  
}

dateAndTime_server <- function(id, df, vbl, threshhold = NULL) {
  
  moduleServer(id, function(input, output, session) {
    
    # dataModal <- function(failed = FALSE) {
    #   modalDialog(
    #     textInput("dataset", "Choose data set",
    #               placeholder = 'Try "mtcars" or "abc"'
    #     ),
    #     span('(Try the name of a valid data object like "mtcars", ',
    #          'then a name of a non-existent object like "abc")'),
    #     if (failed)
    #       div(tags$b("Invalid name of data object", style = "color: red;")),
    #     
    #     footer = tagList(
    #       modalButton("Cancel"),
    #       actionButton("ok", "OK")
    #     )
    #   )
    # }
    
  })
}

plot_demo <- function() {
  
  # df <- data.frame(day = 1:30, arr_delay = 1:30)
  # ui <- fluidPage(plot_ui("x"))
  # server <- function(input, output, session) {
  #   plot_server("x", reactive({df}), "arr_delay")
  # }
  # shinyApp(ui, server)
  
}