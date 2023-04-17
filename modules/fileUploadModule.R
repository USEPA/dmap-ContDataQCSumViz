
fileUpload_UI <- function(id, mLabel = "") {
  ns <- NS(id)
  tagList(
    fileInput(ns("uploaded_data_file"),
              label = h4(
                id = "big-heading",
                label=mLabel
              ),
              multiple = FALSE,
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"
              )
    ),
    hr(),
    actionButton(
        inputId = ns("uploadId"),
        label = "Use this file",
        class = "action-btn-style"
    )
  )
}

fileUpload_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      userFile <- reactive({
        # If no file is selected, don't do anything
        validate(need(input$uploaded_data_file, message = FALSE))
        input$uploaded_data_file
      })
      dataframe <- reactive({
        read.csv(userFile()$datapath,stringsAsFactors = stringsAsFactors)
      })
      return(dataframe)
    }
  )
}
