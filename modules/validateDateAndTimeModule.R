validateDateAndTimeUI <- function(id) {
  ns <- NS(id)
  shinyjs::useShinyjs()
  tagList(
    fluidRow(
      column(width=12,uiOutput(ns("validationDivId")))
    )
  )
}

validateDateAndTimeServer <- function(id,dateColumnNums,parmToProcess,dateFieldNameId,dateFormatId) {
  missingInputsL <- reactive(FALSE)
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      print("from validateDateAndTimeModule")
      print(str(dateColumnNums))
      print(str(parmToProcess))
      

      if (dateColumnNums == "combined" &
          (
            is.null(parmToProcess) |
            parmToProcess == "" |
            dateFieldNameId == "" |
            dateFormatId == ""
          )) {
        missingInputsL <- TRUE
        shinyjs::runjs(paste0("$('#",ns('validationDivId'),"').empty()"))
        output$validationDivId <- renderUI({
          shiny::validate(
            shiny::need(dateFormatId != "", 'Please select date format'),
            shiny::need(
              dateFieldNameId != "",
              'Please select date field name.'
            ),
            shiny::need(
              !is.null(parmToProcess),
              'Please select parameters to process.'
            )
          )
        })
      }
      return(
        list(
          missingInputs=missingInputsL
        )
      )
    })
}
