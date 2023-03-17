#' Download DayMet Data
#'
#' This a wrapper function for the package dataRetrieval to get DayMet data.
#' Daymet gridded daily  are the default data type.
#'Uses library("daymetr") to download dayMetdata
#Citation
# Hufkens K., Basler J. D., Milliman T. Melaas E., Richardson A.D. 2018 An integrated phenology modelling framework in 
#R: Phenology modelling with phenor. Methods in Ecology & Evolution, 9: 1-10.
# 
# Acknowledgements
# This project was supported by the National Science Foundation’s Macro-system
# Biology Program (awards EF-1065029 and EF-1702697) and the Marie Skłodowska-Curie Action (H2020 grant 797668). 
# Logo design elements are taken from the FontAwesome library according to these terms, 
# where the globe element was inverted and intersected.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library("daymetr")
#' @export
validateUserInputs <- function(dateColumnNums,parmToProcess, dateFieldName,dateFormat,timeFieldName,timeFormat,elementId){
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
  tryCatch({
    missingInputs <- FALSE
    # shinyjs::alert(input[[dateColumnNums]])
    # shinyjs::alert(input[[parmToProcess]])
    # shinyjs::alert(input[[dateFormat]])
    # shinyjs::alert(input[[dateFieldName]])
    # shinyjs::alert(input[[timeFieldName]])
    
    if (input[[dateColumnNums]] == "dtCombined" &
        (
          is.null(input[[parmToProcess]]) |
          input[[dateFormat]] == "" |
          input[[dateFieldName]] == ""
        )) {
      missingInputs <- TRUE
      shinyjs::runjs(paste0("$('#",elementId,"').empty()"))
      output[[elementId]] <- renderUI({
        shiny::validate(
          shiny::need(input[[dateFormat]] != "", 'Please select date format'),
          shiny::need(
            input[[dateFieldName]] != "",
            'Please select date field name.'
          ),
          shiny::need(
            !is.null(input[[parmToProcess]]),
            'Please select parameters to process.'
          )
        )
      })
    } else if (input[[dateColumnNums]] == 'dtSeparated' &
               (
                 is.null(input[[parmToProcess]]) |
                 input[[dateFormat]] == "" |
                 input[[dateFieldName]] == "" |
                 input[[timeFieldName]] == ""
               )) {
      missingInputs <- TRUE
      shinyjs::runjs(paste0("$('#",elementId,"').empty()"))
      output[[elementId]] <- renderUI({
        shiny::validate(
          shiny::need(input[[dateFormat]] != "", 'Please select date format'),
          shiny::need(
            input[[dateFieldName]] != "",
            'Please select date field name.'
          ),
          shiny::need(
            !is.null(input[[parmToProcess]]),
            'Please select parameters to process.'
          ),
          shiny::need(
            input[[timeFieldName]] != "",
            'Please select time field name.'
          )
        )
      })
    }
    return(missingInputs)

  },error = function(err) {
    message("Error in download_daymet function")
    print(err)}
  )
  })
 
}##FUN.fun.GageData.END

