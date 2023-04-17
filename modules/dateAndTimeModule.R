dateAndTimeUI <- function(id, paramChoices, uploadedCols) {
  ns <- NS(id)
  shinyjs::useShinyjs()
  tagList(
    fluidRow(
      column(width = 12,
             radioButtons(ns("dateColumnNumsId"), label="", choices = c("Date and time uploaded in one column"="combined",
                                                                  "Date and time uploaded in two separate columns"="separate"), inline = TRUE,
                          selected = "combined")
             
      )
    ),
    # fluidRow(
    #   column(width=12,uiOutput(ns("validationDivId"))),
    #   column(width=12,uiOutput(ns("extraValidationId"))),
    # ),
    fluidRow(
      column(width=4, selectizeInput(ns("parmToProcessId"),label ="Select parameters to process",
                                     choices=paramChoices,
                                     multiple = TRUE,
                                     options = list(hideSelected = FALSE,plugins=list('remove_button'))
      )),
      column(width=4,selectInput(ns("dateFieldNameId"),label="Date Field Name",choices=c("",uploadedCols))),
      tags$div(id=ns("timeFieldParentId"), style="display:none", column(width=4,selectInput(ns("timeFieldNameId"),label="Time Field Name",choices=c("",uploadedCols)))),
    ),
    fluidRow(
      column(width=4,
             selectizeInput(
               ns("dateFormatId"),
               label = "Date Format",
               choices = c(c('Year, Month, Day'),c('Year, Day, Month'),c('Month, Day, Year'),
                           c('Day, Month, Year'),c('Year, Month, Day'),
                           c('Abbreviated month, Day of the month, Year')),
               multiple = FALSE,
               options = list(
                 hideSelected = FALSE,
                 plugins = list('remove_button')
               )
             )),
      column(width=4,selectizeInput(
        ns("timeFormatId"),
        label = "Time Format",
        choices = c('Hour, Minute, Second','Hour, Minute','Hour in 12-hour format, Minute, AM/PM','Hour in 12-hour format, Minute, Second, AM/PM', 'None'),
        multiple = FALSE,
        options = list(
          hideSelected = FALSE,
          plugins = list('remove_button')
        )
      )),
      column(width=4, selectizeInput(
        ns("timeZoneId"),
        label = "Time Zone",
        choices = c('UTC','HST', 'AKST', 'PST', 'MST', 'CST', 'EST','EDT'),
        multiple = FALSE,
        options = list(
          hideSelected = FALSE,
          plugins = list('remove_button')
        )
      )),
    )
  )
  
}

dateAndTimeServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      time_iv <- InputValidator$new()
      time_iv$condition(~ input$dateColumnNumsId == 'separate')
      time_iv$add_rule("timeFieldNameId", sv_required(message=""))
      time_iv$enable()
      
      iv <- InputValidator$new()
      iv$add_rule("dateColumnNumsId", sv_required())
      iv$add_rule("parmToProcessId", sv_required(message=""))
      iv$add_rule("dateFieldNameId", sv_required(message=""))
      iv$add_rule("dateFormatId", sv_required(message=""))
      iv$enable()
      
      observeEvent(input$dateColumnNumsId, ignoreInit = TRUE, {
        if(input$dateColumnNumsId == "separate") {
          shinyjs::show(ns("timeFieldParentId"), asis = TRUE)
        } else if(input$dateColumnNumsId == "combined"){
          shinyjs::hide(ns("timeFieldParentId"), asis = TRUE)
        }
      })
      
      return(
        list(
          dateColumnNums=shiny::reactive(input$dateColumnNumsId),
          parmToProcess=shiny::reactive(input$parmToProcessId),
          dateFieldName=shiny::reactive(input$dateFieldNameId),
          dateFormat=shiny::reactive(input$dateFormatId),
          timeFieldName=shiny::reactive(input$timeFieldNameId),
          timeFormat=shiny::reactive(input$timeFormatId),
          timeZone=shiny::reactive(input$timeZoneId),
          isTimeValid=shiny::reactive(time_iv$is_valid()),
          isDateAndtimeValid=shiny::reactive(iv$is_valid())
        )
      )
    })
}
