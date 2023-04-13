# #currently could not use this as lot of variables are
# #common and there is no time to handle that many changes at once
# #saving for the future use
# 
# dateAndTimeUI <- function(id, label = "Counter") {
#   ns <- NS(id)
#   tagList(
#     actionButton(ns("openModalBtn"), label = label),
#   )
# }
# 
# dateAndTimeServer <- function(id, parmsToProcess, uploadedCols, modalTitle="") {
#   moduleServer(
#     id,
#     function(input, output, session) {
#       ns <- session$ns
#       missingInputs <- FALSE
#       myModal <- function() {
#         draggableModalDialog(
#           size="l",
#           title=modalTitle,
#           fade = TRUE,
#           fluidRow(
#             column(width = 12,
#                radioButtons(ns("dtNumOfCols"), label="", choices = c("Data and time uploaded in one column"="dtCombined",
#                                                            "Data and time uploaded in two separate columns"="dtSeparated"), inline = TRUE,
#                selected = "dtCombined")
# 
#             )
#           ),
#           fluidRow(
#             column(width=12,uiOutput(ns("display_validation_msgs")))
#           ),
#           fluidRow(
#             column(width=4, selectizeInput(ns("parameters_to_process"),label ="Select parameters to process",
#                                            choices=parmsToProcess,
#                                            multiple = TRUE,
#                                            options = list(hideSelected = FALSE,plugins=list('remove_button'))
#             )),
#             column(width=4,selectInput(ns("selectedDateFieldName"),label="Date Field Name",choices=c("",uploadedCols))),
#             shinyjs::hidden(tags$div(id="timeFieldDiv",column(width=4,selectInput(ns("selectedTimeFieldName"),label="Time Field Name",choices=
#                                                                                     c("",uploadedCols))))),
#           ),
#           fluidRow(
#             column(width=4,
#             selectizeInput(
#               ns("selectedDateFormat"),
#               label = "Date Format",
#               choices = c(c('Year, Month, Day'),c('Year, Day, Month'),c('Month, Day, Year'),
#                           c('Day, Month, Year'),c('Year, Month, Day'),
#                           c('Abbreviated month, Day of the month, Year')),
#               multiple = FALSE,
#               options = list(
#                 hideSelected = FALSE,
#                 plugins = list('remove_button')
#               )
#             )),
#             column(width=4,
#             selectizeInput(
#               ns("selectedTimeFormat"),
#               label = "Time Format",
#               choices = c('Hour, Minute, Second','Hour, Minute','Hour in 12-hour format, Minute, AM/PM','Hour in 12-hour format, Minute, Second, AM/PM', 'None'),
#               multiple = FALSE,
#               options = list(
#                 hideSelected = FALSE,
#                 plugins = list('remove_button')
#               )
#             )),
#             column(width = 4,
#                selectizeInput(
#                  ns("selectedTimeZone"),
#                  label = "Time Zone",
#                  choices = c('UTC','HST', 'AKST', 'PST', 'MST', 'CST', 'EST','EDT'),
#                  multiple = FALSE,
#                  options = list(
#                    hideSelected = FALSE,
#                    plugins = list('remove_button')
#                  )
#                )
#             )
#           ),
#           footer = tagList(
#             actionButton(ns("Done"), label="Run meta summary",class="action-btn-style"),
#             modalButton("Cancel"),
#           )
#         )
#       }
#       observeEvent(input$openModalBtn, ignoreNULL = FALSE, showModal(myModal()))
# 
#       observeEvent(input$dtNumOfCols, {
#         if(isolate(input$dtNumOfCols) == "dtCombined")
#             shinyjs::hide("timeFieldDiv", asis = TRUE)
#         else if(isolate(input$dtNumOfCols) == "dtSeparated")
#             shinyjs::show("timeFieldDiv", asis = TRUE)
#       })
# 
#       observeEvent(input$dtNumOfCols, {
#         if(isolate(input$dtNumOfCols) == "dtCombined")
#           shinyjs::hide("timeFieldDiv", asis = TRUE)
#         else if(isolate(input$dtNumOfCols) == "dtSeparated")
#           shinyjs::show("timeFieldDiv", asis = TRUE)
#       })
# 
#       return(
#         list(
#           user_parameters_to_process = shiny::reactive(input$parameters_to_process)
#         )
#       )
# 
#     }
#   )
# }
