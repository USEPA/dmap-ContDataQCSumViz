
# shinyAlertModuleUI <- function(id, msg, type) {
#   ns <- NS(id)
#   shinyjs::useShinyjs()
#   shinyalert(inputId=ns("common_alert_msg"),type,msg,closeOnClickOutside = TRUE,closeOnEsc = TRUE,confirmButtonText="OK",
#              callbackR = "function(x) { shinyjs::alert('called')  shinyjs::runjs('swal.close();')}")
# 
# }
# 
# shinyAlertModuleServer<- function(id) {
#   moduleServer(
#     id,
#     function(input, output, session) {
#       ns <- session$ns
#       observeEvent(input[[ns("common_alert_msg")]],{
#         shinyjs::runjs("swal.close();")
#       })
#     }
#   )
# }

