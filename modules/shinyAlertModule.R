
shinyAlertModuleUI <- function(id, msg, type) {
  ns <- NS(id)
  shinyjs::useShinyjs()
  print("came to UI module")
  shinyalert(inputId=id,type,msg,closeOnClickOutside = TRUE,closeOnEsc = TRUE,confirmButtonText="OK")
}

shinyAlertModuleServer<- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      print("came to serverf")
      ns <- session$ns
      observeEvent(input[[id]],{
        shinyjs::alert("watching")
        shinyjs::runjs("swal.close();")
      })
    }
  )
}
