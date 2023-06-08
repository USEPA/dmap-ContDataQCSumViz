#' User guide - step workflow
#'
#' @param id 
#'
progressWorkflowModuleUI <- function(id) {
  ns <- NS(id)
  shinyjs::useShinyjs()
  fluidRow(
    column(
      width = 1,
      div(class="rectangle",
          span(circleButton(inputId=ns("step1"),icon = icon("upload"),status="primary", size="xs")),
          span("Step 1: Upload file", style="font-weight:bold; word-wrap: break-word;margin-right:30px;")
      )
    ),
    column(
      width = 1),
    column(
      width = 1,
      div(class="rectangle",
          span(circleButton(inputId = ns("step2"),icon = icon("calendar"),status = "primary", size="xs")),
          span("Step 2: Select date and time",style="font-weight:bold; word-wrap: break-word;margin-right:30px;")
      )
    ),# end of column
    column(width=1),
    column(
      width = 1,
      div(class="rectangle",
          span(circleButton(inputId = ns("step3"),icon = icon("tasks"),status = "primary", size="xs")),
          span("Step 3: Run meta summary",style="font-weight:bold; word-wrap: break-word;margin-right:30px;")
      )
    ),# end of column
    column(width=1),
    column(
      width = 1,
      div(class="rectangle", style="width:280px;",
          span(circleButton(inputId = ns("step4"),icon = icon("calculator"),status = "primary",size="xs")),
          span("Step 4: Calculate daily statistics",style="font-weight:bold; word-wrap: break-word;margin-right:30px;")
      )
    ),# end of column
    column(width=1),
    column(
      width = 1,
      div(class="rectangle",style="margin-left:30px;",
          span(circleButton(inputId = ns("step5"),icon = icon("check"),status = "primary", size="xs")),
          span("Step 5: Visualize data",style="font-weight:bold; word-wrap: break-word;")
      )
    )# end of column
  )



}

#' User guide - step workflow (server side)
#'
#' @param id 
#' @param workflowStatus 
#'
progressWorkflowModuleServer <- function(id, workflowStatus) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      observe({

        if(workflowStatus$elementId == "step1" & workflowStatus$state == "success" & workflowStatus$finish == FALSE) {
            #print("int the step1 success")
            removeCssClasses()
            addSuccessClass(c("step1"))
            addPrimaryClass(c("step2","step3","step4","step5"))
            changeButtonState(state="disable", btnList=c("runQS","display_raw_ts"))
            changeButtonState(state="enable", btnList=c("display_raw_ts"))
        } else if(workflowStatus$elementId == "step1" & workflowStatus$state == "error") {
            #print("int the step1 error")
            removeCssClasses()
            shinyjs::addClass("step1", "btn-danger")
            addPrimaryClass(c("step2","step3","step4","step5"))
            changeButtonState(state="disable", btnList=c("display_raw_ts","runQS"))
            workflowStatus$finish=FALSE
        }
        else if(workflowStatus$elementId == "step2" & workflowStatus$state == "success" & workflowStatus$finish == FALSE) {
          #print("in the step2")
          removeCssClasses()
          addPrimaryClass(c("step3","step4","step5"))
          addSuccessClass(c("step1","step2"))
          changeButtonState(state="enable", btnList=c("display_raw_ts","runQS"))
        } else if(workflowStatus$elementId == "step2" & workflowStatus$state == "error") {
          removeCssClasses()
          shinyjs::addClass("step2", "btn-danger")
          addPrimaryClass(c("step3","step4","step5"))
          addSuccessClass(c("step1"))
          changeButtonState(state="enable", btnList=c("runQS","display_raw_ts"))
          workflowStatus$finish=FALSE
        }

        else if(workflowStatus$elementId == "step3" & workflowStatus$state == "success" & workflowStatus$finish == FALSE) {
          removeCssClasses()
          addSuccessClass(c("step1","step2","step3"))
          addPrimaryClass(c("step4","step5"))
          changeButtonState(state="enable", btnList=c("display_raw_ts","runQS"))
          js$enableTab("downloadData")
          js$enableTab("discreateDataEx")
        } else if(workflowStatus$elementId == "step3" & workflowStatus$state == "error") {
          removeCssClasses()
          shinyjs::addClass("step3", "btn-danger")
          addPrimaryClass(c("step4","step5"))
          addSuccessClass(c("step1","step2"))
          changeButtonState(state="enable", btnList=c("runQS","display_raw_ts"))
          js$disableTab("downloadData")
          js$disableTab("discreateDataEx")
          workflowStatus$finish=FALSE
        }

        else if(workflowStatus$elementId == "step4" & workflowStatus$state == "success" & workflowStatus$finish == FALSE) {
          removeCssClasses()
          addSuccessClass(c("step1","step2","step3","step4"))
          changeButtonState(state="enable", btnList=c("display_raw_ts","runQS"))
          addPrimaryClass(c("step5"))
        } else if(workflowStatus$elementId == "step4" & workflowStatus$state == "error") {
          removeCssClasses()
          shinyjs::addClass("step4", "btn-danger")
          addSuccessClass(c("step1","step2","step3"))
          addPrimaryClass(c("step5"))
          changeButtonState(state="enable", btnList=c("display_raw_ts","runQS"))
          workflowStatus$finish=FALSE
        }

        else if(workflowStatus$elementId == "step5" & workflowStatus$state == "success" & workflowStatus$finish == FALSE) {
          removeCssClasses()
          addSuccessClass(c("step1","step2","step3","step4","step5"))
          js$enableTab("DataExploration")
          # js$enableTab("CreateReport")
          changeButtonState(state="enable", btnList=c("display_raw_ts","runQS"))
          workflowStatus$finish = TRUE
        } else if(workflowStatus$elementId == "step4" & workflowStatus$state == "error") {
          removeCssClasses()
          shinyjs::addClass("step5", "btn-danger")
          addSuccessClass(c("step1","step2","step3","step4"))
          changeButtonState(state="enable", btnList=c("display_raw_ts","runQS"))
          workflowStatus$finish=FALSE
        }
        if(workflowStatus$elementId %in% c("step1","step2","step3","step4") & workflowStatus$finish == FALSE) {
          js$disableTab("DataExploration")
        }
        if(workflowStatus$elementId %in% c("step1","step2") & workflowStatus$finish == FALSE) {
          js$disableTab("downloadData")
          js$disableTab("discreateDataEx")
        }

      })

      changeButtonState <- function(state, btnList) {
        if(state == "enable") {
          for(b in btnList) {
            #enable(b)
            shinyjs::show(b, asis = TRUE)
          }
        } else if(state =="disable"){
          for(b in btnList) {
            shinyjs::hide(b,asis = TRUE)
            #disable(b)
          }
        }
      }

      removeCssClasses <- function(){
        cssClasses <- c("btn-success","btn-primary","btn-danger")
        elementIds <- c("step1","step2","step3", "step4","step5")
        for(e in elementIds) {
          for (c in cssClasses) {
            shinyjs::removeClass(e, c)      }
        }
      }
      addPrimaryClass <- function(elmentIds){
        for(e in elmentIds) {
          shinyjs::addClass(e, "btn-primary")
        }
      }
      addSuccessClass <- function(elmentIds) {
        for(e in elmentIds) {
          shinyjs::addClass(e, "btn-success")
        }
      }
    })
}
