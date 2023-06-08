#' Run Meta Summary Button (UI)
#' @description    This module get meta data on user uploaded files
#'
#' @param id
metaDataUI <- function(id) {
  ns <- NS(id)
  shinyjs::useShinyjs()
  tagList(
    uiOutput(outputId = ns("metaSummaryTb")),
    uiOutput(outputId = ns("meta_footnote_text")),
    fluidRow(
      column(width = 4, uiOutput(outputId = ns("fillMissingData"))),
      column(width = 4, uiOutput(outputId = ns("saveStats")), ),
      column(width = 4, uiOutput(outputId = ns("meta_exclude_checks")))
    )
  ) # end of tagList
}

#' Run Meta Summary Button (server side)
#'
#' @param id 
#' @param paramToProcess 
#' @param formatedUploadedData 
#' @param uploadData 
#'
metaDataServer <- function(id, paramToProcess, formatedUploadedData, uploadData) {
  toReport <- NULL

  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      fillMissingData2 <- shiny::reactive(input$fillMissingData2)
      exclude_flagged2 <- shiny::reactive(input$exclude_flagged2)
      how_to_save2 <- shiny::reactive(input$how_to_save2)

      observeEvent(uploadData, {
        # remvoe old values and calculations because new file is uploaded
        output$meta_footnote_text <- renderUI({})
        output$metaSummaryTb <- renderTable({})
        output$meta_exclude_checks <- renderUI({})
        output$fillMissingData <- renderUI({})
        output$saveStats <- renderUI({})
      })

      observeEvent(formatedUploadedData, {
        dailyCheck <- ReportMetaData(
          fun.myFile = NULL,
          fun.myDir.import = NULL,
          fun.myParam.Name = paramToProcess,
          fun.myDateTime.Name = "date.formatted",
          fun.myDateTime.Format = "%Y-%m-%d",
          fun.myThreshold = 20,
          fun.myConfig = "",
          df.input = formatedUploadedData
        )
        print("passed ReportMetaData")
        getQuickSummary <- lapply(dailyCheck, fun.getMetaSummary)
        toReport <- as.data.frame(matrix(nrow = length(dailyCheck), ncol = 5))
        colnames(toReport) <-
          c(
            "Parameters",
            "Number of days with missing data",
            "Number of days with data flagged as fail",
            "Number of days with data flagged as suspect",
            "Number of days with data flagged not known"
          )
        toReport$Parameters <- names(dailyCheck)
        for (n in 1:length(dailyCheck)) {
          toReport[n, 2:5] <- getQuickSummary[[n]]
        }
        output$metaSummaryTb <- renderTable(
          {
            toReport
          },
          type = "html",
          bordered = TRUE,
          striped = TRUE,
          align = "c",
          width = "100%"
        )

        date_column <- formatedUploadedData[, "date.formatted"]
        max_date <- max(as.POSIXct(date_column, format = "%Y-%m-%d"), na.rm = TRUE)
        min_date <- min(as.POSIXct(date_column, format = "%Y-%m-%d"), na.rm = TRUE)
        total_N_days <- as.integer(difftime(max_date, min_date, units = "days"))

        output$meta_footnote_text <- renderUI({
          div(
            class = "text-info fs-6",
            div(
              class = "panel panel-default", style = "padding:10px;",
              HTML(paste0("<b>Period of record:</b> ", min_date, "<b> to </b>", max_date)),
              br(),
              HTML(paste0("<b>Total number of days in this period: </b>", total_N_days, " days"))
            )
          )
        })
        output$fillMissingData <- renderUI({
          HTML(paste0("<span style='position:relative;top:-10px;'>", checkboxInput(ns("fillMissingData2"), label = "Fill missing data with 'NA' values", TRUE), "</span>"))
        })
        output$saveStats <- renderUI({
          radioButtons(
            ns("how_to_save2"),
            "How to save daily statistics",
            choices = c(
              "Per site Per parameter" = "save1",
              "Per site with all parameters" = "save2",
              # "Multiple sites together" = "save3",
              "Save for WQX upload" = "save4"
            ),
            selected = "save2",
            inline = FALSE
          )
        })

        check_no_flags <-
          all(toReport[, 3] == "No flag field found") &
            all(toReport[, 4] == "No flag field found")

        if (!check_no_flags) {
          output$meta_exclude_checks <- renderUI({
            checkboxGroupInput(
              ns("exclude_flagged2"),
              "Select data points to be excluded",
              choices = c(
                "fail" = "fail",
                "suspect" = "suspect",
                "flag not known" = "flag not known"
              ),
              selected = "fail"
            )
          }) # renderUI close
        } # if  close
      })

      return(
        list(
          fillMissingData2 = shiny::reactive(input$fillMissingData2),
          exclude_flagged2 = shiny::reactive(input$exclude_flagged2),
          how_to_save2 = shiny::reactive(input$how_to_save2)
        )
      )
    }
  )
}
