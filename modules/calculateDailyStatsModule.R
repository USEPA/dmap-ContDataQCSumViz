calculateDailyStatsModuleUI <- function(id, readyForCalculation) {
  ns <- NS(id)
  shinyjs::useShinyjs()
  if (readyForCalculation$status == TRUE) {
    tagList(
      hr(),
      actionButton(
        inputId = ns("calculateDailyStatistics2"),
        label = "Step 4: Calculate daily statistics",
        class = "btn btn-primary"
      ),
      hr(),
      downloadButton(
        outputId = ns("saveDailyStatistics2"),
        label = "Save daily statistics",
        class = "btn btn-primary",
        style = "padding-left:15px;padding-right:15px;display:none;"
      )
    )
  }
}

calculateDailyStatsModuleServer <- function(id, formated_raw_data, homeDTvalues, metaHomeValues, loaded_data, dailyStatusCalculated, processed, readyForCalculation) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      dailyStats <- reactiveValues(processed = list())
      taskState <- reactiveValues(success = FALSE)

      observeEvent(readyForCalculation$status, {
        if (readyForCalculation$status == TRUE) {
          shinyjs::show(id = "calculateDailyStatistics2")
          if (dailyStatusCalculated$status == "finished") {
            shinyjs::show(id = "saveDailyStatistics2")
          }
        } else {
          shinyjs::hide(id = "calculateDailyStatistics2")
          shinyjs::hide(id = "saveDailyStatistics2")
        }
      })


      observeEvent(input$calculateDailyStatistics2, {
        tryCatch(
          {
            withProgress(message = paste("Calculating the daily statistics"), value = 0, {
              incProgress(0, detail = "now... ")
              raw_data <- formated_raw_data$derivedDF
              # print(head(raw_data))
              ## print(formated_raw_data$derivedDF)
              # dateRange$min <- min(as.Date(raw_data$date.formatted), na.rm = TRUE)
              # dateRange$max <- max(as.Date(raw_data$date.formatted), na.rm = TRUE)
              variables_to_calculate <- homeDTvalues$homeDateAndTime$parmToProcess()

              if (is.null(metaHomeValues$metaVal$fillMissingData2())) {
                ContData.env$myStats.Fails.Exclude <- FALSE
                ContData.env$myStats.Suspects.Exclude <- FALSE
              }
              if ("fail" %in% metaHomeValues$metaVal$fillMissingData2()) {
                print(paste0("check the exclude flagged choices are:", metaHomeValues$metaVal$fillMissingData2()))
                ContData.env$myStats.Fails.Exclude <- TRUE
              } else {
                ContData.env$myStats.Fails.Exclude <- FALSE
              }
              if ("suspect" %in% metaHomeValues$metaVal$fillMissingData2()) {
                print(paste0("check the exclude flagged choices are:", metaHomeValues$metaVal$fillMissingData2()))
                ContData.env$myStats.Suspects.Exclude <- TRUE
              } else {
                ContData.env$myStats.Suspects.Exclude <- FALSE
              }
              if ("flag not known" %in% metaHomeValues$metaVal$fillMissingData2()) {
                # do nothing, logic is not there in the sumStts.updated function
                # print("Just for testing")
              }


              # Fill missing data
              ContData.env$myStats.missing.data.fill <- metaHomeValues$metaVal$fillMissingData2()

              dailyStats$processed <- SumStats.updated(
                fun.myFile = NULL,
                fun.myDir.import = NULL,
                fun.myParam.Name = variables_to_calculate,
                fun.myDateTime.Name = "date.formatted",
                fun.myDateTime.Format = "%Y-%m-%d %H:%M:%S",
                fun.myThreshold = 20,
                fun.myConfig = "",
                df.input = raw_data
              )

              shinyjs::show(id = "saveDailyStatistics2")
              incProgress(1 / 1, detail = "Calculated the daily statistics")
              processed$processed_dailyStats <- dailyStats$processed
              dailyStatusCalculated$status <- "finished"
            })
          },
          error = function(parsingMsg) {
            #print(parsingMsg$message)
            dailyStatusCalculated$status <- "error"
          }
        )
      })

      output$saveDailyStatistics2 <- downloadHandler(
        filename = function() {
          name_in_file <- loaded_data$name
          if (endsWith(loaded_data$name, ".csv")) name_in_file <- sub(".csv$", "", loaded_data$name)
          if (endsWith(loaded_data$name, ".xlsx")) name_in_file <- sub(".xlsx$", "", loaded_data$name)

          if (metaHomeValues$metaVal$how_to_save2() == "save2") {
            paste0("saved_dailyStats_", name_in_file, "_dailyStats.csv")
          } else if (metaHomeValues$metaVal$how_to_save2() == "save1") {
            paste0("saved_dailyStats_", name_in_file, ".zip")
          } else if (metaHomeValues$metaVal$how_to_save2() == "save4") {
            paste0("saved_dailyStats_wqx_", name_in_file, ".csv")
          }
        },
        content = function(file) {
          if (metaHomeValues$metaVal$how_to_save2() == "save2") {
            combined_data <- Reduce(full_join, dailyStats$processed)
            write.csv(combined_data, file, row.names = FALSE)
          } else if (metaHomeValues$metaVal$how_to_save2() == "save4") {
            wqxData <- Reduce(full_join, dailyStats$processed)
            wqxData <- wqxData %>%
              gather(key = "CharacteristicName", value = "Value", -Date)
            write.csv(wqxData, file, row.names = FALSE)
          } else if (metaHomeValues$metaVal$how_to_save2() == "save1") {
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            files <- NULL
            for (i in 1:length(dailyStats$processed)) {
              name_i <- names(dailyStats$processed)[i]
              print(name_i)
              filename <- paste0("saved_dailyStats_", loaded_data$name, "_", name_i, "_dailyStats.csv")
              write.csv(dailyStats$processed[[i]], filename, row.names = FALSE)
              files <- c(filename, files)
            }
            zip::zip(file, files)
          }
        }
      )
    }
  )
}
