#' Continuous Data Exploration / All Parameter / Box Plots tab (user interface side)
#'
#' @param id 
#'
TsBoxPlotModuleUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(class="panel panel-default",style="margin:10px;",
          div(class="panel-heading"),
          div(class="panel-body",
              uiOutput(ns("box_input_1")),
              uiOutput(ns("box_input_2")),
              uiOutput(ns("box_input_3")),
              uiOutput(ns("box_input_4")),
              uiOutput(ns("box_input_5"))
          )#end of panel body
      )# end of panel
    ),
    mainPanel(
      width = 9,
      fluidRow(column(width = 12, 
                      div(style="width:100%", uiOutput(ns("boxPlotError"))),
                      plotlyOutput(ns("display_box_plots"))))
    ) # mainPanel end
  ) # sidebarLayout end
}

#' Continuous Data Exploration / All Parameter / Box Plots tab (server side)
#'
#' @param id 
#' @param dailyStats 
#' @param renderTSBoxPlot 
#'
TsBoxPlotModuleServer <- function(id, dailyStats, renderTSBoxPlot) {
 
  localStats <- reactiveValues(stats=list())
  variables_avail <- reactiveValues(params=list())
  
  moduleServer(
    id,
    function(input, output, session) {
          ns <- session$ns
          observe({
            localStats <- dailyStats
            variables_avail$params <- names(localStats$processed_dailyStats)
            localStats$stats <- localStats$processed_dailyStats
            #print(localStats$stats)
          })
          
          observe({
            if(renderTSBoxPlot$render == TRUE) {
                    output$box_input_1 <- renderUI({
                      selectizeInput(ns("boxplot_variable_name"),label ="Select variable name",
                                     choices=variables_avail$params,
                                     multiple = FALSE,
                                     selected=variables_avail$params[1],
                                     options = list(hideSelected = FALSE))
                    })
                    
                    output$box_input_2 <- renderUI({
                      
                      selectizeInput(ns("boxplot_metrics"),label ="Select daily statistics metrics",
                                     choices=c("mean","median","min", "max","range","sd","var","cv","n"),
                                     multiple = FALSE,
                                     selected="mean",
                                     options = list(hideSelected = FALSE))
                    })
                    
                    output$box_input_3 <- renderUI({
                      div(
                        radioButtons(ns("box_group"), "Group by", choices = c("month"="month"
                                                                          ,"month(years side by side)"="month2"
                                                                          ,"year"="year"
                                                                          ,"season"="season"
                                                                          ,"season(years side by side)"="season2"),
                                     selected = "month"))
                      
                    })
                    
                    output$box_input_4 <- renderUI({
                      textInput(inputId=ns("box_title"), label="Plot title",value="")
                    })
                    
                    
                    output$box_input_5 <- renderUI({
                      actionButton(inputId=ns("display_box"), label="Display",class="btn btn-primary")
                    })
            }
          })
          observeEvent(input$display_box, {
            localStats <- dailyStats
            clearContents()
            clearPlot()
            tryCatch({
                    myList <- localStats$processed_dailyStats
                    variable_to_plot <- input$boxplot_variable_name
                    myData <- myList[[which(names(myList)==variable_to_plot)]]
                    mean_col <- paste0(input$boxplot_variable_name,".",input$boxplot_metrics)
                    if(input$box_group=="year"){
                      #myData[,input$box_group] <- format(myData[,"Date"],"%Y")
                      myData[,input$box_group] <- format(as.Date(myData$Date, format="%Y-%m-%d %H:%M:%S"),"%Y")
                      cols_selected = c("Date",input$box_group,mean_col)
                    }else if(input$box_group=="month"){
                      #myData[,input$box_group] <- format(myData[,"Date"],"%m")
                      myData[,input$box_group] <- format(as.Date(myData$Date, format="%Y-%m-%d %H:%M:%S"),"%m")
                      cols_selected = c("Date",input$box_group,mean_col)
                    }else if(input$box_group=="season"){
                      myData <- addSeason(myData)
                      cols_selected = c("Date",input$box_group,mean_col)
                    }else if(input$box_group=="month2"){
                      #myData[,"year"] <- format(myData[,"Date"],"%Y")
                      myData[,"year"] <- format(as.Date(myData$Date, format="%Y-%m-%d %H:%M:%S"),"%Y")
                      #myData[,"month"] <- format(myData[,"Date"],"%m")
                      myData[,"month"] <- format(as.Date(myData$Date, format="%Y-%m-%d %H:%M:%S"),"%m")
                      cols_selected = c("Date","year","month",mean_col)
                    }else if(input$box_group=="season2"){
                      myData <- addSeason(myData)
                      cols_selected = c("Date","year","season",mean_col)
                    }
                    
                    data_to_plot <- myData[cols_selected]
                    if (!all(is.na(data_to_plot[,mean_col]))&input$box_group!="month2"&input$box_group!="season2"){
                      output$display_box_plots <- renderPlotly({
                        
                        p2 <- ggplot(data=data_to_plot,aes(x=!!sym(isolate(input$box_group)),y=!!sym(isolate(mean_col)))) +
                          geom_boxplot()+
                          labs(title=isolate(input$box_title),x = isolate(input$box_group),y = isolate(input$boxplot_variable_name))+
                          theme_bw()+
                          theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                                ,plot.title = element_text(hjust=0.5)
                                ,axis.text.x = element_text(angle=0, hjust=1))
                        p2 <- ggplotly(p2)
                        print(p2)
                      })
                    } else if(!all(is.na(data_to_plot[,mean_col]))&input$box_group=="month2"){
                      output$display_box_plots <- renderPlotly({
                        
                        p2 <- ggplot(data=data_to_plot,aes(x=month,y=!!sym(isolate(mean_col)),fill=year)) +
                          geom_boxplot(position=position_dodge(width=0.1))+
                          labs(title=isolate(input$box_title),x = "month",y = isolate(input$boxplot_variable_name))+
                          theme_bw()+
                          theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                                ,plot.title = element_text(hjust=0.5)
                                ,axis.text.x = element_text(angle=0, hjust=1))
                        ggplotly(p2) %>% plotly::layout(boxmode="group")
                      })
                    } else if(!all(is.na(data_to_plot[,mean_col]))&input$box_group=="season2"){
                      output$display_box_plots <- renderPlotly({
                        data_to_plot$season = reorderSeason(data_to_plot$season)
                        p2 <- ggplot(data=data_to_plot,aes(x=season,y=!!sym(isolate(mean_col)),fill=year)) +
                          geom_boxplot(position=position_dodge(width=0.1))+
                          labs(title=isolate(input$box_title),x = "season",y = isolate(input$boxplot_variable_name))+
                          theme_bw()+
                          theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                                ,plot.title = element_text(hjust=0.5)
                                ,axis.text.x = element_text(angle=0, hjust=1))
                        ggplotly(p2) %>% plotly::layout(boxmode="group")
                      })
                    }else{
                      renderErrorMsg(noBoxPlotDataFound)
                      clearPlot()
                    }
            }, error = function(err) {
              renderErrorMsg(paste("Process failed due to invalid data, error: ", err$message))
              clearPlot()
            })

          })  #observeEvent end
          
          #common
          renderErrorMsg <- function(msg) {
            output$boxPlotError <- renderUI({
              div(class="alert alert-danger" , msg) 
            })
          }
          clearContents <- function(){
            output$boxPlotError <- renderUI({})
          }
          
          clearPlot <- function(){
            output$display_box_plots <- renderPlotly({
              plotly_empty()
            })
          }


    })# end of server module
}
