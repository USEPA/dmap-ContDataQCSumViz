#' Continuous Data Exploration / All Parameter / Time Series - Annual Overlays (user interface side)
#'
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
TsOverlayModuleUI <- function(id) {
  ns <- NS(id)
  shinyjs::useShinyjs()
  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(class="panel panel-default",style="margin:10px;",
          div(class="panel-heading"),
          div(class="panel-body",
              uiOutput(ns("time_series_overlay_input_1")),
              uiOutput(ns("time_series_overlay_input_2")),
              uiOutput(ns("time_series_overlay_input_3")),
              uiOutput(ns("time_series_overlay_input_4")),
              # shinyjs::hidden(
              #   div(
              #commented out as asked by Tom Faber
              #     id = "cp_new_data_overlay",
              #     conditionalPanel(
              #       condition = "input$overlay_shading == 'newData' ",
              #       fileInput("uploaded_overlay_newData_file",
              #                 label = "Upload your new data", multiple = FALSE,
              #                 accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
              #       ),
              #       selectizeInput("overlay_newData_lower_col",
              #                      label = "Select column to be used as lower bound",
              #                      choices = NULL,
              #                      multiple = FALSE,
              #                      selected = NULL,
              #                      options = list(hideSelected = FALSE)
              #       ),
              #       selectizeInput("overlay_newData_longterm_col",
              #                      label = "Select column to be used as long-term reference line",
              #                      choices = NULL,
              #                      multiple = FALSE,
              #                      selected = NULL,
              #                      options = list(hideSelected = FALSE)
              #       ),
              #       selectizeInput("overlay_newData_upper_col",
              #                      label = "Select column to be used as upper bound",
              #                      choices = NULL,
              #                      multiple = FALSE,
              #                      selected = NULL,
              #                      options = list(hideSelected = FALSE)
              #       ),
              #       selectizeInput("overlay_newData_date_col",
              #                      label = "Select month-day column",
              #                      choices = NULL,
              #                      multiple = FALSE,
              #                      selected = NULL,
              #                      options = list(hideSelected = FALSE)
              #       ),
              #       textInput("overlay_newData_name",
              #                 label = "New data name",
              #                 value = "USGS"
              #       )
              #     ), # conditionalPanel end
              #   ) # div end
              # ), # shinyjs:: hidden end
              hr(),
              uiOutput(ns("time_series_overlay_input_5")),
          ) # end of panel body
      ) # end of panel
    ),
    mainPanel(
      width = 9,

      fluidRow(column(width = 12, 
                      div(style="width:100%", uiOutput(ns("tsOverlayError"))),
                      div(plotlyOutput(ns("display_time_series_overlay"))),
                      div(plotOutput(ns("display_time_series_overlay_1")))
                      
               )#end of columns
     ) # end of fluidRow
    ) # mainPanel end
  ) # sidebarLayout end

}

#' Continuous Data Exploration / All Parameter / Time Series - Annual Overlays (server side)
#'
#' @param id 
#' @param dailyStats 
#' @param renderTSOverlay 
#'
TsOverlayModuleServer <- function(id, dailyStats, renderTSOverlay) {
 
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
            if(renderTSOverlay$render == TRUE) {
                  output$time_series_overlay_input_1 <- renderUI({
                    selectizeInput(ns("dailyStats_ts_overlay_variable_name"),label ="Select variable name",
                                   choices=variables_avail$params,
                                   multiple = FALSE,
                                   selected=variables_avail$params[1],
                                   options = list(hideSelected = FALSE))
                  })
                  output$time_series_overlay_input_2 <- renderUI({
                    
                    selectizeInput(ns("dailyStats_ts_overlay_metrics"),label ="Select daily statistics metrics",
                                   choices=c("mean","median","min", "max","range","sd","var","cv","n"),
                                   multiple = FALSE,
                                   selected="mean",
                                   options = list(hideSelected = FALSE))
                  })
                  
                  output$time_series_overlay_input_3 <- renderUI({
                    textInput(inputId=ns("dailyStats_ts_overlay_title"), label="Plot title",value="")
                  })
                  
                  output$time_series_overlay_input_4 <- renderUI({
                    radioButtons(ns("overlay_shading"), "Add shading with", choices = c("none"="none"
                                                                                    ,"overall minimum and maximum(all years)"="overall"
                    ),
                    selected = "none")
                  })
                  output$time_series_overlay_input_5 <- renderUI({
                    actionButton(inputId=ns("display_ts_overlay"), label="Display",class="btn btn-primary")
                  })
            }
          
          })
          
          
          observeEvent(input$display_ts_overlay, {
            
            #clean previous output, if any
            output$display_time_series_overlay <- renderPlotly({})
            output$display_time_series_overlay_1 <- renderPlot({})
            clearContents()

            localStats <- dailyStats
            variables_avail$params <- names(localStats$processed_dailyStats)
            myList <- localStats$processed_dailyStats
            variable_to_plot <- input$dailyStats_ts_overlay_variable_name
            myData <- myList[[which(names(myList)==variable_to_plot)]]
            mean_col <- paste0(input$dailyStats_ts_overlay_variable_name,".",input$dailyStats_ts_overlay_metrics)
            cols_selected <- c("Date",mean_col)
            data_to_plot <- myData[cols_selected]

            data_to_plot[,"year"] <- format(as.Date(data_to_plot$Date, format="%Y-%m-%d %H:%M:%S"),"%Y")
            

            if ((input$dailyStats_ts_overlay_metrics=="mean"|input$dailyStats_ts_overlay_metrics=="median")&input$overlay_shading=="overall"){
              
               ## calculate overall(include all years) monthly minimum and maximum values
              min_col <- paste0(input$dailyStats_ts_overlay_variable_name,".min")
              max_col <- paste0(input$dailyStats_ts_overlay_variable_name,".max")
              data_for_overlay <- myData[c("Date",min_col,max_col)]
              #data_for_overlay[,"MonthDay"] <- format(data_for_overlay[,"Date"],"%m-%d")
              data_for_overlay[,"MonthDay"] <- format(as.Date(data_for_overlay$Date, format="%Y-%m-%d %H:%M:%S"),"%m-%d")


              monthDay_min <- aggregate(data_for_overlay[,2],list(data_for_overlay$MonthDay),FUN=mean)
              monthDay_max <- aggregate(data_for_overlay[,3],list(data_for_overlay$MonthDay),FUN=mean)
              merged_overlay <- merge(monthDay_min,monthDay_max,by="Group.1")
              colnames(merged_overlay) <- c("MonthDay","min","max")
              #save(merged_overlay,file="test_overall_min_max_overlay.RData")
              

              output$display_time_series_overlay <- renderPlotly({  
                isolate({
                  p1 <- ggplot(data_to_plot)+
                    geom_line(aes(x=as.Date(yday(Date),"2000-01-01"),y=isolate(!!sym(mean_col)),colour=year),size=0.8)+
                    geom_ribbon(data=merged_overlay,aes(x=as.Date(yday(paste0("2000","-",MonthDay)),"2000-01-01"),
                                                        ymin=min,ymax=max,
                                                        fill="overall minimum and maximum" ),alpha=0.5)+
                    scale_x_date(date_breaks="1 month",limits=c(as.Date("2000-01-01"),as.Date("2000-12-31")),date_labels = "%m%d")+
                    scale_fill_manual(" ",labels="overall minimum and maximum",values=c("grey80"="grey80"))+
                    labs(title=isolate(input$dailyStats_ts_overlay_title),x = "MonthDay",y =mean_col)+
                    theme_classic()+
                    theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                          ,panel.border = element_rect(colour="black",fill=NA, size=0.5)
                          ,plot.title = element_text(hjust=0.5)
                          ,plot.background = element_rect(color="grey20",size=2)
                          ,legend.position = "right"
                          ,axis.text.x=element_text(angle=45, hjust=1))
                  ggplotly(p1,dynamicTicks = FALSE)
                  print(p1)
                })
              }) # renderPlot close

              output$display_time_series_overlay_1 <- renderPlot({
                isolate({
                  p1 <- ggplot(data_to_plot)+
                    geom_line(aes(x=as.Date(yday(Date),"2000-01-01"),y=isolate(!!sym(mean_col)),colour=year),size=0.8)+
                    geom_ribbon(data=merged_overlay,aes(x=as.Date(yday(paste0("2000","-",MonthDay)),"2000-01-01"),
                                                        ymin=min,ymax=max,
                                                        fill="overall minimum and maximum" ),alpha=0.5)+
                    scale_x_date(date_breaks="1 month",limits=c(as.Date("2000-01-01"),as.Date("2000-12-31")),date_labels = "%m%d")+
                    scale_fill_manual(" ",labels="overall minimum and maximum",values=c("grey80"="grey80"))+
                    labs(title=isolate(input$dailyStats_ts_overlay_title),x = "MonthDay",y =mean_col)+
                    theme_classic()+
                    theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                          ,panel.border = element_rect(colour="black",fill=NA, size=0.5)
                          ,plot.title = element_text(hjust=0.5)
                          ,plot.background = element_rect(color="grey20",size=2)
                          ,legend.position = "right"
                          ,axis.text.x=element_text(angle=45, hjust=1))
                  #ggplotly(p1,dynamicTicks = FALSE)
                  print(p1)
                })
              }) # renderPlot close
            }else{
              if (!all(is.na(data_to_plot[,mean_col]))){
                        output$display_time_series_overlay <- renderPlotly({
                          p1 <- ggplot(data_to_plot,aes(x=as.Date(yday(Date),"2000-01-01"),y=!!sym(mean_col)))+
                            geom_line(aes(colour=year),size=0.8)+
                            scale_x_date(date_breaks="1 month",limits=c(as.Date("2000-01-01"),as.Date("2000-12-31")),date_labels = "%m%d")+
                            labs(title=isolate(input$dailyStats_ts_overlay_title),x = "MonthDay",y =mean_col)+
                            theme_classic()+
                            theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                                  ,panel.border = element_rect(colour="black",fill=NA, size=0.5)
                                  ,plot.title = element_text(hjust=0.5)
                                  ,plot.background = element_rect(color="grey20",size=2)
                                  ,legend.position = "right"
                                  ,axis.text.x=element_text(angle=45, hjust=1))
                            ggplotly(p1,dynamicTicks = FALSE) %>% plotly::layout()
                        })  # renderPlot close
      
                    }else{
                      renderErrorMsg(noAnnualOverlayData)
                      clearPlot()

                    }##inner if else loop close
            } ## outer if else loop close
          }) ##observeEvent end
          
          
          #common
          renderErrorMsg <- function(msg) {
            output$tsOverlayError <- renderUI({
              div(class="alert alert-danger" , msg) 
            })
          }
          clearContents <- function(){
            output$tsOverlayError <- renderUI({})
          }
          
          clearPlot <- function(){
            output$display_time_series_overlay <- renderPlotly({
              plotly_empty()
            })
            output$display_time_series_overlay_1 <- renderPlotly({
              plotly_empty()
            })
          }
        
    }) # end of module server
  

  
  #Tom Faber asked to comment out the new data feature on this tab
  # Moving the code here. It will need to be modified to work as shiny module. I am just storing it here.
  
  # uploaded_overlay_newData <- eventReactive(c(input$uploaded_overlay_newData_file),{
  #   my_data <- uploadFile(c(input$uploaded_overlay_newData_file), stopExecution = FALSE)
  #   return(my_data)
  # })
  # 
  
  
  # observeEvent(input$overlay_shading,{
  #   if (input$overlay_shading == 'newData'){
  #     shinyjs::show("cp_new_data_overlay")
  #   }else{
  #     shinyjs::hide("cp_new_data_overlay")
  #   }
  # })
  

  
  # observeEvent(input$uploaded_overlay_newData_file,{
  #   cols_avail <- colnames(uploaded_overlay_newData())
  #   #print(cols_avail)
  #   updateSelectizeInput(session,"overlay_newData_lower_col",label ="Select column to be used as lower bound",
  #                        choices=cols_avail,
  #                        selected=''
  #   )
  #   
  #   updateSelectizeInput(session,"overlay_newData_longterm_col",label ="Select column to be used as long-term reference line",
  #                        choices=cols_avail,
  #                        selected=''
  #   )
  #   
  #   updateSelectizeInput(session,"overlay_newData_upper_col",label ="Select column to be used as upper bound",
  #                        choices=cols_avail,
  #                        selected='',
  #                        options = list(hideSelected = FALSE))
  #   
  #   updateSelectizeInput(session,"overlay_newData_date_col",label ="Select month-day column",
  #                        choices=cols_avail,
  #                        selected='',
  #                        options = list(hideSelected = FALSE))
  # })
  
  
  
  #saving block of code if newData comes back
  
 # else if ((input$dailyStats_ts_overlay_metrics=="mean"|input$dailyStats_ts_overlay_metrics=="median")&input$overlay_shading=="newData") {
    # This option is removed from the front end for now
    # overlay_data <- uploaded_overlay_newData()
    # 
    # 
    # # save(data_to_add_as_overlay,file="test_data_to_add_as_overlay.RData")
    # 
    # output$plot_dailyStats_ts_overlay <- renderPlotly({
    #   isolate({
    # 
    #     p1 <- ggplot(data_to_plot)+
    #       geom_line(aes(x=as.Date(yday(Date),"2000-01-01"),y=!!sym(mean_col),colour=year),size=0.8)
    # 
    #     if((input$overlay_newData_lower_col!='')&(input$overlay_newData_upper_col!='')){
    #       overlay_cols_selected <- c(input$overlay_newData_date_col,input$overlay_newData_lower_col,input$overlay_newData_upper_col)
    #       data_to_add_as_overlay <- overlay_data[overlay_cols_selected]
    #       p1<- p1 +
    #         geom_ribbon(data=data_to_add_as_overlay,aes(x=as.Date(yday(paste0("2000","-",month_day)),"2000-01-01"),
    #                                                     ymin=!!sym(input$overlay_newData_lower_col),ymax=!!sym(input$overlay_newData_upper_col),
    #                                                     fill=input$overlay_newData_name),alpha=0.5)+
    #         scale_fill_manual(" ",labels=input$overlay_newData_name,values=c("grey80"="grey80"))
    #     }
    # 
    #     if(input$overlay_newData_longterm_col!=''){
    #       data_to_add_as_longterm <- overlay_data[c(input$overlay_newData_date_col,input$overlay_newData_longterm_col)]
    #       print(input$overlay_newData_longterm_col)
    #       p1 <- p1 +
    #         geom_line(data=data_to_add_as_longterm,aes(x=as.Date(yday(paste0("2000","-",month_day)),"2000-01-01"),y=!!sym(input$overlay_newData_longterm_col),color="USGS long-term"),size=0.8,color="black")
    #     }
    # 
    #     p1 <- p1+
    #       scale_x_date(date_breaks="1 month",limits=c(as.Date("2000-01-01"),as.Date("2000-12-31")),date_labels = "%m%d")+
    #       labs(title=isolate(input$dailyStats_ts_overlay_title),x = "MonthDay",y =mean_col)+
    #       theme_classic()+
    #       theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
    #             ,panel.border = element_rect(colour="black",fill=NA, size=0.5)
    #             ,plot.title = element_text(hjust=0.5)
    #             ,plot.background = element_rect(color="grey20",size=2)
    #             ,legend.position = "right"
    #             ,axis.text.x=element_text(angle=45, hjust=1))
    #     p1<- plotly::ggplotly(p1)
    #     print(p1)
    # 
    #   })
    # }) # renderPlot close
    # 
    # output$plot_dailyStats_ts_overlay_1 <- renderPlot({
    #   isolate({
    #     p1 <- ggplot(data_to_plot)+
    #       geom_line(aes(x=as.Date(yday(Date),"2000-01-01"),y=!!sym(mean_col),colour=year),size=0.8)
    #     if((input$overlay_newData_lower_col!='')&(input$overlay_newData_upper_col!='')){
    #       overlay_cols_selected <- c(input$overlay_newData_date_col,input$overlay_newData_lower_col,input$overlay_newData_upper_col)
    #       data_to_add_as_overlay <- overlay_data[overlay_cols_selected]
    #       p1 <- p1+
    #         geom_ribbon(data=data_to_add_as_overlay,aes(x=as.Date(yday(paste0("2000","-",month_day)),"2000-01-01"),
    #                                                     ymin=!!sym(input$overlay_newData_lower_col),ymax=!!sym(input$overlay_newData_upper_col),
    #                                                     fill=input$overlay_newData_name),alpha=0.5)+
    #         scale_fill_manual(" ",labels=input$overlay_newData_name,values=c("grey80"="grey80"))
    #     }
    # 
    #     if(input$overlay_newData_longterm_col!=''){
    #       data_to_add_as_longterm <- overlay_data[c(input$overlay_newData_date_col,input$overlay_newData_longterm_col)]
    #       p1 <- p1 + geom_line(data=data_to_add_as_longterm,aes(x=as.Date(yday(paste0("2000","-",month_day)),"2000-01-01"),y=!!sym(input$overlay_newData_longterm_col),color="USGS long-term"),size=0.8,color="black")
    #     }
    # 
    #     p1 <- p1+
    #       scale_x_date(date_breaks="1 month",limits=c(as.Date("2000-01-01"),as.Date("2000-12-31")),date_labels = "%m%d")+
    #       labs(title=isolate(input$dailyStats_ts_overlay_title),x = "MonthDay",y =mean_col)+
    #       theme_classic()+
    #       theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
    #             ,panel.border = element_rect(colour="black",fill=NA, size=0.5)
    #             ,plot.title = element_text(hjust=0.5)
    #             ,plot.background = element_rect(color="grey20",size=2)
    #             ,legend.position = "right"
    #             ,axis.text.x=element_text(angle=45, hjust=1))
    #     #p1<- plotly::ggplotly(p1)
    #     print(p1)
    # 
    #   })
    # }) # renderPlot close
    
 # }
  
  
}
