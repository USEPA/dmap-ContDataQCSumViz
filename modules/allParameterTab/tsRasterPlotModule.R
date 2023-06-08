#' Continuous Data Exploration / All Parameter / Raster graphs (user interface side)
#'
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
TsRasterPlotModuleUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(class="panel panel-default",style="margin:10px;",
          div(class="panel-heading"),
          div(class="panel-body",
              uiOutput(ns("raster_input_1")),
              uiOutput(ns("raster_input_2")),
              uiOutput(ns("raster_input_3")),
              uiOutput(ns("raster_input_4")),
              uiOutput(ns("raster_input_5")),
              hr(),
              uiOutput(ns("raster_input_6"))
          ) # end of panel body
      ) #end of panel
    ),
    mainPanel(
      width = 9,
      column(width = 12, 
             div(style="width:100%", uiOutput(ns("rasterError"))),
             plotOutput(ns("display_raster_graphs"),height="550px",width="100%"))
    ) # mainPanel end
  ) # sidebarLayout end
}

#' Continuous Data Exploration / All Parameter / Raster graphs (server side)
#'
#' @param id 
#' @param dailyStats 
#' @param renderRasterPlot 
#'
TsRasterPlotModuleServer <- function(id, dailyStats, renderRasterPlot) {
 
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
           
            localStats <- dailyStats
            myList <- localStats$processed_dailyStats
            variables_avail$params <- names(localStats$processed_dailyStats)
           
            if(renderRasterPlot$render == TRUE) {
                  output$raster_input_1 <- renderUI({
                    selectizeInput(ns("dailyStats_raster_variable_name"),label ="Select variable name",
                                   choices=variables_avail$params,
                                   multiple = FALSE,
                                   selected=variables_avail$params[1],
                                   options = list(hideSelected = FALSE))
                  })
                  
                  output$raster_input_2 <- renderUI({
                    
                    selectizeInput(ns("dailyStats_raster_metrics"),label ="Select daily statistics metrics",
                                   choices=c("mean","median","min", "max","range","sd","var","cv","n"),
                                   multiple = FALSE,
                                   selected="mean",
                                   options = list(hideSelected = FALSE))
                  })
                  
                  output$raster_input_3 <- renderUI({
                    textInput(inputId=ns("dailyStats_raster_title"), label="Plot title",value="")
                    
                  })
                  
                  output$raster_input_4 <- renderUI({
                    numericInput(inputId=ns("raster_plot_aspect_ratio"), label="Adjust plot aspect ratio",0.5,min=0,max=10,step=0.1)
                    
                  })
                  
                  output$raster_input_5 <- renderUI({
                    radioButtons(inputId=ns("raster_plot_color"),label ="Color palette options",choices=c("hcl","rainbow","heat","terrain","topo"),selected="hcl",inline=FALSE)
                  })
                  
                  output$raster_input_6 <- renderUI({
                    actionButton(inputId=ns("run_raster"), label="Display",class="btn btn-primary")
                  })

            }
          })
          
          observeEvent(input$run_raster, {
            clearContents()
            localStats <- dailyStats
            myMonth <- seq(as.Date("2020-01-01"),as.Date("2020-12-31"),by="1 month")
            month_numeric <- lubridate::yday(myMonth)/365*52+1
            month_label <- lubridate::month(myMonth,label=TRUE)
            myList <- localStats$processed_dailyStats
            variable_to_plot <- input$dailyStats_raster_variable_name
            myData <- myList[[which(names(myList)==variable_to_plot)]]
            mean_col <- paste0(input$dailyStats_raster_variable_name,".",input$dailyStats_raster_metrics)
            cols_selected <- c("Date",mean_col)
            data_to_plot <- myData[cols_selected]
            data_to_plot[,"year"] <- format(data_to_plot[,"Date"],"%Y")
            if (input$raster_plot_color=="hcl"){
              colorV <- hcl.colors(12)
            }else if(input$raster_plot_color=="rainbow"){
              colorV <- rainbow(12)
            }else if(input$raster_plot_color=="terrain"){
              colorV <- terrain.colors(12)
            }else if (input$raster_plot_color=="heat"){
              colorV <- heat.colors(12)
            }else if (input$raster_plot_color=="topo"){
              colorV <- topo.colors(12)
            }
            #data_to_plot[,"yday"] <- lubridate::yday(as.Date(data_to_plot[,"Date"],format="%Y-%m-%d"))
            if (!all(is.na(data_to_plot[,mean_col]))){
              output$display_raster_graphs <- renderPlot({
                p1 <- ggplot(data_to_plot, aes(x=as.Date(yday(Date),"2000-01-01"),y=year))+
                  geom_raster(aes(fill=!!sym(mean_col)))+
                  coord_equal()+
                  scale_fill_gradientn(name=mean_col,na.value="white",colours=colorV)+
                  scale_x_date(date_breaks="1 month",date_labels = "%b")+
                  scale_colour_manual(values=NA)+
                  labs(title=isolate(input$dailyStats_raster_title), x = "month",y = "year")+
                  guides(color=guide_legend("No data",override.aes = list(fill="white")))+
                  theme_classic()+
                  theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                        ,panel.border = element_rect(colour="black",fill=NA, size=0.5)
                        ,plot.title = element_text(hjust=0.5)
                        ,aspect.ratio = isolate(input$raster_plot_aspect_ratio)
                        ,plot.background = element_rect(color="grey20",size=2)
                        ,legend.position = "right"
                  )
                #ggplotly(p1)
                print(p1)
              })  # renderPlot close
              
            }else{
              renderErrorMsg(noRasterData)
              clearPlot()
              
            }##inner if else loop close
            
          }) ##observeEvent end
          
          #common
          renderErrorMsg <- function(msg) {
            output$rasterError <- renderUI({
              div(class="alert alert-danger" , msg) 
            })
          }
          clearContents <- function(){
            output$rasterError <- renderUI({})
          }
          
          clearPlot <- function(){
            output$display_raster_graphs <- renderPlotly({
              plotly_empty()
            })
          }

    })# end of server module
}
