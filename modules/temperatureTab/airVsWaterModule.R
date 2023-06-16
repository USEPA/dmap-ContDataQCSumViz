#' Continuous Data Exploration / Temperature / Air vs Water (user interface side)
#'
#' @param id 
#'
AirVsWaterModuleUI <- function(id) {
  ns <- NS(id)
  shinyjs::useShinyjs()
  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(class="panel panel-default",style="margin:10px;",
          div(class="panel-heading"),
          div(class="panel-body",
              uiOutput(ns("air_vs_water_input_1")),
              uiOutput(ns("air_vs_water_input_2")),
              radioButtons(ns("exclude_data_points"),
                           "Limit the data points with air temperature",
                           choices = c("No" = "No", "Yes" = "Yes"),
                           selected = "No"
              ),
              div(id = ns("cp_air_temp"),
                    uiOutput(ns("air_vs_water_input_4"))
                ), # div end
              hr(),
              uiOutput(ns("display_thermal_sensitivity_button"))
          )#end of panel body
      ) #end of panel
    ),
    mainPanel(
      width = 9,
      column(
        width = 12,
        #uiOutput(ns("display_help_text_air_water")),
        shinydashboard::box(id=ns("display_help_text_air_water"), style="display:none;", width=12, class="well",
                            h4("Temperature - Air vs Water"),
                            div(style="width:100%;", "Because solar radiation affects both air and water temperature, the two parameters are typically closely correlated and many statistical models use changes in air temperature to explain variances in stream temperatures (Caissie 2006). 
                                                      This relationship can be quantified as the slope of the linear regression line between air and water temperature and indicates how sensitive a given stream is to changes in water temperature due to changes in air temperature (Kelleher et al. 2012)"),
                            br(),
                            div(style="width:100%;", "We give users the option of excluding air temperatures <0°C because 0 is generally the point at which additional cooling of water forms ice, causing the linear air-water temperature relationship to break down (Kelleher et al. 2012). 
                                                      This relationship also tends to break down at high temperatures (above ~20°C) (Mohseni and Stefan 1999). In the future, we may add in an option to exclude high-end temperatures and to calculate nonlinear regression equations."),
                            br(),
                            div(style="width:100%" , "For more information, see Kelleher et al. (2012)."),
                            br(),
                            div(style="width:100%", "Citation: Caissie, D. 2006. The Thermal Regime of Rivers: A Review. Freshwater Biology 51 (8): 1389–406. ",
                                a('https://doi.org/10.1111/j.1365-2427.2006.01597.x', href='https://doi.org/10.1111/j.1365-2427.2006.01597.x', target='_blank')),
                            br(),
                            div(style="width:100%", "Kelleher, Christa & Wagener, Thorsten & Gooseff, Michael & Mcglynn, Brian & McGuire, Kevin & Marshall Price, Lucy. 2012. Investigating Controls on the Thermal Sensitivity of Pennsylvania Streams. Hydrological Processes. 26. 771 - 785. 10.1002/hyp.8186."),
                            br(),
                            div(style="width:100%", "Mohseni, O., and H.G. Stefan. 1999. Stream Temperature/Air Temperature Relationship: A Physical Interpretation. Journal of Hydrology 218 (3–4): 128–41.",
                             a('https://doi.org/doi:10.1016/s0022-1694(99)00034-7', href='https://doi.org/doi:10.1016/s0022-1694(99)00034-7', target='_blank'))

        ), # end of box
        div(style="width:100%", uiOutput(ns("airVsWaterError"))),
        plotOutput(ns("display_thermal_sensitivity_plot_1"))
      )
    ) # mainPanel end
  ) # sidebarLayout end

}


#' Continuous Data Exploration / Temperature / Air vs Water (server side)
#'
#' @param id 
#' @param uploaded_data 
#' @param dailyStats 
#' @param renderAirVsWater 
#'
AirVsWaterModuleServer <- function(id, uploaded_data, dailyStats, renderAirVsWater) {
 
  localStats <- reactiveValues(stats=list())
  variables_avail <- reactiveVal()
  
  moduleServer(
    id,
    function(input, output, session) {
          ns <- session$ns
          
          # observeEvent(uploaded_data(), {
          #   variables_avail <- names(uploaded_data())
          # })
          
           observe({
            localStats <- dailyStats
            variables_avail <- names(uploaded_data())
            if(renderAirVsWater$render == TRUE) {
                  output$air_vs_water_input_1 <- renderUI({
                    air_keys_in_favor_order <- c("Air.Temp.C","AIR.TEMP.C","Air_Temp_C","AIR_TEMP_C")
                    possible_air_columns <- air_keys_in_favor_order[air_keys_in_favor_order %in% variables_avail]
                    if (length(possible_air_columns)==0){
                      air_to_select <- variables_avail[grep('air',variables_avail,ignore.case=TRUE)][1]
                    }else{
                      air_to_select <- possible_air_columns[1]
                    }
                    selectizeInput(ns("air_temp_name"),label ="Select Air Temperature Column",
                                   choices=variables_avail,
                                   multiple = FALSE,
                                   selected=air_to_select,
                                   options = list(hideSelected = FALSE))
                  })
                  
                  output$air_vs_water_input_2 <- renderUI({
                    water_keys_in_favor_order <- c("Water.Temp.C","WATER.TEMP.C","Water_Temp_C","WATER_TEMP_C")
                    possible_water_columns <- water_keys_in_favor_order[water_keys_in_favor_order %in% variables_avail]
                    if (length(possible_water_columns)==0){
                      water_to_select <- variables_avail[grep('water',variables_avail,ignore.case=TRUE)][1]
                    }else{
                      water_to_select <- possible_water_columns[1]
                    }
                    selectizeInput(ns("water_temp_name"),label ="Select Water Temperature Column",
                                   choices=variables_avail,
                                   multiple = FALSE,
                                   selected=water_to_select,
                                   options = list(hideSelected = FALSE))
                  })
                  
                  output$display_thermal_sensitivity_button <- renderUI({
                    actionButton(inputId=ns("display_thermal_sensitivity"), label="Display thermal sensitivity",class="btn btn-primary")
                  })
                  #clear previous error messages
                  clearContents()
                  shinyjs::show(id=ns("display_help_text_air_water"), asis=TRUE)
                  
                  #Nilima Gandhi - Remvoing old way, it is a overkill, but keeping the code, do not know if there is a future plan to use the file.
                  # output$display_help_text_air_water <- renderUI({
                  #   verbatimTextOutput(ns("help_text_air_water"))
                  # })
                  # 
                  # output$help_text_air_water <- renderText({
                  #   filePath <- "help_text_files/Temperature_AirWater.txt"
                  #   fileText <- paste(readLines(filePath,encoding="UTF-8"),collapse="\n")
                  #   fileText
                  # })
            }
          })
           
           
           observeEvent(input$display_thermal_sensitivity, {
             localStats <- dailyStats
             clearContents()
             shinyjs::hide(id=ns("display_help_text_air_water"), asis=TRUE)
             
             myList <- localStats$processed_dailyStats
             ## check if both of "Air.Temp.C" and "Water.Temp.C" are available
             #if(all(names(myList) %in% c(input$air_temp_name,input$water_temp_name))){
             if(input$air_temp_name %in% names(myList) & input$water_temp_name %in% names(myList)){
               myData.Air <- myList[[which(names(myList)==input$air_temp_name)]]
               myData.Water <- myList[[which(names(myList)==input$water_temp_name)]]
               mean_col_air <- paste0(input$air_temp_name,".mean")
               mean_col_water <- paste0(input$water_temp_name,".mean")
               data_air_to_plot <- myData.Air[c("Date",mean_col_air)]
               data_water_to_plot <- myData.Water[c("Date",mean_col_water)]
               data_to_plot <- merge(data_air_to_plot,data_water_to_plot,by="Date")
               if (input$exclude_data_points=="Yes"){
                 data_to_plot <- data_to_plot[data_to_plot$Air.Temp.C.mean>input$air_limit_temp,]
               }
               data_to_model <- data_to_plot
               names(data_to_model)[match(mean_col_water,names(data_to_model))] <- "y"
               names(data_to_model)[match(mean_col_air,names(data_to_model))] <- "x"
               myModel <- lm(y ~ x,data_to_model,na.action=na.exclude)
               myEquation <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                                        list(a = format(unname(coef(myModel)[1]), digits = 2),
                                             b = format(unname(coef(myModel)[2]), digits = 2),
                                             r2 = format(summary(myModel)$r.squared, digits = 3)))
               
               output$display_thermal_sensitivity_plot_1 <- renderPlot({
                 p1 <- ggplot(data_to_plot,aes(x=!!sym(mean_col_air),y=!!sym(mean_col_water)))+
                   geom_point(alpha=0.5,size=1.5)+
                   geom_smooth(method="loess",se=FALSE,color="black")+
                   geom_smooth(method="lm",se=FALSE,color="cornflowerblue",linetype="dashed",size=2)+
                   geom_text(x=(min(data_to_plot[,mean_col_air],na.rm=TRUE)+5)
                             ,y=(max(data_to_plot[,mean_col_water],na.rm=TRUE)-1.5)
                             ,label=as.character(as.expression(myEquation))
                             ,color="cornflowerblue"
                               ,size=8
                             ,parse= TRUE)+
                   labs(x = "Air Temperature",y = "Water Temperature")+
                   theme_minimal()+
                   theme(text=element_text(size=16,face = "bold", color="cornflowerblue"),
                         plot.background = element_rect(color="grey20",linewidth=2),
                         legend.position = "right",
                   )
                 #ggplotly(p1)
                 print(p1)
               })  # renderPlot close
               
             }else{
               renderErrorMsg(airvswaterMsg)
               clearPlot()
             } ## outer if else loop close
             
           }) ## observeEvent end

           
           observeEvent(input$exclude_data_points,{
             if (input$exclude_data_points == 'Yes'){
                   air_limit_temp_tooltip_text = paste0("limit the data points with air temperature")
                   output$air_vs_water_input_4 <- renderUI({
                     tipify(numericInput(ns("air_limit_temp"),label ="air temperature less than this value will be excluded",0,min=-10,max=100,step=1.0),air_limit_temp_tooltip_text,placement="right",trigger="hover")
                   })
               shinyjs::show("cp_air_temp")
             }else{
               shinyjs::hide("cp_air_temp")
             }
           })
           
           #common
           renderErrorMsg <- function(msg) {
             output$airVsWaterError <- renderUI({
               div(class="alert alert-danger" , msg) 
             })
           }
           
           clearContents <- function(){
             output$airVsWaterError <- renderUI({})
           }
           
           clearPlot <- function(){
             output$display_thermal_sensitivity_plot_1 <- renderPlotly({
               plotly_empty()
             })
           }
          
    }) # end of module server
}
