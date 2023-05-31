
################### "Create report"####
#Not used, for the future
# observeEvent(input$createReport,{
# 
#   showModal(modalDialog("Creating the report now...",footer=NULL))
# 
#   #if (!file.exists("Output/reports")) dir.create(file.path("Output/reports"),showWarnings = FALSE, recursive = TRUE)
#   ## copy template file to a temporary directory so that the output file will be saved there
#   ## in case user do not have written permission to the app directory when deployed
#   tempRMD <- file.path(tempdir(),"SiteSummary.Rmd")
#   file.copy("_moved/SiteSummary.Rmd",tempRMD,overwrite= TRUE)
#   build_summary_updated(dir_data="Output/to_report/"
#                 ,file_main="_Captions_SiteX.xlsx"
#                 ,sheet_main="metadata"
#                 ,file_prefix_sep="_"
#                 ,rmd_template=tempRMD
#                 ,output_format=input$report_format
#                 ,output_file=paste0(input$report_name,".",input$report_format))
# 
#   to_download$fileName_report <- paste0(tempdir(),"/",input$report_name,".",input$report_format)
#   print(to_download$fileName_report)
#   removeModal()
# 
# })
# 
# 
# output$downloadReport <- downloadHandler(
#   filename = function(){
#     to_download$fileName_report
#   },
#   content = function(file){
#     file.copy(to_download$fileName_report,file)
#   }
# )