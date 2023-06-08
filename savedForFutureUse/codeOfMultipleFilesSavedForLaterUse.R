
## Load all the uploaded files to a list, this feature will be activated in the future
# datasetlist <- eventReactive(input$uploadId,{
#
#   Selected_Files <- list.files("Selected_Files/")
#   Sys.sleep(2)
#   File_Format <- readRDS("File_Format.rds")
#   datalist <- list()
#   datalist <- lapply(1:length(File_Format[[1]]), function(d) read.csv(paste0("Selected_Files/",File_Format$file[d] ),
#                                                                       header = File_Format$header[d],
#                                                                       sep = File_Format$sep[d],
#                                                                       dec = File_Format$dec[d],
#                                                                       check.names = FALSE,
#                                                                       quote = File_Format$quote[d]))
#   names(datalist) <- paste(File_Format$index, File_Format$file,sep = ". ")
#   return(datalist)
#
# })

# output$manage <- renderUI({
#   data <- uploaded_data() ## datasetlist()
#   print(length(data))
#   selectInput("dataset", "Dataset", choices = loaded_data$name, selected = loaded_data$name)  ## names(data) if use datasetlist()
# })
# 
# output$siteType <- renderUI({
#   data <- uploaded_data() ## datasetlist()
#   selectInput("siteType_input",label="Single site or multiple sites",
#               choices = c("Single site","Multiple sites"),
#               selected = "Single site")
# })