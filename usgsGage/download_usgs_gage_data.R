

library(shiny)
library(shinyFiles)
library(rmarkdown)
library(rsconnect)
library(ContDataQC)
library(zoo)
library(shinythemes)
library(zip)
library(shinyjs)

#Converts a string of USGS site IDs (comma delimited)
#into an array of site IDs for gage data retrieval
USGSsiteParser <- function(siteIDs) {
  USGSsiteVector <- unlist(strsplit(siteIDs, split=", "))
  return(USGSsiteVector)
}

#Deletes the input csvs and output QC csvs
# and Word reports from the server after each download
#(actually, after new data are uploaded)
deleteFiles <- function(directory, inputFiles) {
  
  # #Lists the paths and names of the input csvs
  csvsInputsToDelete <- substring(list.files(path = directory
                                             , pattern = "QC.*csv"
                                             , full.names = FALSE), 4)
  #csvsInputsToDelete <- paste(directory, csvsInputsToDelete, sep="/")
  csvsInputsToDelete <- file.path(directory, csvsInputsToDelete)
  
  #Lists all the output csvs and QC Word documents on the server from QCRaw
  csvsOutputsToDelete <- list.files(path = directory
                                    , pattern = "QC.*csv"
                                    , full.names = TRUE)
  htmlOutputsToDelete <- list.files(path = directory
                                    , pattern = ".*html"
                                    , full.names = TRUE)
  pdfOutputsToDelete <- list.files(path = directory
                                   , pattern = ".*pdf"
                                   , full.names = TRUE)
  logOutputsToDelete <- list.files(path = directory
                                   , pattern = ".*tab"
                                   , full.names = TRUE)
  gageOutputsToDelete <- list.files(path = directory
                                    , pattern = ".*Gage.*csv"
                                    , full.names = TRUE)
  #inputsToDelete <- paste(directory, inputFiles, sep="/")
  inputsToDelete <- file.path(directory, inputFiles)
  
  #Actually deletes the files
  file.remove(csvsOutputsToDelete)
  file.remove(htmlOutputsToDelete)
  file.remove(pdfOutputsToDelete)
  file.remove(logOutputsToDelete)
  file.remove(csvsInputsToDelete)
  file.remove(gageOutputsToDelete)
}