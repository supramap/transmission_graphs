library(shiny)
library(shinythemes)
library(ape)
library(castor)
library(visNetwork)
library(hashmap)
library(plyr)
library(network)
library(igraph)
library(data.table)
library(DT)
library(magrittr)
library(htmlwidgets)
source("../transnet.R")

server <- function(input, output, session) {
  
  ## List State Column Choices
  availablecolumns <- eventReactive(input$getlistbutton, {
    availablecolumns <- listStates(csvFileName = input$csvfile$datapath)
  })
  
  output$columnselection <- DT::renderDataTable({DT::datatable(availablecolumns(),
                                                               rownames = FALSE,
                                                               colnames = c("Index",
                                                                            "State"),
                                                               options = list(dom = 't',
                                                                              autoWidth = TRUE,
                                                                              initComplete = JS(
                                                                                "function(settings, json) {",
                                                                                "$(this.api().table().header()).css({'background-color': '#2d3e4f', 'color': '#fff'});",
                                                                                "}")),
                                                               selection = 'single')})
  
  # observeEvent(availablecolumns(), {
  #   updateSelectInput(session = session,
  #                     inputId = "columnselection",
  #                     choices = availablecolumns()$Column)
  # })
  
  ## Network Viz
  graph <- eventReactive(input$plotbutton, {
    validate(
      need(input$treefile != "", "\n1. Please upload a tree file."),
      need(input$csvfile != "",  "\n2. Please upload the accompanying metadata file."),
      # need(input$columnSelection != "",  "\n3. List the columns and pick one to use.")
      if (exists("input$treefile") & exists("input$csvfile")){
        need(!input$input$columnselection_row_last_clicked %in% getUsableColumns(treeFileName = input$treefile$datapath,
                                                                                          csvFileName = input$csvfile$datapath),
             "\n3. Please select a different column. This column has all identical values.")
      }

    )
    graph <-  makeTransNet(treeFileName = input$treefile$datapath,
                           csvFileName = input$csvfile$datapath,
                           # columnSelection = input$columnselection,
                           columnSelection = input$columnselection_row_last_clicked,
                           centralityMetric = input$metricradio)
  })
  
  # output$graphplot <- renderPlot({print(graph())})
  output$graphplot <- renderVisNetwork({print(graph())})
  
  ## Tree File Preview
  output$treepreview <- renderPlot({
    # df <- read.csv(input$treefile$datapath)
    treepreview <- ape::read.tree(input$treefile$datapath)
    #return(treepreview)
    plot(treepreview)
  })
  
  ## File Output
  output$metricstable <- DT::renderDataTable({DT::datatable(read.csv(paste0(input$treefile$datapath,"_metrics.csv")),
                                                            colnames = c("Metastates",
                                                                         "Degree",
                                                                         "Indegree",
                                                                         "Outdegree",
                                                                         "Betweeness",
                                                                         "Closeness",
                                                                         "Source Hub Ratio"),
                                                            options = list(autoWidth = TRUE,
                                                                           initComplete = JS(
                                                                             "function(settings, json) {",
                                                                             "$(this.api().table().header()).css({'background-color': '#2d3e4f', 'color': '#fff'});",
                                                                             "}")))})
  
  # output$metricstable <- renderTable({
  #   metrics <- read.csv(paste0(input$treefile$datapath,"_metrics.csv"))
  #   return(metrics)
  # })
  
  # Downloadable CSV of metrics
  output$downloadmetrics <- downloadHandler(
    filename = function() {
      paste0(input$treefile, "_metrics.csv")
    },
    content = function(file) {
      write.csv(read.csv(paste0(input$treefile$datapath,"_metrics.csv")), file, row.names = FALSE)
    }
  )
  

}