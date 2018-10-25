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

server <- function(input, output) {
  
  ## List Character State Index Choices
  characters <- eventReactive(input$getlistbutton, {
    data <- getMetadata(input$nexus1$datapath)
    characters <- listCharacterStates(data)
  })
  output$characterlist <- DT::renderDataTable({DT::datatable(characters(),
                                                             rownames = FALSE,
                                                             colnames = c("Index",
                                                                          "Character State"),
                                                             options = list(dom = 't',
                                                                            autoWidth = TRUE,
                                                                            initComplete = JS(
                                                                              "function(settings, json) {",
                                                                              "$(this.api().table().header()).css({'background-color': '#2d3e4f', 'color': '#fff'});",
                                                                              "}")),
                                                             selection = 'single')})
  
  #output$characterlist <- renderTable({characters()})
  
  
  ## Network Viz
  graph <- eventReactive(input$plotbutton, {
    graph <-  makeTransNet(fileName = input$nexus1$datapath,
                           #charIndex = input$charIndex,
                           charIndex = input$characterlist_row_last_clicked,
                           centralityMetric = input$metricradio)
  })
  
  # output$graphplot <- renderPlot({print(graph())})
  output$graphplot <- renderVisNetwork({print(graph())})
  
  ## Nexus File Output
  output$nexustable <- renderTable({
    df <- read.csv(input$nexus1$datapath)
    return(df)
  })
  
  ## Nexus File Output
  output$metricstable <- DT::renderDataTable({DT::datatable(read.csv(paste0(input$nexus1$datapath,"_metrics.csv")),
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
  #   metrics <- read.csv(paste0(input$nexus1$datapath,"_metrics.csv"))
  #   return(metrics)
  # })
  
  # Downloadable CSV of metrics
  output$downloadmetrics <- downloadHandler(
    filename = function() {
      paste0(input$nexus1, "_metrics.csv")
    },
    content = function(file) {
      write.csv(read.csv(paste0(input$nexus1$datapath,"_metrics.csv")), file, row.names = FALSE)
    }
  )
  

}