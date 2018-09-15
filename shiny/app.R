## Shiny Web Application for Transmission Graphs
library(shiny)

## Load other libraries
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

## Run the application 
# shinyApp(ui = "ui.R",
#          server = "server.R")

runApp('shiny')

#runApp('shiny', display.mode = "showcase")

