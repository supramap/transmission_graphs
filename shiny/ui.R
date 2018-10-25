library(shiny)
library(shinythemes)

ui <- tagList(
  navbarPage(
    theme = shinytheme("flatly"),
    title = "TransNet",
    tabPanel("Network Visualizer",
             sidebarPanel(
               width = 3,
               fileInput('nexus1',
                         label = 'Choose your NEXUS File',
                         accept = c('text/nexus', 'text/plain', '.nex', '.nexus')),
               actionButton("getlistbutton", label = "List Character States", class = "btn-primary"),
               #tableOutput("characterlist"),
               dataTableOutput("characterlist"),
               # numericInput('charIndex',
               #              label = 'Character State Index',
               #              value = 1,
               #              min = 1,
               #              step = 1,
               #              width = "50%"),
               radioButtons("metricradio",
                            label ="Centrality Metric",
                            choices = list("Indegree" = 1,
                                           "Outdegree" = 2,
                                           "Betweenness" = 3,
                                           "Closeness" = 4,
                                           "Degree" = 5,
                                           "Source Hub Ratio" = 6),
                            selected = 1),
               actionButton("plotbutton", label = "Generate Network", class = "btn-primary")
             ),
             mainPanel(
               width = 9,
               tabsetPanel(
                 tabPanel("Network Plot",
                          # plotOutput("graphplot")
                          visNetworkOutput("graphplot")
                 ),
                 tabPanel("Nexus Preview",
                          h4("Nexus File Contents"),
                          tableOutput("nexustable")
                          ),
                 tabPanel("Metrics",
                          downloadButton("downloadmetrics", "Download Output Metrics"),
                          DT::dataTableOutput("metricstable")
                          # tableOutput("metricstable")
                          )
               )
             )
    ),
    tabPanel("Network Comparison",
             "Coming soon...")
  )
)