library(shiny)
library(shinythemes)

ui <- tagList(
  #shinythemes::themeSelector(),
  navbarPage(
    theme = "superhero",
    title = "TransNet",
    tabPanel("Network Visualizer",
             sidebarPanel(
               fileInput('nexus1',
                         label = 'Choose your NEXUS File',
                         accept = c('text/nexus', 'text/plain', '.nex', '.nexus')),
               numericInput('charIndex',
                            label = 'Character State Index',
                            value = 1,
                            min = 0,
                            step = 1,
                            width = "50%"),
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
               tabsetPanel(
                 tabPanel("Network Plot",
                          h4("Table"),
                          tableOutput("table"),
                          h4("Verbatim text output"),
                          verbatimTextOutput("txtout"),
                          h1("Header 1"),
                          h2("Header 2"),
                          h3("Header 3"),
                          h4("Header 4"),
                          h5("Header 5")
                 ),
                 tabPanel("Nexus Preview", "This panel is intentionally left blank"),
                 tabPanel("Metrics", "This panel is intentionally left blank",
                          downloadButton("downloadmetrics", "Download Output Metrics"))
               )
             )
    ),
    tabPanel("Network Comparison",
             icon = ,
             "This panel is currently blank")
  )
)