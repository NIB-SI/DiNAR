

library(shiny)

# fluidPage(
  navbarPage("DiNARsubApp",
             
    tabPanel("Intro", 
             htmlOutput("text0")
    ),
             
    tabPanel("tables",
    
      titlePanel("Uploading Files"),
        sidebarLayout(
          sidebarPanel(
            # checkboxInput('header', 'Header', TRUE),
            radioButtons('sep', 'Separator',
                         c(Tab = '\t',
                           Comma = ',',
                           Semicolon = ';'),
                         '\t'),
            tags$hr(),
            fileInput('file1', 'Choose Nodes File',
                      accept = c('text/csv', 
                               'text/comma-separated-values,text/plain', 
                               '.csv')),
            tags$hr(),
            fileInput('file2', 'Choose Edges File',
                      accept = c('text/csv', 
                                 'text/comma-separated-values,text/plain', 
                                 '.csv')),
            tags$hr(),
            textInput("caption1", "Type in desired network name:", "CustomNetwork"),
            verbatimTextOutput("value1"),
            tags$hr(),
            actionButton("go1", "Go!")
          ),
          mainPanel(
            tabsetPanel(
              tabPanel("Plot",
                       conditionalPanel(
                         condition = "(input.go1!=0)",
                          plotOutput('igraphPlot1')),
                       conditionalPanel(
                         condition = "(input.go1!=0)",
                         downloadButton("downloadGraphmlT", "Download")
                       )
                       ),
              tabPanel("Nodes",
                       conditionalPanel(
                         condition = "(input.go1!=0)", 
                         DT::dataTableOutput('nodesNEW')),
                       conditionalPanel(
                         condition = "(input.go1!=0)", 
                          downloadButton("downloadNodesT", "Download")
                         )
                       ),
              tabPanel("Edges",
                       conditionalPanel(
                         condition = "(input.go1!=0)", 
                         DT::dataTableOutput('edgesNEW')),
                       conditionalPanel(
                         condition = "(input.go1!=0)", 
                         downloadButton("downloadEdgesT", "Download")
                         )
                       )
            )
          )
        )
    ),

  tabPanel(".graphml",
           titlePanel("Uploading Files"),
           sidebarLayout(
             sidebarPanel(
               fileInput('file3', 'Choose .graphml File',
                         accept = c('.graphml')),
               tags$hr(),
               textInput("caption2", "Type in desired network name:", "CustomNetwork"),
               verbatimTextOutput("value2"),
               tags$hr(),
               actionButton("go2", "Go!")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Plot",
                          conditionalPanel(
                            condition = "(input.go2!=0)",
                            plotOutput('igraphPlot2'))#,
                          # conditionalPanel(
                          #   condition = "(input.go2!=0)",
                          #   downloadButton("downloadGraphmlT", "Download")
                          # )
                 ),
                 tabPanel("Nodes",
                          conditionalPanel(
                            condition = "(input.go2!=0)",
                            DT::dataTableOutput('nodesNEW2')),
                          conditionalPanel(
                            condition = "(input.go2!=0)",
                            downloadButton("downloadNodesG", "Download")
                          )
                 ),
                 tabPanel("Edges",
                          conditionalPanel(
                            condition = "(input.go2!=0)",
                            DT::dataTableOutput('edgesNEW2')),
                          conditionalPanel(
                            condition = "(input.go2!=0)",
                            downloadButton("downloadEdgesG", "Download")
                          )
                 )
               )
             )
           )
  ),
  tabPanel("graphml & .xgmml",
          titlePanel("Uploading Files"),
           sidebarLayout(
             sidebarPanel(
               fileInput('file4', 'Choose .graphml File',
                         accept = c('.graphml')),
               tags$hr(),
               fileInput('file5', 'Choose .xgmml File',
                         accept = c('.xgmml')),
               tags$hr(),
               textInput("caption3", "Type in desired network name:", "CustomNetwork"),
               verbatimTextOutput("value3"),
               tags$hr(),
               actionButton("go3", "Go!")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Plot",
                          conditionalPanel(
                            condition = "(input.go3!=0)",
                            plotOutput('igraphPlot3'))#,
                          # conditionalPanel(
                          #   condition = "(input.go2!=0)",
                          #   downloadButton("downloadGraphmlT", "Download")
                          # )
                 ),
                 tabPanel("Nodes",
                          conditionalPanel(
                            condition = "(input.go3!=0)",
                            DT::dataTableOutput('nodesNEW3')),
                          conditionalPanel(
                            condition = "(input.go3!=0)",
                            downloadButton("downloadNodesX", "Download")
                          )
                 ),
                 tabPanel("Edges",
                          conditionalPanel(
                            condition = "(input.go3!=0)",
                            DT::dataTableOutput('edgesNEW3')),
                          conditionalPanel(
                            condition = "(input.go3!=0)",
                            downloadButton("downloadEdgesX", "Download")
                          )
                 )
               )
             )
           )
  )
  

)
# )

