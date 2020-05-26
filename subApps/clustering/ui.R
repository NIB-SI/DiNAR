

library(shiny)

# fluidPage(
  navbarPage("DiNARsubApp",
             
    tabPanel("Intro", 
             htmlOutput("text0")
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
  )
  

)
# )

