

library(shiny)

# fluidPage(
  navbarPage("DiNARsubApp",
             
    tabPanel("Intro", 
             htmlOutput("text0")
    ),
             
    tabPanel("Input Files",
    
      titlePanel("Uploading Files"),
        sidebarLayout(
          sidebarPanel(
            checkboxInput("header1", "My file containes header", TRUE),
            
            radioButtons("sep1", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = "\t"),
            
            radioButtons("quote1", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = ""),
            
            fileInput("file1", "Choose Nodes File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            tags$hr(),
            
            checkboxInput("header2", "My file containes header", TRUE),
            
            radioButtons("sep2", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = "\t"),
            
            radioButtons("quote2", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = ""),
            
            fileInput("file2", "Choose Edges File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
          
            tags$hr(),
            textInput("caption1", "Type in desired network name:", "CustomNetwork"),
            verbatimTextOutput("value1"),
            tags$hr(),
            actionButton("goButton", "Generate!"),
            p("Click the button to generate the intersection table and check results in the output tab panel"),
            tags$hr()
          ),
          mainPanel(      
            navbarPage('Tables',
                                     tabPanel(title = 'input',
                                              value = "1",
                                              br(),
                                              p(strong("Upload"), " files and ", strong("select columns"), " of interest, in order, containing: ", strong("geneID"), " and, ", strong("short Name, coordinate x and coordinate y"), " if any"),
                                              br(),
                                              DT::dataTableOutput('ex1'),
                                              verbatimTextOutput('y22'), 
                                              br(),      
                                              br(),
                                              p(strong("Upload"), " files and ", strong("select columns"), " of interest, in order, containing: ", strong("geneID1, geneID2"), " and, ", strong("reaction Type"), " if any"),
                                              DT::dataTableOutput('ex2'),
                                              verbatimTextOutput('y33')
                                     ),
                                     
                                     tabPanel(title = 'output',
                                              value = "2",
                                              br(),
                                              br(),
                                              conditionalPanel(
                                                condition = "(output.goButton!=0)",
                                                # busyIndicator(text = "Loading, please wait...", wait = 400), 
                                                DT::dataTableOutput('y441')), # %>% withSpinner(color="#A9F5F2", proxy.height = "50px"),
                                              br(),
                                              br(),
                                              conditionalPanel(
                                                condition = "(output.goButton!=0)",
                                                # busyIndicator(text = "Loading, please wait...", wait = 400), 
                                                DT::dataTableOutput('y442')), # %>% withSpinner(color="#A9F5F2", proxy.height = "50px"),
                                              br()
                                     ),
                                     
                                           tabPanel("Plot",
                                                    conditionalPanel(
                                                      condition = "(output.goButton!=0)",
                                                      plotOutput('igraphPlot1',
                                                                 height = "800px",
                                                                 width = "800px")),
                                                    conditionalPanel(
                                                      condition = "(output.goButton!=0)",
                                                      downloadButton("downloadGraphmlT", "Download")
                                                    )
                                           ),
                                     
                                     id = "tabselected")
          )
          )
    ),
    tabPanel("Shortest path",
             titlePanel("Choose IDs"),
             sidebarLayout(
               sidebarPanel(
                 textInput("Xfrom", "geneID 1 to calculate shortest path:", "X"),
                 verbatimTextOutput("geneIDfrom"),
                 tags$hr(),
                 textInput("Xto", "geneID 2 to calculate shortest path:", "Y"),
                 verbatimTextOutput("geneIDto"),
                 tags$hr(),
                 actionButton("goButton2", "Generate!"),
                 p("Click the button to generate the subnetwork"),
                 tags$hr()
               ),
             mainPanel(
               navbarPage('Shortest Path',

                 #### see https://rdrr.io/cran/igraphinshiny/
                 tabPanel(title = "Subnetwork Plot", 
                          value = "3",
                          conditionalPanel(
                            condition = "(output.goButton2!=0 & output.geneIDfrom!=NULL & output.geneIDto!=NULL)",
                            # busyIndicator(text = "Loading, please wait...", wait = 400), 

                          wellPanel(
                           fluidRow(
                             column(12, wellPanel(
                               plotOutput("graphPlot")
                             ))
                           ),
                           fluidRow(
                             column(4, wellPanel(
                               radioButtons(inputId="PlotLayout", label="Plot Layout (igraphinshiny)", choices=c("Auto","Random","Circle","Sphere","Fruchterman Reingold","Kamada Kawai","Drl","Spring","Reingold Tilford", "Lgl","Graphout"), selected="Auto")
                             )),
                             column(4, wellPanel(
                               checkboxInput(inputId = "showNodeName", label = "Show Short Name",  value = TRUE),
                               checkboxInput(inputId = "showEdgeName", label = "Show Edge Type",  value = FALSE),
                               sliderInput(inputId = "vertexSize", label = "Vertex Size",  value = 15, min=1, max=100)
                             )),
                             column(4, wellPanel(
                               downloadButton('downloadPlot', 'Download Plot in pdf')
                             ))
                           )
                 ))),
                 tabPanel(title = 'Subnetwork Centralities by igraphinshiny',
                          value = "4",
                          br(),
                          conditionalPanel(
                            condition = "(output.goButton2!=0 & output.geneIDfrom!=NULL & output.geneIDto!=NULL)",
                            # busyIndicator(text = "Loading, please wait...", wait = 400),
                            tableOutput("Centralities"))

                 )

                           
    ),
    id = "tabselected2")
    )
    )
)


