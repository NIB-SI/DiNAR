

library(shiny)

fluidPage(
  titlePanel("Uploading Files"),
  sidebarLayout(
    sidebarPanel(
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Tab='\t',
                     Comma=',',
                     Semicolon=';'),
                   '\t'),
      tags$hr(),
      fileInput('file1', 'Choose Nodes File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      tags$hr(),
      fileInput('file2', 'Choose Edges File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      tags$hr(),
      textInput("caption", "Type in desired network name:", "LKN"),
      verbatimTextOutput("value"),
      tags$hr(),
      actionButton("do", "Go!")
    ),
    mainPanel(
      dataTableOutput('contents1'),
      dataTableOutput('contents2')
    )
  )
)

