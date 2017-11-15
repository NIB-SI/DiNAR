

library(shiny)

if (!require("knitr")) install.packages("knitr")
library(knitr)
if (!require("markdown")) install.packages("markdown")
library(markdown)
if (!require("rmarkdown")) install.packages("rmarkdown")
library(rmarkdown)

nodes1 <- reactiveValues()
edges1 <- reactiveValues()
myLKNname <<- reactiveValues()

function(input, output) {
  
  nodes1 <- reactive({
    

    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    dir0 = getwd()
    dir1 = paste(dir0,'/netTables',sep='')
    ifelse(!dir.exists(dir1), dir.create(dir1), FALSE)
    
    myNodes = read.csv(inFile$datapath, 
                       header=input$header, 
                       sep=input$sep, 
                       quote='')
    write.table(myNodes, 
                file = paste0(dir1, "/Nodes.txt"), 
                append = FALSE, 
                quote = FALSE, sep = "\t",
                row.names = FALSE,
                col.names = TRUE)
    
    return(myNodes)
    
  })
  
  output$contents1 <- renderDataTable({
    
    return(nodes1())
    
  })
  
  edges1 <- reactive({
    
    
    inFile <- input$file2
    
    if (is.null(inFile))
      return(NULL)
    
    dir0 = getwd()
    dir1 = paste(dir0,'/netTables',sep='')
    ifelse(!dir.exists(dir1), dir.create(dir1), FALSE)
    
    myEdges = read.csv(inFile$datapath, 
             header=input$header, 
             sep=input$sep, 
             quote='')
    
    write.table(myEdges, 
                file = paste0(dir1, "/Edges.txt"), 
                append = FALSE, 
                quote = FALSE, sep = "\t",
                row.names = FALSE,
                col.names = TRUE)
    
    return(myEdges)
  })
  
  output$contents2 <- renderDataTable({
    
    return(edges1())
    
  })
  
  myLKNname0 <<- reactive({
    if (is.null(input$caption))
      return(NULL)
    return(input$caption)
  })
  
  output$value <- renderText({ 
    return(paste0(myLKNname0(), '.graphml'))
  })
  

  observeEvent(input$do, {
    source("01_createGraphML.R")
    render("02_multilevel_and_spinglass_clustering.Rmd", "all")
    cat("\n\n", "DONE!","\n\n", "You may close the app and check the results.")
  })
  
}

