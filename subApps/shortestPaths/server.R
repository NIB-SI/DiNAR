

library(shiny)

# if (!require("knitr")) install.packages("knitr")
# library(knitr)
# if (!require("markdown")) install.packages("markdown")
# library(markdown)
# if (!require("rmarkdown")) install.packages("rmarkdown")
# library(rmarkdown)

if (!require("igraph")) install.packages("igraph")
library(igraph)


# By default, Shiny limits file uploads to 5MB per file. 
# 100 MB limit
options(shiny.maxRequestSize=100*1024^2) # 1024*1024*100

set.seed(123456)
'%ni%' = Negate('%in%') 



# getCoordNEr <- list()
getCoordNEr2 <- list()
myoutput <- list()


# nodes1 <- reactiveValues()
# edges1 <- reactiveValues()
# nodesNEW <- reactiveValues()
# edgesNEW <- reactiveValues()
myLKNname1 <- reactiveValues()
myfrom <- reactiveValues()
myto <- reactiveValues()
ntext <- reactiveValues()
ntext2 <- reactiveValues()

# nodesNEW2 <- reactiveValues()
# edgesNEW2 <- reactiveValues()
# myLKNname2 <- reactiveValues()
# graphml1 <- reactiveValues()

# nodesNEW3 <- reactiveValues()
# edgesNEW3 <- reactiveValues()
# myLKNname3 <- reactiveValues()
# xgmml1 <- reactiveValues()

function(input, output, session) {
  
  output$text0 <- renderUI({
    
    str0 <- paste("</br><b><a href='https://NIB-SI.shinyapps.io/DiNAR'>DiNAR</a> Shortest Paths </b></br>",
                  "From <b>nodes/edges tables</b> <a href='https://github.com/NIB-SI/DiNAR'>DiNAR</a></br></br>",
                  "<b><u><i>Node attributes/column names:</i></u></b>",
                  "- geneID (obligatory, case sensitive values),", 
                  "- shortName (string or '-'),",
                  "- x coordinate and ",
                  "- y coordinate </br>",
                  "<b><u><i>Edge attributes/column names:</i></u></b>",
                  "- geneID1 (obligatory, case sensitive values),",
                  "- geneID2 (obligatory, case sensitive values),", 
                  "- reactionType (string or '-')</br>",
                  "<b>Note:</b></br>&#9479;could be time-consuming for large networks (more than 2<sup>11</sup> nodes/edges) when coordinates are not provided</br>",
                  "<b>More information at: </b> <a href='https://github.com/NIB-SI/DiNAR/tree/master/subApps'>https://github.com/NIB-SI/DiNAR/tree/master/subApps</a></br>",
                  sep = "</br>"
    )
    str1 <- paste0('</br>')
    
    HTML(paste(str0, str1,sep = '<br/>'))
  })
  

  output$ex1 <- DT::renderDataTable({
    
    req(input$file1)
    
    tryCatch(
      {
        df1 <- read.csv(input$file1$datapath,
                        header = input$header1,
                        sep = input$sep1,
                        quote = input$quote1)
      },
      error = function(e) {
        # parsing error
        stop(safeError(e))
      }
    )
    
    DT::datatable(df1, options = list(pageLength = 6), selection = list(target = 'column', selected = c(1)))
    
    
  })
  
  
  output$y22 = renderPrint({
    if (!is.na(req(input$file1))[1]) {
      
      cat("Columns selected: ")
      input$ex1_columns_selected      
    }
  })
  
  
  
  output$ex2 <- DT::renderDataTable({
    
    req(input$file2)
    
    tryCatch(
      {
        df2 <- read.csv(input$file2$datapath,
                        header = input$header2,
                        sep = input$sep2,
                        quote = input$quote2)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    DT::datatable(df2, options = list(pageLength = 6), selection = list(target = 'column', selected = c(1)))
  })
  
  
  output$y33 = renderPrint({
    if (!is.na(req(input$file2))[1]) {
      cat("Columns selected: ")
      input$ex2_columns_selected      
    }
  })
  
  ntext <- eventReactive(input$goButton, {
    return(TRUE)
  })
  
  ntext2 <- eventReactive(input$goButton2, {
    return(TRUE)
  })
  
  myoutput = reactive({
    if (!is.na(req(input$file2))[1] & (!is.na(req(input$file1))[1]) & ntext()) {
      
      tryCatch(
        {
          myEdges <- read.csv(input$file2$datapath,
                            header = input$header2,
                            sep = input$sep2,
                            quote = input$quote2,
                            stringsAsFactors = FALSE)
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )
      
      tryCatch(
        {
          myNodes <- read.csv(input$file1$datapath,
                              header = input$header1,
                              sep = input$sep1,
                              quote = input$quote1,
                              stringsAsFactors = FALSE)
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )
      
      if ((length(input$ex1_columns_selected) == 4) & (length(input$ex2_columns_selected) == 3)) {
        
        geneID = input$ex1_columns_selected[1] # geneID
        shortName = input$ex1_columns_selected[2] # geneID
        name = input$ex1_columns_selected[1] # geneID
        x = input$ex1_columns_selected[3] # geneID
        y = input$ex1_columns_selected[4] # geneID
        geneID1 = input$ex2_columns_selected[1]
        geneID2 = input$ex2_columns_selected[2]
        from = input$ex2_columns_selected[1]
        to = input$ex2_columns_selected[2]
        reactionType = input$ex2_columns_selected[3]
        o1 = myNodes[,c(geneID, shortName, x, y)]
        o4 = myNodes[,c(name, geneID, shortName, x, y)]
        o2 = myEdges[,c(geneID1, geneID2, reactionType)]
        o3 = myEdges[,c(from, to, geneID1, geneID2, reactionType)]
        

        colnames(o1) = c("geneID", "shortName", "x", "y")
        colnames(o4) = c("name", "geneID", "shortName", "x", "y")
        colnames(o2) = c("geneID1", "geneID2", "reactionType")
        colnames(o3) = c("from", "to", "geneID1", "geneID2", "reactionType")
        
        g <- graph.data.frame(o3, vertices = o4, directed = TRUE)
        

        return(list(o1, o2, g))
      }
      
      if ((length(input$ex1_columns_selected) == 1) & (length(input$ex2_columns_selected) == 2)) {
        
        geneID = input$ex1_columns_selected[1] # geneID
        shortName = input$ex1_columns_selected[1] # geneID
        name = input$ex1_columns_selected[1] # geneID
        geneID1 = input$ex2_columns_selected[1]
        geneID2 = input$ex2_columns_selected[2]
        from = input$ex2_columns_selected[1]
        to = input$ex2_columns_selected[2]
        o1 = myNodes[,c(geneID, shortName)]
        o4 = myNodes[,c(name, geneID, shortName)]
        o2 = myEdges[,c(geneID1, geneID2)]
        o3 = myEdges[,c(from, to, geneID1, geneID2)]
        
        
        colnames(o1) = c("geneID", "shortName")
        colnames(o4) = c("name", "geneID", "shortName")
        colnames(o2) = c("geneID1", "geneID2")
        colnames(o3) = c("from", "to", "geneID1", "geneID2")
        
        # print(head(o1))
        # print(head(o2))
        # print(head(o3))
        # print(head(o4))
        # 


        edges = o3
        nodes = o4
        
        g <- graph.data.frame(edges, vertices = nodes, directed = TRUE)
        
        # print("l2START")
        
        l1 = layout_on_grid(g, dim = 2)

        if ((vcount(g) <= 2^11) & (ecount(g) >= 2^2) & (ecount(g) <= 2^11)) {
          l2 = layout_with_kk(g, coords = l1, dim = 2,
                              maxiter = 999 * vcount(g),
                              epsilon = 0, kkconst = vcount(g),
                              #weights = rep(100, length.out),
                              minx = NULL, maxx = NULL,
                              miny = NULL, maxy = NULL,
                              minz = NULL,maxz = NULL)
        } else {l2 = l1}
        z = round(ecount(g)/vcount(g))
        l2 = l2*2*z
        
        # print("l2DONE")
        
        nodes$x <- as.numeric(l2[,1])
        nodes$y <- as.numeric(l2[,2])
        o1$x <- as.numeric(l2[,1])
        o1$y <- as.numeric(l2[,2])
        o4$x <- as.numeric(l2[,1])
        o4$y <- as.numeric(l2[,2])
        edges$reactionType = 'unk'
        o2$reactionType = 'unk'
        o3$reactionType = 'unk'
        
        colnames(nodes) = colnames(o4) = c("name", "geneID", "shortName", "x", "y")
        colnames(edges) = colnames(o3) = c("from", "to", "geneID1", "geneID2", "reactionType")
        colnames(o2) = c("geneID1", "geneID2", "reactionType")
        colnames(o1) = c("geneID", "shortName", "x", "y")

        g <- graph.data.frame(edges, vertices = nodes, directed = TRUE)

        # print(summary(g))
        
        return(list(o1, o2, g))
        

      }
    
  }
})
  
  output$y441 <- DT::renderDataTable(DT::datatable({myoutput()[[1]]},
                                                  extensions = 'Buttons',
                                                  options = list(dom = 'Bfrtip',
                                                                 pageLength = 10,
                                                                 buttons = c('copy', 
                                                                             'csv', 
                                                                             'excel', 
                                                                             'pdf', 
                                                                             'print')), rownames = FALSE
  ), server = FALSE
  )
  
  output$y442 <- DT::renderDataTable(DT::datatable({myoutput()[[2]]},
                                                   extensions = 'Buttons',
                                                   options = list(dom = 'Bfrtip',
                                                                  pageLength = 10,
                                                                  buttons = c('copy', 
                                                                              'csv', 
                                                                              'excel', 
                                                                              'pdf', 
                                                                              'print')), rownames = FALSE
  ), server = FALSE
  )
  
  # jump to output
  observe({
    if( (!is.na(req(input$file2))[1]) & (!is.na(req(input$file1))[1]) & ntext() & (!is.null(myoutput()))){
      updateNavbarPage(session, inputId = 'tabselected', selected = "2")
    }
  })
  
  # jump to output
  observe({
    if( (!is.na(req(input$file2))[1]) & (!is.na(req(input$file1))[1]) & (!is.na(req(input$Xto))[1]) & (!is.na(req(input$Xfrom))[1]) & ntext2() & (!is.null(myoutput()))){
      updateNavbarPage(session, inputId = 'tabselected2', selected = "3")
    }
  })
  
  myLKNname1 <- reactive({
    if (is.null(input$caption1))
      return(NULL)
    return(input$caption1)
  })
  
  output$value1 <- renderText({ 
    return(paste0(myLKNname1(), '.graphml'))
  })
  
  

  output$igraphPlot1 <- renderPlot({
    
    if (is.null(myoutput()))
      return(NULL)
    
    g = isolate(myoutput())[[3]]
    

    autocurve.edges <- function(graph, start=0.5) {
      el <- apply(get.edgelist(graph, names = FALSE), 1, paste, collapse = ":")
      ave(rep(NA, length(el)), el, FUN = function(x) {
        if (length(x) == 1) {
          return(0)
        } else {
          return(seq(-start, start, length = length(x)))
        }
      })
    }
    
    if ((vcount(g) <= 2^12) & (ecount(g) >= 2^2) & (ecount(g) <= 2^13)) { ### max e
      plot(0, type = "n",
           axes = FALSE,
           xlim = extendrange(V(g)$x),
           ylim = extendrange(V(g)$y),
           xlab = '',
           ylab = '')
  
      plot(g, layout = cbind(V(g)$x, V(g)$y),
           edge.label = '',
           vertex.label = "", #V(g)$shortName,
           vertex.size = degree(g),
           vertex.color = 'white',
           rescale = FALSE, add = TRUE,
           vertex.label.cex = 0.75,
           edge.arrow.size = 0.25,
           edge.arrow.width = 0.25,
           edge.lty = 'solid',
           edge.color = 'gray',
           edge.width = 0.25,
           edge.label.cex = 0.25,
           edge.curved = autocurve.edges(g))
    }
    
    
  })
  
  
  output$downloadGraphmlT <- downloadHandler(
    
    
    filename = function() {
      paste(myLKNname1(), ".graphml", sep = "")
    },
    content = function(file) {
      write.graph(isolate(myoutput())[[3]], 
                  file, 
                  format = "graphml")
    }
  )


  ##### subnetwork from shortest paths between two nodes
  myfrom <- reactive({
    if (is.null(input$Xfrom))
      return(NULL)
    return(input$Xfrom)
  })

  output$geneIDfrom <- renderText({
    
    if (is.null(myoutput()))
      return(NULL)
    
    g = myoutput()[[3]]
    if((myfrom()) %in% V(g)$name) {
      # print(V(g)$name)
      return(paste0((myfrom())))
    } else {
      return(NULL)
        }
  })

  myto <- reactive({
    if (is.null(input$Xto))
      return(NULL)
    return(input$Xto)
  })

  output$geneIDto <- renderText({
    if (is.null(myoutput()))
      return(NULL)
    
    g = myoutput()[[3]]
    if((myto()) %in% V(g)$name) {
      # print(V(g)$name)
      return(paste0((myto())))
    } else {
      return(NULL)
    }
  })
  
  

  getCoordNEr2 <- reactive({
    
    if (is.null(myoutput()))
      return(NULL)

    nodes <- isolate(myoutput()[[1]])
    edges <- isolate(myoutput()[[2]])

    
    # print(head(nodes))
    # print(head(edges))
    # 

    
      gg <- isolate(myoutput()[[3]])
      # print(summary(gg))

      
      # save(gg, file = "gg.RData")

      nfrom = myfrom()
      nto = myto()
      
      # print(nfrom)
      # print(nto)
      
      if (((nfrom %in% edges$geneID1 | nfrom %in% edges$geneID2) & (nfrom %in% nodes$geneID)) &
          (nto %in% edges$geneID1 | nto %in% edges$geneID2) & (nto %in% nodes$geneID)) {
        
        # shortest_paths calculates a single shortest path 
        # (i.e. the path itself, not just its length) between the source vertex 
        # given in from, to the target vertices given in to. 
        sp = shortest_paths(gg, from = ((nfrom)), to = ((nto)),
                            mode = "all", weights = NULL,
                            output = "both",
                            predecessors = FALSE, inbound.edges = FALSE)
        
        # all_shortest_paths calculates all shortest paths between pairs of vertices
        spa = all_shortest_paths(graph = gg, 
                                 from = nfrom, 
                                 to = nto, 
                                 mode = "all",
                                 weights = NULL)
        nID = unlist(spa$res)
        
        # print(sp)
        
        # save(sp, file = "sp.RData")
        
        # g = induced_subgraph(gg, vids = sp$vpath[[1]])
        g = induced_subgraph(gg, vids = nID)
        # print(summary(g))
        
        # save(g, file = "g.RData")
        
        
        
        v = vertex_attr_names(g)
        e = edge_attr_names(g)
        
        # print(v)
        # print(e)
        
        df1 = matrix(NA, vcount(g), length(v))
        for (i in 1:length(v)) {
          df1[,i] = vertex_attr(g,v[i])
        }
        df1 = as.data.frame(df1, stringsAsFactors = FALSE)
        colnames(df1) = v
        
        
        df2 = matrix(NA, ecount(g), length(e))
        for (i in 1:length(e)) {
          df2[,i] = edge_attr(g,e[i])
        }
        df2 = as.data.frame(df2, stringsAsFactors = FALSE)
        colnames(df2) = e
        
        
        # print(head(df1))
        # print(head(df2))
        # print(summary(g))
        
        return(list(df1, df2, g))
        
      } 
})
    
    
    

  plotGraph <- function(g){
    

    # print(summary(g))
    
    # print("in function")
    # 
    # 
    # print("deciding layout")
    # print(input$PlotLayout)
    
    plotlayout <- switch(input$PlotLayout,
                         "Auto"=cbind(V(g)$x, V(g)$y),
                         "Random"=layout.random(g),
                         "Circle"=layout.circle(g),
                         "Sphere"=layout.sphere(g),
                         "Fruchterman Reingold"=layout.fruchterman.reingold(g),
                         "Kamada Kawai"=layout.kamada.kawai(g),
                         "Drl"=layout.drl(g),
                         "Spring"=layout.spring(g),
                         "Reingold Tilford"=layout.reingold.tilford(g),
                         "Lgl"=layout.lgl(g),
                         "Graphopt"=layout.graphopt(g)
    )
    
    # print(plotlayout)
    
    if(!input$showNodeName){
      V(g)$label = V(g)$geneID
    } else {
      V(g)$label = V(g)$shortName
      }
    if(!input$showEdgeName){
      E(g)$label = ""
    } else {
      E(g)$label = E(g)$reactionType
    }
    
    V(g)$size = input$vertexSize
    V(g)$color = rainbow(max(unique(degree(g, mode = "total"))))[degree(g, mode = "total")]
    # cat(V(g)$size, V(g)$color, V(g)$label, "\n")
    plot(g, layout = plotlayout)
    
    
  }
  
  calculateCentrality <- function(){

    if (is.null(getCoordNEr2())) {
      return(NULL)
      # print("bla")
    }

    if (!is.null(getCoordNEr2()) & !is.null(ntext2())) {
      g <- isolate(getCoordNEr2())[[3]]
      Centralities <- list()
      Centralities$nodeID = V(g)$name
      Centralities$Alpha <- as(igraph::alpha.centrality(g),"matrix")
      Centralities$Bon <- as(igraph::bonpow(g),"matrix")
      Centralities$Closeness <- as(igraph::closeness(g),"matrix")
      Centralities$Evcent <- as(igraph::evcent(g)$vector,"matrix")
      Centralities$Kleinberg <- as(igraph::authority.score(g)$vector,"matrix")
      Centralities$PageRank <- as(igraph::page.rank(g)$vector,"matrix")
      Centralities$Betweenness <- as(igraph::betweenness(g),"matrix")
      return(as.data.frame(Centralities))
    }
}
  
output$graphPlot <- renderPlot({
    if (is.null(getCoordNEr2())) {
      return(NULL)
      # print("bla")
    }
    if (!is.null(getCoordNEr2()) & !is.null(ntext2())) {
      g <- isolate(getCoordNEr2())[[3]]
      # print(summary(g))
      suppressWarnings(plotGraph(g))
    }
})
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste('NetworkGraph',format(Sys.time(),"%Y%m%d_%H%M%S"),'.pdf', sep='')
    },
    content = function(file) {
      pdf(file)
      print(suppressWarnings(plotGraph(g)))
      dev.off()
    }
  )
  
  output$Centralities <- renderTable(
    as(calculateCentrality(),"matrix")
    )
  

}

