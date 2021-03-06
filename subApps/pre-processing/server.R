

library(shiny)

if (!require("knitr")) install.packages("knitr")
library(knitr)
if (!require("markdown")) install.packages("markdown")
library(markdown)
if (!require("rmarkdown")) install.packages("rmarkdown")
library(rmarkdown)

if (!require("igraph")) install.packages("igraph")
library(igraph)


# By default, Shiny limits file uploads to 5MB per file. 
# 100 MB limit
options(shiny.maxRequestSize=100*1024^2) # 1024*1024*100

set.seed(123456)
'%ni%' = Negate('%in%') 


getCoordNEr <- list()



nodes1 <- reactiveValues()
edges1 <- reactiveValues()
nodesNEW <- reactiveValues()
edgesNEW <- reactiveValues()
myLKNname1 <- reactiveValues()

nodesNEW2 <- reactiveValues()
edgesNEW2 <- reactiveValues()
myLKNname2 <- reactiveValues()
graphml1 <- reactiveValues()

nodesNEW3 <- reactiveValues()
edgesNEW3 <- reactiveValues()
myLKNname3 <- reactiveValues()
xgmml1 <- reactiveValues()

function(input, output, session) {
  
  output$text0 <- renderUI({
    
    str0 <- paste("</br><b><a href='https://NIB-SI.shinyapps.io/DiNAR'>DiNAR</a> input data preprocessing</b></br>",
                  "From <b>nodes/edges tables, .graphml, .graphml & .xgmml</b> - to Tab Separated Values (TSV) format with obligatory colums for <a href='https://NIB-SI.shinyapps.io/DiNAR'>DiNAR</a></br></br>",
                  "<b><u><i>Node attributes/column names:</i></u></b>",
                  "- geneID (obligatory case sensitive values),", 
                  "- shortName (string or '-'),",
                  "- shortDescription (string or '-') and ",
                  "- MapManBin (XX.Y.Z_Bin Description | aa.b.c.d.e.f_Another Bin Description style [use ' | ' as separator for multiple bins and '_' for concatenation of bin with its description] or '-')</br>",
                  "<style>
                    table {
                      font-family: arial, sans-serif;
                      border-collapse: collapse;
                      max-width: 1030px;
                    }
                  td, th {
                    border: 1px solid #dddddd;
                    text-align: left;
                    padding: 8px;
                  }
                  </style>
                  <table>
                  <tr style='background-color: #dddddd;'>
                  <th>geneID</th>
                  <th>shortName</th>
                  <th>shortDescription</th>
                  <th>MapManBin</th>
                  </tr>
                  <tr>
                  <td>AT3G48090</td>
                  <td><div>EDS1</div></td>
                  <td>Enhanced disease susceptibility 1; Positive regulator of basal resistance and of effector- triggered immunity specifically mediated by TIR-NB-LRR (TNL) resistance proteins.</td>
                  <td><div>16.4.2_secondary metabolism.N misc.betaine | 17.3.1.1.1_hormone metabolism.brassinosteroid.synthesis-degradation.BRs.DET2 | 20.1.3_stress.biotic.signalling</div></td>
                  </tr>
                  <tr>
                  <td>AT3G52430</td>
                  <td>PAD4</td>
                  <td>PHYTOALEXIN DEFICIENT 4; Probable lipase required downstream of MPK4 for accumulation of the plant defense-potentiating molecule, salicylic acid, thus contributing to the plant innate immunity against invasive biotrophic pathogens and to defense mechanisms upon recognition of microbe-associated molecular patterns (MAMPs).</td>
                  <td>20.1.3_stress.biotic.signalling</td>
                  </tr>
                  <tr>
                  <td>AT5G44420</td>
                  <td>PDF1.2</td>
                  <td>Plant defensin 1.2; Confers broad-spectrum resistance to pathogens. Has antifungal activity in vitro.</td>
                  <td>20.1.7.12_stress.biotic.PR-proteins.PR12 (plant defensins)</td>
                  </tr>
                  <tr>
                  <td>AT5G47220</td>
                  <td>ERF2</td>
                  <td>ethylene responsive element binding factor 2</td>
                  <td>17.5.2_hormone metabolism.ethylene.signal transduction | 20.1.5_stress.biotic.regulation of transcription | 27.3.3_RNA.regulation of transcription.AP2/EREBP, APETALA2/ethylene-responsive element binding protein family</td>
                  </tr>
                  </table> ",
                  "<b><u><i>Edge attributes/column names:</i></u></b>",
                  "- geneID1 (obligatory case sensitive values),",
                  "- geneID2 (obligatory case sensitive values),", 
                  "- reactionType (string or '-')</br>",
                  "<table>
                    <tr style='background-color: #dddddd;'>
                    <th>geneID1</th>
                    <th>geneID2</th>
                    <th>reactionType</th>
                    </tr>
                    <tr>
                    <td>AT3G52430</td>
                    <td>AT3G48090</td>
                    <td>binding</td>
                    </tr>
                    <tr>
                    <td>AT5G44420</td>
                    <td>AT3G52430</td>
                    <td>-</td>
                    </tr>
                    <tr>
                    <td>AT5G47220</td>
                    <td>AT5G44420</td>
                    <td>act_TF</td>
                    </tr>
                    </table></br>",
                  "&rArr; use <b> tables </b> to get DiNAR obligatory columns, including dedicated x and y coordinates, from your nodes/edges tables",
                  "&rArr; use <b>.graphml </b> to get DiNAR obligatory columns, including dedicated x and y coordinates, from your .graphml file (e.g. export from Cytoscape 3.6.0 or yED)",
                  "&rArr; use <b>.graphml & .xgmml </b> to get DiNAR obligatory columns while keeping x and y coordinates, from your .graphml and .xgmml files (e.g. exports from Cytoscape 3.6.0)</br>",
                  "<b>Note:</b></br>&#9479;could be time-consuming for large networks (more than 2<sup>11</sup> nodes/edges)</br>&#9479;large networks are not plotted</br>&#9479;large networks recommendation: use <b>.graphml & .xgmml</b> or <a href='https://nib-si.shinyapps.io/clustering/'>https://nib-si.shinyapps.io/clustering/</a></br>",
                  "<b>More information at: </b> <a href='https://github.com/NIB-SI/DiNAR/tree/master/subApps'>https://github.com/NIB-SI/DiNAR/tree/master/subApps</a></br>",
                  sep = "</br>"
    )
    str1 <- paste0('</br>')
    
    HTML(paste(str0, str1,sep = '<br/>'))
  })
  
  nodes1 <- reactive({
    
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    title.line <- readLines(paste0(inFile$datapath), n = 1)
    title.line = strsplit(title.line, "\t")[[1]]
    
    nodes <- read.table(inFile$datapath, skip = 1, 
                        stringsAsFactors = FALSE, 
                        header = FALSE, sep = input$sep, col.names = title.line,
                        quote = "", comment.char = "")
    
    return(nodes)
    
  })
  
  edges1 <- reactive({
    
    
    inFile <- input$file2
    
    if (is.null(inFile))
      return(NULL)
    
    title.line <- readLines(paste0(inFile$datapath), n = 1)
    title.line = strsplit(title.line, "\t")[[1]]
    
    edges <- read.table(inFile$datapath, skip = 1, 
                        stringsAsFactors = FALSE, 
                        header = FALSE, sep = input$sep, col.names = title.line,
                        quote = "", comment.char = "")
    
    
    return(edges)
  })
  
  myLKNname1 <- reactive({
    if (is.null(input$caption1))
      return(NULL)
    return(input$caption1)
  })
  
  output$value1 <- renderText({ 
    return(paste0(myLKNname1(), '.graphml'))
  })
  
  
  getCoordNEr <- reactive({
    
    
    if (is.null(nodes1()))
      return(NULL)
    if (is.null(edges1()))
      return(NULL)
    
    nodes <- isolate(nodes1())
    edges <- isolate(edges1())
    
    # source('coordFromTables.R', local = TRUE)
    
    
    getCoordNE <- function(nodes, edges){
      nodes = nodes
      edges = edges
      
      
      edges$exists = rep(1, nrow(edges))
      
      if (length(edges$geneID1) & length(edges$geneID2)) {
        edges$from = edges$geneID1
        edges$to = edges$geneID2
      } else if (length(edges$from) & length(edges$to)) {
        edges$geneID1 = edges$from
        edges$geneID2 = edges$to
      }
      
      
      nodes$expressed = rep(1, nrow(nodes))
      
      if (length(nodes$geneID)) {
        nodes$name =  nodes$geneID
      } else if (length(nodes$name)) {
        nodes$geneID =  nodes$name
      } else if (length(nodes$nodeID)) {
        nodes$geneID =  nodes$nodeID
      }
      
      
      edges$to = toupper(edges$to)
      edges$from = toupper(edges$from)
      nodes$name = toupper(nodes$name)
      
      
      colNames = toupper(c("from", "to", 
                           "geneID1", "geneID2", "reactionType",
                           "exists"))
      e = colnames(edges)
      importantColsE = unlist(sapply(colNames,
                                     function(x) grep(paste("^",x,"$", sep = ""),
                                                      toupper((e)))))
      
      edges = edges[, importantColsE]
      
      colNames = toupper(c("name", "nodeID", 
                           "geneID", "shortName", "shortDescription", "MapManBin",
                           "expressed"))
      v = colnames(nodes)
      importantColsE = unlist(sapply(colNames,
                                     function(x) grep(paste("^",x,"$", sep = ""),
                                                      toupper((v)))))
      
      nodes = nodes[, importantColsE]
      
      
      g <- graph.data.frame(edges, vertices = nodes, directed = TRUE)
      
      # print("l2START")
      
      l1 = layout_on_grid(g, dim = 2)
      
      if ((vcount(g) <= 2^11) & (ecount(g) >= 2^2) & (ecount(g) <= 2^14)) {
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
      
      V(g)$clusterID <- rep(1, vcount(g))
      V(g)$x <- as.numeric(l2[,1])
      V(g)$y <- as.numeric(l2[,2])
      mydeg = igraph::degree(g, loops = FALSE, normalized = FALSE,  mode = "all")
      V(g)$clusterSimplifiedNodeDegree <- mydeg
      
      E(g)$clusterID_geneID1 = V(g)$clusterID[match(E(g)$geneID1,  V(g)$geneID)]
      E(g)$clusterID_geneID2 = V(g)$clusterID[match(E(g)$geneID2,  V(g)$geneID)]
      E(g)$clusterSimplifiedNodeDegree_geneID1 = V(g)$clusterSimplifiedNodeDegree[match(E(g)$geneID1,  V(g)$geneID)]
      E(g)$clusterSimplifiedNodeDegree_geneID2 = V(g)$clusterSimplifiedNodeDegree[match(E(g)$geneID2,  V(g)$geneID)]
      
      
      v = vertex_attr_names(g) # 2021-02-24
      e = edge_attr_names(g)
      
      df1 = matrix(NA, vcount(g), length(v))
      for (i in 1:length(v)) {
        df1[,i] = vertex_attr(g,v[i])
      }
      df1 = as.data.frame(df1, stringsAsFactors = FALSE)
      colnames(df1) = v
      
      
      colNames = toupper(c("geneID", "shortDescription", "shortName", "MapManBin",
                           "clusterID", "x", "y", "clusterSimplifiedNodeDegree", "expressed"))
      
      importantColsE = unlist(sapply(colNames,
                                     function(x) grep(paste("^",x,"$", sep = ""),
                                                      toupper((v)))))
      
      df1 = df1[,importantColsE]
      
      df2 = matrix(NA, ecount(g), length(e))
      for (i in 1:length(e)) {
        df2[,i] = edge_attr(g,e[i])
      }
      df2 = as.data.frame(df2, stringsAsFactors = FALSE)
      colnames(df2) = e
      
      
      colNames = toupper(c("geneID1", "geneID2", "reactionType",
                           "clusterID_geneID1", "clusterID_geneID2",
                           "clusterSimplifiedNodeDegree_geneID1", "clusterSimplifiedNodeDegree_geneID2",
                           "exists"
      ))
      importantColsE = unlist(sapply(colNames,
                                     function(x) grep(paste("^",x,"$", sep = ""),
                                                      toupper((e)))))
      df2 = df2[,importantColsE]
      
      return(list(df1, df2, g))
      
    }
    
    
    
    mylist = getCoordNE(nodes, edges)
    
    # print(length(mylist))
    return(mylist)
    
    
    
    
    
  })
  
  
  output$nodesNEW <- DT::renderDataTable({
    
    if (is.null(getCoordNEr())) return(NULL)
    
    # nodes <- isolate(nodes1())
    # edges <- isolate(edges1())
    # 
    # if (is.null(nodes))
    #   return(NULL)
    # if (is.null(edges))
    #   return(NULL)
    
    
    nodes2 = isolate(getCoordNEr())[[1]]
    return(nodes2)
    
  })
  
  
  output$downloadNodesT <- downloadHandler(
    
    
    filename = function() {
      paste(myLKNname1(), ".txt", sep = "")
    },
    content = function(file) {
      write.table(isolate(getCoordNEr())[[1]], file, 
                  row.names = FALSE,
                  append = FALSE, quote = FALSE, sep = "\t",
                  eol = "\n", na = "NA", dec = ".")
    }
  )
  
  
  output$edgesNEW <- DT::renderDataTable({
    
    if (is.null(getCoordNEr())) return(NULL)
    
    # nodes <- isolate(nodes1())
    # edges <- isolate(edges1())
    # 
    # if (is.null(nodes))
    #   return(NULL)
    # if (is.null(edges))
    #   return(NULL)
    
    edges2 = isolate(getCoordNEr())[[2]]
    return(edges2)
    
  })
  
  output$downloadEdgesT <- downloadHandler(
    
    
    filename = function() {
      paste(myLKNname1(), ".txt", sep = "")
    },
    content = function(file) {
      write.table(isolate(getCoordNEr())[[2]], file, 
                  row.names = FALSE,
                  append = FALSE, quote = FALSE, sep = "\t",
                  eol = "\n", na = "NA", dec = ".")
    }
  )
  
  output$igraphPlot1 <- renderPlot({
    
    if (is.null(getCoordNEr()))
      return(NULL)
    
    g = isolate(getCoordNEr())[[3]]
    
    
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
    
    if ((vcount(g) <= 2^11) & (ecount(g) >= 2^2) & (ecount(g) <= 2^14)) {
      plot(0, type = "n",
           axes = FALSE,
           xlim = extendrange(V(g)$x),
           ylim = extendrange(V(g)$y),
           xlab = '',
           ylab = '')
      
      plot(g, layout = cbind(V(g)$x, V(g)$y),
           edge.label = '',
           vertex.label = V(g)$shortName,
           # vertex.size = rep(1, vcount(g)),
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
      paste(myLKNname1(), ".txt", sep = "")
    },
    content = function(file) {
      write.graph(isolate(getCoordNEr())[[3]], 
                  file, 
                  format = "graphml")
    }
  )
  
  # observeEvent(input$do, {
  #   # source("01_createGraphML.R")
  #   # render("02_multilevel_and_spinglass_clustering.Rmd", "all")
  #   
  # 
  #   cat("\n\n", "DONE!","\n\n", "You may close the app and check the results.")
  # })
  # 
  
  graphml1 <- reactive({
    
    
    inFile <- input$file3
    
    if (is.null(inFile))
      return(NULL)
    
    
    gg = read.graph(file = inFile$datapath, 
                    format = "graphml")
    
    # summary(gg)
    
    
    l1 = layout_on_grid(gg, dim = 2)
    
    if ((vcount(gg) <= 2^11) & (ecount(gg) >= 2^2) & (ecount(gg) <= 2^14)) {
      l2 = layout_with_kk(gg, coords = l1, dim = 2,
                          maxiter = 999 * vcount(gg),
                          epsilon = 0, kkconst = vcount(gg),
                          #weights = rep(100, length.out),
                          minx = NULL, maxx = NULL,
                          miny = NULL, maxy = NULL,
                          minz = NULL,maxz = NULL)
    } else {l2 = l1}
    z = round(ecount(gg)/vcount(gg))
    l2 = l2*2*z
    
    V(gg)$clusterID <- rep(1, vcount(gg))
    V(gg)$x <- as.numeric(l2[,1])
    V(gg)$y <- as.numeric(l2[,2])
    mydeg = igraph::degree(gg, loops = FALSE, normalized = FALSE,  mode = "all")
    V(gg)$clusterSimplifiedNodeDegree <- mydeg
    V(gg)$expressed = rep(1, vcount(gg))
    
    E(gg)$clusterID_geneID1 = V(gg)$clusterID[match(E(gg)$geneID1,  V(gg)$geneID)]
    E(gg)$clusterID_geneID2 = V(gg)$clusterID[match(E(gg)$geneID2,  V(gg)$geneID)]
    E(gg)$clusterSimplifiedNodeDegree_geneID1 = V(gg)$clusterSimplifiedNodeDegree[match(E(gg)$geneID1,  V(gg)$geneID)]
    E(gg)$clusterSimplifiedNodeDegree_geneID2 = V(gg)$clusterSimplifiedNodeDegree[match(E(gg)$geneID2,  V(gg)$geneID)]
    E(gg)$exists = rep(1, ecount(gg))
    
    return(gg)
    
  })
  
  myLKNname2 <- reactive({
    if (is.null(input$caption2))
      return(NULL)
    return(input$caption2)
  })
  
  output$value2 <- renderText({ 
    return(paste0(myLKNname2()))
  })
  
  output$igraphPlot2 <- renderPlot({
    
    if (is.null(graphml1()))
      return(NULL)
    
    g = isolate(graphml1())
    
    
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
    
    if ((vcount(g) <= 2^11) & (ecount(g) >= 2^2) & (ecount(g) <= 2^14)) {
      plot(0, type = "n",
           axes = FALSE,
           xlim = extendrange(V(g)$x),
           ylim = extendrange(V(g)$y),
           xlab = '',
           ylab = '')
      
      plot(g, layout = cbind(V(g)$x, V(g)$y),
           edge.label = '',
           vertex.label = V(g)$shortName,
           # vertex.size = rep(1, vcount(g)),
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
  
  edgesNEW2 <- reactive({
    
    if (is.null(graphml1())) return(NULL)
    
    
    gg = isolate(graphml1())
    
    e = edge_attr_names(gg)
    
    df2 = matrix(NA, ecount(gg), length(e))
    for (i in 1:length(e)) {
      df2[,i] = edge_attr(gg,e[i])
    }
    
    df2 = as.data.frame(df2, stringsAsFactors = FALSE)
    colnames(df2) = e
    
    colNames = toupper(c("geneID1", 
                         "geneID2", 
                         "reactionType", 
                         "clusterID_geneID1", 
                         "clusterID_geneID2", 
                         "clusterSimplifiedNodeDegree_geneID1", 
                         "clusterSimplifiedNodeDegree_geneID2", 
                         "exists"
    ))
    importantColsE = unlist(sapply(colNames,
                                   function(x) grep(paste("^",x,"$", sep = ""),
                                                    toupper((e)))))
    df2 = df2[,importantColsE]
    
    return(df2)
    
    
  })
  
  output$edgesNEW2 <- DT::renderDataTable({
    return(edgesNEW2())
  })
  
  nodesNEW2 <- reactive({
    
    if (is.null(graphml1())) return(NULL)
    
    
    gg = isolate(graphml1())
    
    v = vertex_attr_names(gg)
    
    df1 = matrix(NA, vcount(gg), length(v))
    for (i in 1:length(v)) {
      df1[,i] = vertex_attr(gg,v[i])
    }
    df1 = as.data.frame(df1, stringsAsFactors = FALSE)
    colnames(df1) = v
    
    colNames = toupper(c("geneID", 
                         "shortDescription", 
                         "shortName", 
                         "MapManBin", 
                         "clusterID", 
                         "x", 
                         "y", 
                         "clusterSimplifiedNodeDegree",
                         "expressed"))
    
    importantColsE = unlist(sapply(colNames,
                                   function(x) grep(paste("^",x,"$", sep = ""),
                                                    toupper((v)))))
    
    df1 = df1[,importantColsE]
    
    return(df1)
    
    
  })
  
  output$nodesNEW2 <- DT::renderDataTable({
    return(nodesNEW2())
  })
  
  
  output$downloadEdgesG <- downloadHandler(
    
    
    filename = function() {
      paste(myLKNname2(), ".txt", sep = "")
    },
    content = function(file) {
      write.table(isolate(edgesNEW2()), file, 
                  row.names = FALSE,
                  append = FALSE, quote = FALSE, sep = "\t",
                  eol = "\n", na = "NA", dec = ".")
    }
  )
  
  
  output$downloadNodesG <- downloadHandler(
    
    
    filename = function() {
      paste(myLKNname2(), ".txt", sep = "")
    },
    content = function(file) {
      write.table(isolate(nodesNEW2()), file, 
                  row.names = FALSE,
                  append = FALSE, quote = FALSE, sep = "\t",
                  eol = "\n", na = "NA", dec = ".")
    }
  )
  
  
  
  xgmml1 <- reactive({
    
    
    inFile <- input$file4
    
    if (is.null(inFile))
      return(NULL)
    
    inFileX <- input$file5
    
    if (is.null(inFileX))
      return(NULL)
    
    
    gg = igraph::read.graph(file = inFile$datapath, 
                    format = "graphml")
    
    myXgmml = readLines(inFileX$datapath)
    
    ind1 = grep('<node', myXgmml)
    ind2 = grep('graphics h=', myXgmml)
    myIDs = myXgmml[ind1]
    myXY = myXgmml[ind2]
    
    myIDs = unlist(sapply(1:length(myIDs), 
                          function(i) {
                            if (grep('label', myIDs[i])) {
                              myIDs[i] = gsub("[^A-Za-z0-9(/):,.+_-]", " ", myIDs[i])
                              str = strsplit(myIDs[i], 'label')
                              myIDs[i] = gsub(' ', '', str[[1]][2])
                              strsplit(myIDs[i], "label=")  
                            }
                          }
    )
    )
    
    myX = unlist(sapply(1:length(myXY), 
                        function(i) {
                          if (grep('x=', myXY[i])) {
                            str = strsplit(myXY[i], ' ')[[1]]
                            strx = str[grep('x=', str)]
                            myXY[i] = gsub("[^x0-9.=-]", " ", strx)
                            str = strsplit(myXY[i], 'x=')
                            myXY[i] = gsub(' ', '', str[[1]][2])
                          }
                        }
    )
    )
    
    myXY = myXgmml[ind2]
    myY = unlist(sapply(1:length(myXY), 
                        function(i) {
                          if (grep('y=', myXY[i])) {
                            str = strsplit(myXY[i], ' ')[[1]]
                            stry = str[grep('y=', str)]
                            myXY[i] = gsub("[^y0-9.=-]", " ", stry)
                            str = strsplit(myXY[i], 'y=')
                            myXY[i] = gsub(' ', '', str[[1]][2])
                          }
                        }
    )
    )
    
    if (! 'geneID' %in% igraph::vertex_attr_names(gg)) V(gg)$geneID = V(gg)$name
    ind = match(gsub(' ', '', V(gg)$geneID), myIDs)
    l2 = (cbind(as.numeric(myX[ind]), (-1)*as.numeric(myY[ind])))
    
    
    
    
    V(gg)$clusterID <- rep(1, vcount(gg))
    V(gg)$x <- as.numeric(l2[,1])
    V(gg)$y <- as.numeric(l2[,2])
    mydeg = igraph::degree(gg, loops = FALSE, normalized = FALSE,  mode = "all")
    V(gg)$clusterSimplifiedNodeDegree <- mydeg
    V(gg)$expressed = rep(1, vcount(gg))
    
    E(gg)$clusterID_geneID1 = V(gg)$clusterID[match(E(gg)$geneID1,  V(gg)$geneID)]
    E(gg)$clusterID_geneID2 = V(gg)$clusterID[match(E(gg)$geneID2,  V(gg)$geneID)]
    E(gg)$clusterSimplifiedNodeDegree_geneID1 = V(gg)$clusterSimplifiedNodeDegree[match(E(gg)$geneID1,  V(gg)$geneID)]
    E(gg)$clusterSimplifiedNodeDegree_geneID2 = V(gg)$clusterSimplifiedNodeDegree[match(E(gg)$geneID2,  V(gg)$geneID)]
    E(gg)$exists = rep(1, ecount(gg))
    
    return(gg)
    
  })
  
  
  myLKNname3 <- reactive({
    if (is.null(input$caption3))
      return(NULL)
    return(input$caption3)
  })
  
  output$value3 <- renderText({ 
    return(paste0(myLKNname3()))
  })
  
  output$igraphPlot3 <- renderPlot({
    
    if (is.null(xgmml1()))
      return(NULL)
    
    g = isolate(xgmml1())
    
    
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
    
    if ((vcount(g) <= 2^11) & (ecount(g) >= 2^2) & (ecount(g) <= 2^14)) {
      plot(0, type = "n",
           axes = FALSE,
           xlim = extendrange(V(g)$x),
           ylim = extendrange(V(g)$y),
           xlab = '',
           ylab = '')
      
      plot(g, layout = cbind(V(g)$x, V(g)$y),
           edge.label = '',
           vertex.label = V(g)$shortName,
           # vertex.size = rep(1, vcount(g)),
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
  
  
  nodesNEW3 <- reactive({
    
    if (is.null(xgmml1())) return(NULL)
    
    
    gg = isolate(xgmml1())
    
    v = vertex_attr_names(gg)
    
    df1 = matrix(NA, vcount(gg), length(v))
    for (i in 1:length(v)) {
      df1[,i] = vertex_attr(gg,v[i])
    }
    df1 = as.data.frame(df1, stringsAsFactors = FALSE)
    colnames(df1) = v
    
    colNames = toupper(c("geneID", 
                         "shortDescription", 
                         "shortName", 
                         "MapManBin", 
                         "clusterID", 
                         "x", 
                         "y", 
                         "clusterSimplifiedNodeDegree",
                         "expressed"))
    
    importantColsE = unlist(sapply(colNames,
                                   function(x) grep(paste("^",x,"$", sep = ""),
                                                    toupper((v)))))
    
    df1 = df1[,importantColsE]
    
    return(df1)
    
    
  })
  
  output$nodesNEW3 <- DT::renderDataTable({
    return(nodesNEW3())
  })
  
  
  output$downloadNodesX <- downloadHandler(
    
    
    filename = function() {
      paste(myLKNname3(), ".txt", sep = "")
    },
    content = function(file) {
      write.table(isolate(nodesNEW3()), file, 
                  row.names = FALSE,
                  append = FALSE, quote = FALSE, sep = "\t",
                  eol = "\n", na = "NA", dec = ".")
    }
  )
  
  edgesNEW3 <- reactive({
    
    if (is.null(xgmml1())) return(NULL)
    
    
    gg = isolate(xgmml1())
    
    e = edge_attr_names(gg)
    
    df2 = matrix(NA, ecount(gg), length(e))
    for (i in 1:length(e)) {
      df2[,i] = edge_attr(gg,e[i])
    }
    
    df2 = as.data.frame(df2, stringsAsFactors = FALSE)
    colnames(df2) = e
    
    colNames = toupper(c("geneID1", 
                         "geneID2", 
                         "reactionType", 
                         "clusterID_geneID1", 
                         "clusterID_geneID2", 
                         "clusterSimplifiedNodeDegree_geneID1", 
                         "clusterSimplifiedNodeDegree_geneID2", 
                         "exists"
    ))
    importantColsE = unlist(sapply(colNames,
                                   function(x) grep(paste("^",x,"$", sep = ""),
                                                    toupper((e)))))
    df2 = df2[,importantColsE]
    
    return(df2)
    
    
  })
  
  output$edgesNEW3 <- DT::renderDataTable({
    return(edgesNEW3())
  })
  
  
  output$downloadEdgesX <- downloadHandler(
    
    
    filename = function() {
      paste(myLKNname3(), ".txt", sep = "")
    },
    content = function(file) {
      write.table(isolate(edgesNEW3()), file, 
                  row.names = FALSE,
                  append = FALSE, quote = FALSE, sep = "\t",
                  eol = "\n", na = "NA", dec = ".")
    }
  )
  
  
}

