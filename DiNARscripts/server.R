# server.R zagor

# By default, Shiny limits file uploads to 5MB per file. 
options(shiny.maxRequestSize=100*1024^2) # 1024*1024*100

# # # # global server variables: # # # # # # # # # # # # # # # # # # # # # # # #
#
# # # # # # # # # # # # # # # #  # # # # # # # # # # # # # # # # # # # # # # # #

# # # # list of ui variables: # # # # # # # # # # # # # # # # # # # # # # # # #
# Large Knowledge Network (LKN) or User defined Custom network: lkn
# Custom network nodes table: fileN
# Custom network edges table: fileE
# Experimental file, last uploaded: file1

# # # # # # # # # # # # # # # #  # # # # # # # # # # # # # # # # # # # # # # # #

# # # # list of server reactive variables: # # # # # # # # # # # # # # # # # # #
# filedata
# 
# # # # # # # # # # # # # # # #  # # # # # # # # # # # # # # # # # # # # # # # #

# # # # control points: # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# selectedLKN 
# UserNetwork 
# fileUploaded 
# 
# # # # # # # # # # # # # # # #  # # # # # # # # # # # # # # # # # # # # # # # #

shinyServer(
  
  function(input, output, session) {
    
    source("staticDETPPlotVis.R", local=TRUE)
    
    source("staticDETPPlotAB.R", local=TRUE)
    
    source("interactiveBCKGPlot.R", local=TRUE)
    
    source("dataPreparationForAnimatorAB.R", local=TRUE)
    source("dataPreparationForAnimatorVIS.R", local=TRUE)
    
    source("animatedPlotAB.R", local=TRUE)
    source("animatedPlotVis.R", local=TRUE)
    source("animatedPlotNDTV.R", local=TRUE)
    
    # # # # # # # # # # # # # #  # # # # # # # # # # # # # # # # # # # # # # # #
    ########  variables for session ######## 
    myFiles0 <- list()
    myFiles <- list()
    myFileNames <- list()
    duplicates <- vector()
    nodesNew <- reactiveValues()
    edgesNew <- reactiveValues()
    mypalette <- list()
    getNodes <- reactiveValues()
    getEdges <- reactiveValues()
    myIDcol <- reactiveValues()
    selectedPval <- reactiveValues()
    selectedLogFC <- reactiveValues()
    cutP <- reactiveValues()
    cutFC <- reactiveValues()
    tanim2 <- reactiveValues()
    animatoRNE <- list()
    maxTP <- reactiveValues()
    colMaxMin <- reactiveValues()
    NEWfLIST <- reactiveValues()
    maxCLU <- reactiveValues()
    ranges <- reactiveValues()
    VIScluster <- reactiveValues()
    tanimVis <- reactiveValues()
    whereAmI <- reactiveValues()
    myNDTVout <- reactiveValues()
    # # # # # # # # # # # # # #  # # # # # # # # # # # # # # # # # # # # # # # #
    
    
    output$text12 <- renderUI({
      
      str0 <- '<br>'
      str1 <- paste0('While this tab is active, one can change few parameters or upload forgotten files.<br/>')
      str2 <- paste0('More details in ')
      str3 <- paste0(tags$a(href="http://conferences.nib.si/DiNAR/", target = "_blank", HTML(paste0('<span style="color: #E5008B"><b>Quick help</b></span> (ReadMe)'))))
      str4 <- paste0('.</br>')
      str5 <- paste0("If you want to change the network, 'Reload' option is recommended; otherwise, menu reactivity is partially lost.")

      HTML(paste(str0, str1,str2,str3,str4,str5,sep = ''))
    })
    
    output$text13 <- renderUI({
      
      str0 <- '<br>'
      str1 <- paste0('<h1><i><b>Di</b>fferential <b>N</b>etwork <b>A</b>nalysis in <span style="color: #0000CC"><b>R</b></span></i><h1>')

      HTML(paste(str0, str1,sep = '<br/>'))
    })
    
    output$textHELP <- renderUI({
    HTML(paste0('<h2><span style="color: #E5008B"><b>Quick help</b></span> (ReadMe)</h2>')) 
    })
    output$textHELPwebm <- renderUI({
      HTML(paste0('<h2><b><span style="color: #C8E500">Quick tutorial</span></b> (WebM - video)</h2>')) 
    })
    
    output$AdditionalInformation <- renderUI({
      HTML(paste0('<h3><b><span style="color: #666699">Additional Information:</span></b></h3>')) 
    })
    output$GEODataAnalysis <- renderUI({
      HTML(paste0('<h4><span style="color: #6666A7"><p style="text-indent:20px;">Data Analysis from GEO</p></span></h4>')) 
    })
    output$subApps0 <- renderUI({
      HTML(paste0('<h4><span style="color: #6666A7"><p style="text-indent:20px;">subApps</p></span></h4>')) 
    })
    output$subApps1 <- renderUI({
      HTML(paste0('<h4><span style="color: #6666A7"><p style="text-indent:18px;">&ensp;&ensp;preprocessing at shinyapps</p></span></h4>')) 
    })
    output$subApps2 <- renderUI({
      HTML(paste0('<h4><span style="color: #6666A7"><p style="text-indent:18px;">&ensp;&ensp;clustering at shinyapps</p></span></h4>')) 
    })
    output$GoMapMan <- renderUI({
      HTML(paste0('<h4><span style="color: #6666A7"><p style="text-indent:20px;">www.gomapman.org</p></span></h4>')) 
    })
    
    
    output$text14 <- renderUI({
      
      str0 <- '<br>'
      str2 <- paste0('Examine your omics datasets in the prior knowledge context.<br>
                     Follow the steps as indicated in interactive menu.<br>
                     For the help overlay the mouse over the info button or go to Quick help section.<br>
                    Comprehensive knowledge network of <i><b>Arabidopsis thaliana</b></i> and <i><b>Solanum tuberosum</b></i> Immune Signalling are provided.
                     ')
      
      HTML(paste(str0, str2, sep = '<br/>'))
    })
    
    output$text15 <- renderUI({
      
      str0 <- '<br>'
      str1 <- paste0('* Select area and double click to zoom in.', "</br>", '* Use double click to zoom in/out.')
      
      HTML(paste(str0, str1,sep = '<br/>'))
    })
    
    output$text16 <- renderUI({
      
      # str1 <- paste0('\n', 
      #                '* Use navigation buttons or arrow keys to move the whole cluster.',
      #                "\n",
      #                '* Zoom in/out using mouse or +/- buttons.',
      #                "\n",
      #                "* Overlay mouse pointer over nodes or edges to get some aditional information.",
      #                "\n",
      #                "* Drag nodes and edges to wanted position/coordinates using mouse.",
      #                "\n",
      #                "* Select node and its neighbors by label using >>Select by id option<<.",
      #                "\n",
      #                "  ** Keyboard entry is enabled.",
      #                "\n",
      #                "  **  Node label is defined as combination of a short name (if available) and its expression value (or similar) in discrete time-point/condition.")
      str1 <- tags$ol(
                tags$ol('Use navigation buttons or arrow keys to move the whole cluster.'),
                tags$ol('Zoom in/out using mouse or +/- buttons.'),
                tags$ol("Overlay mouse pointer over nodes or edges to get some aditional information."),
                tags$ol("Drag nodes and edges to wanted position/coordinates using mouse."),
                tags$ol("Select node and its neighbors by label using >>Select by id option<<. Keyboard entry is enabled. Node label is defined as combination of a short name (if available) and its expression value (or similar) in discrete time-point/condition.")
              )
      HTML(paste(str1))
    })
    
    output$text17 <- renderUI({
      
      # str0 <- '<br>'
      # str1 <- paste0('* Drag nodes and edges to wanted position and zoom in/out using mouse.', 
      #                "</br>", 
      #                '* Overlay mouse pointer over nodes or edges to get some aditional information.',
      #                "</br>", 
      #                "** Node label is defined as combination of short name (if available) and its expression value (or similar).",
      #                "</br>", 
      #                "*** Node expression value (or similar) between time-points/conditions is calculated using homotopy.")
      # HTML(paste(str0, str1,sep = '<br/>'))
      str1 <- tags$ol(
        tags$ol('Use navigation buttons or arrow keys to move the whole cluster.'),
        tags$ol('Zoom in/out using mouse or +/- buttons.'),
        tags$ol("Overlay mouse pointer over nodes or edges to get some aditional information."),
        tags$ol("Drag nodes and edges to wanted position/coordinates using mouse."),
        tags$ol("Node label is visible in zoom mode and it is defined as combination of a short name (if available) and its expression value (or similar). Node expression value (or similar) between discrete time-points/conditions is calculated using homotopy.")
      )
      HTML(paste(str1))
    })
    
    output$wikiG <- renderUI({
      HTML(paste0('<span style="color: #666699"><b><h2>Glossary of graph theory terms</h2></b></span>(https://en.wikipedia.org/wiki/)')) 
    })
    
    output$text18 <- renderUI({
      
      str0 <- paste("<b>Graph</b> - The fundamental object of study in graph theory, a system of vertices connected in pairs by edges. Often subdivided into directed graphs or undirected graphs according to whether the edges have an orientation or not. Mixed graphs include both types of edges.",
                    "<b>Network</b> - A graph in which attributes (e.g. names) are associated with the nodes and/or edges.",
                    "<b>Node</b>  - A synonym for vertex.",
                    "<b>Vertex</b>  - A vertex (plural vertices) is (together with edges) one of the two basic units out of which graphs are constructed. Vertices of graphs are often considered to be atomic objects, with no internal structure.",
                    "<b>Edge</b> - An edge is (together with vertices) one of the two basic units out of which graphs are constructed. Each edge has two (or in hypergraphs, more) vertices to which it is attached, called its endpoints. Edges may be directed or undirected.",
                    "<b>Adjacent</b>  - The relation between two vertices that are both endpoints of the same edge.",
                    "<b>Loop</b>  - A loop or self-loop is an edge both of whose endpoints are the same vertex. These are not allowed in simple graphs.",
                    "<b>Degree</b> - The degree of a vertex in a graph is its number of incident edges. Degree is sometimes called valency. In a directed graph, one may distinguish the in-degree (number of incoming edges) and out-degree (number of outgoing edges).",
                    "<b>Label</b> - Information associated with a vertex or edge of a graph. A labeled graph is a graph whose vertices or edges have labels.",
                    "<b>Neighbour</b> - A vertex that is adjacent to a given vertex.",
                    "<b>A directed graph</b> is one in which the edges have a distinguished direction, from one vertex to another. In a <b>mixed graph</b>, a directed edge is again one that has a distinguished direction; directed edges may also be called arcs or arrows.",
                    "<b>A multigraph</b> is a graph that allows multiple adjacencies (and, often, self-loops); a graph that is not required to be simple.",
                    "<b>A subgraph</b> of a graph G is another graph formed from a subset of the vertices and edges of G.",
                    "<b>A topological graph</b> is a representation of the vertices and edges of a graph by points and curves in the plane (not necessarily avoiding crossings).",
                    sep = "</br>"
      )
      str1 <- paste0('</br>')
      
      HTML(paste(str0, str1,sep = '<br/>'))
    })
    
    output$text19 <- renderUI({
      
      str0 <- '<br>'
      str1 <- paste0('* Click-drag to zoom, shift-click to pan, double-click to autoscale.')
      
      HTML(paste(str0, str1,sep = '<br/>'))
    })
    
    output$textD3 <- renderUI({
      
      str0 <- paste0('Download web-based animation of a networkDynamic object.')
      str1 <- paste0('<b><span style="color: #CC0066;">Maximal cluster size on shinyapps.io: 1024 verices and 2048 edges.</span></b>')
      str2 <- paste0('Rendering duration depends on the number of vertices and edges, selected speed and number of conditions.')
      str3 <- paste0('When running DiNAR locally, maximal number of nodes and edges threshold could be modified in:')
      str4 <- paste0('<b><i>ndtvd3Threshold.R</b> script.</i></b>')
      HTML(paste(str0, str1, str2, str3, str4, sep = '<br/>'))
      
    })
    
    
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    TIP <- reactiveValues()
    
    # Network selection help
    observe({
      TIP$a <- paste('Select a background knowledge network: Arabidopsis thaliana Large Knowledge Network (LKN), Solanum tuberosum Immune Signalling (PIS) or Custom Network.', 
                     'In the case of Custom Network, user also needs to provide nodes and edges tables in proper format (see help page).',
                     sep = '<br>')
    })
    output$helpLKN <- renderUI({
      span("Select network", tipify(el = icon(name = "info-circle", lib = "font-awesome"), 
                                    title = TIP$a))
    })
    
    #### ####
    #### #### check whether user has selected the network
    output$selectedLKN <- reactive({
      if (input$lkn != '') {
        if (input$lkn != 'Custom network'){
          return(1)
        } else {
          infile1 <- input$fileN
          infile2 <- input$fileE
          if ((!(is.null(infile1))) & (!(is.null(infile2)))) {
            return(1)            
          } else return(-1)
        }
      } else {
        return(-1)
      }
    })
    outputOptions(output, 'selectedLKN', suspendWhenHidden=FALSE) # Disable suspend
    
    #### ####
    #### #### check whether user has selected the custom network
    output$UserNetwork <- reactive({
      if (input$lkn == "Custom network") return(1)
      else return(0)
    })
    outputOptions(output, 'UserNetwork', suspendWhenHidden=FALSE) 
    
    #### #### check whether user has selected the custom network
    output$TESTNetwork <- reactive({
      if (input$lkn == "QuickAppTest(Ath123)") return(1)
      else return(0)
    })
    outputOptions(output, 'TESTNetwork', suspendWhenHidden=FALSE) 
    
    # Nodes and edges tables upload help
    observe({
      TIP$b <- paste('Upload nodes table, formatted according to the tutorial example.', 
                     'Without the required columns the applications will not work properly!',
                     sep = '<br>')
    })
    output$helpUserNodes <- renderUI({
      span("Upload nodes table", tipify(el = icon(name = "info-circle", lib = "font-awesome"), 
                                        title = TIP$b))
    })
    observe({
      TIP$c <- paste('Upload edges table, formatted according to the tutorial example.', 
                     'Without the required columns the applications will not work properly!',
                     sep = '<br>')
    })
    output$helpUserEdges <- renderUI({
      span("Upload edges table", tipify(el = icon(name = "info-circle", lib = "font-awesome"), 
                                        title = TIP$c))
    })
    
    # Experimental data upload help
    observe({
      TIP$d <- paste('Upload your experimental data files.', 
                     'Files can be uploaded in multiple steps, one by one or all at once.',
                     "Selection and sort comes later (below).",
                     "Note: all files should be in the same format!",
                     sep = '<br>')
    })
    output$helpExperimentalDataUpload <- renderUI({
      span("Upload experimental data files", tipify(el = icon(name = "info-circle", lib = "font-awesome"), 
                                                    title = TIP$d))
      
    })
    
    #### ####
    #### #### check whether user did upload experimental files
    # http://stackoverflow.com/questions/29703413/how-to-return-a-list-of-files-uploaded-in-shiny
    # http://stackoverflow.com/questions/19686581/make-conditionalpanel-depend-on-files-uploaded-with-fileinput
    output$fileUploaded <- reactive({
      if (is.null(filedata())){
        return(0)
      } else {
        return(1)
      }
    })
    outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE) # , priority = 10) # Change priority
    
    # Experimental data file format help
    observe({
      TIP$e <- "Select if your files contain header (column names)."
    })
    output$helpHeader <- renderUI({
      span("Header?", tipify(el = icon(name = "info-circle", lib = "font-awesome"), 
                                          title = TIP$e))
    })
    observe({
      TIP$f <- "Is it comma, semicolon or tab separated text file format?"
    })
    output$helpSeparator <- renderUI({
      span("Select separator", tipify(el = icon(name = "info-circle", lib = "font-awesome"), 
                                      title = TIP$f))
    })
    
    observe({
      TIP$g <- "Which column, by column name or column order (in the case of the files without header), contains node/gene IDs?"
    })
    output$helpColID <- renderUI({
      span("Select column with gene IDs", tipify(el = icon(name = "info-circle", lib = "font-awesome"), 
                                      title = TIP$g))
    })

    observe({
      TIP$h <- paste("Which column, by column name or column order (in the case of the files without header), contains measure of statistical significance (required field) you want to apply in analysis?",
                     "Filtering by this column is optional; maximal value can be selected as a threshold.",
                     sep = '<br>')
    })
    output$helpColPval <- renderUI({
      span("Select column with measure of statistical significance", tipify(el = icon(name = "info-circle", lib = "font-awesome"), 
                                                 title = TIP$h))
    })


    observe({
      TIP$i <- paste("Which column, by column name or column order (in the case of the files without header), contains logFC-values or similar (required field) to set the additional cut-off threshold?",
                     'Absolute value (e.g. 0) can be selected as a threshold (optional field).,',
                     'Note: This column also dictates node/gene colour and size assignments.',
                     sep = '<br>')
    })
    output$helpCollogFC <- renderUI({
      span("Select column with logFCs or similar", tipify(el = icon(name = "info-circle", lib = "font-awesome"), 
                                                 title = TIP$i))
    })

    observe({
      TIP$j <- paste("Create personalised colour palette from three colours.",
                     "First selected lower boundary colour (e.g. for small/negative values), ",
                     "then intermediate-range colour and last upper boundary colour (e.g. for large values).",
                     "The resulting palette is visible in panel named DE pallette.",
                     'Nodes with absolute values higher than defined upper/lower boundary palette threshold will adopt marginal colours.',
                     "This is absolute value, and it can be changed.",
                     sep = '<br>')
    })
    output$helpPalette <- renderUI({
      span("Choose colours for custom palette", tipify(el = icon(name = "info-circle", lib = "font-awesome"), 
                                                          title = TIP$j))
    })
    output$helpPalette2 <- renderUI({
      str0 <- '<br>'
      str1 <- paste0('To confirm - click the <span style="color: #009900"><b>Proceed!</b></span> button')
      HTML(paste(str1, str0, sep = '<br/>'))
    })
    
    # Further selection and filtering help
    observe({
      TIP$k <- paste('Select cluster by node/gene ID.', 
                     'Case sensitive!',
                     'This is optional - mostly to discover in which cluster is your node/gene.',
                     sep = '<br>')
    })
    output$geneNameSelection<- renderUI({
      span("Search by gene ID", tipify(el = icon(name = "info-circle", lib = "font-awesome"), 
                                    title = TIP$k))
    })
    observe({
      TIP$l <- paste('Select cluster by cluster ID.', 
                     'Only nodes from selected cluster will be shown.',
                     'Edges connecting nodes within cluster and between clusters will be shown.',
                     sep = '<br>')
    })
    output$clusterIDselection <- renderUI({
      span(paste0("Search by cluster ID", ' [1:',maxCLU(), ']'), tipify(el = icon(name = "info-circle", lib = "font-awesome"), 
                      title = TIP$l))
    })
    observe({
      TIP$m <- paste('To extract subnetwork from cluster depending on the minimal degree of nodes within the cluster.', 
                     'Filter network cluster by minimal node degree threshold.',
                     'Observe changes in Background cluster and DE per cluster images.',
                     sep = '<br>')
    })
    output$minimalClusterDegreeSelection <- renderUI({
      span("Minimal node degree", tipify(el = icon(name = "info-circle", lib = "font-awesome"), 
                      title = TIP$m))
    })
    observe({
      TIP$n <- paste('For which condition/time point you would like to see static DE plot?', 
                     'According to the order you have uploaded the experimental files.',
                     sep = '<br>')
    })
    output$timePointSelection<- renderUI({
      span("Static Condition/Time point", tipify(el = icon(name = "info-circle", lib = "font-awesome"), 
                      title = TIP$n))
    })
    
    
    observe({
      TIP$o <- paste('Reload button', 
                     'reloads the DinaR application.',
                     'You start with a new session.',
                     'All current settings and data are lost.',
                      sep = '<br>')
    })
    output$RefBut<- renderUI({
      span("Reload", tipify(el = icon(name = "refresh", lib = "font-awesome"), 
                                          title = TIP$o))
    })

    observe({
      TIP$p <- paste('Here you can see in which time point/condition cluster expression is shown.', 
                     'It is also possible to stop and restart the animation.',
                     sep = '<br>')
    })
    output$animatoRtime<- renderUI({
      span("Dynamic Time/Condition", tipify(el = icon(name = "spinner", lib = "font-awesome"), 
                             title = TIP$p))
    })
    
    observe({
      TIP$r <- paste('Select files for further analysis in preferred order.', 
                     "Use left and right arrow keys for navigation.",
                     "Use Backspace or Delete key to remove file from the list.",
                     "Use mouse click or Enter to add file to the list.",
                     sep = '<br>')
    })
    output$selectFileshelp<- renderUI({
      span("Files selection and order", tipify(el = icon(name = "info-circle", lib = "font-awesome"), 
                                    title = TIP$r))
    })
    
    
    observe({
      TIP$s <- paste('Speed up or slow down animation.',
                     sep = '<br>')
    })
    output$stepSelection<- renderUI({
      span("Animation speed", tipify(el = icon(name = "info-circle", lib = "font-awesome"), 
                                                 title = TIP$s))
    })
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    
    
    
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    #### NETWORKS
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    
    # read user defined network files: nodes and edges tables
    nodesNew <- reactive({
      infile <- input$fileN
      if (is.null(infile)){
        return(NULL)
      }
      title.line <- readLines(infile$datapath, n=1)
      title.line = strsplit(title.line, "\t")[[1]]
      
      tmpNodes <- read.table(infile$datapath, skip=1,
                             stringsAsFactors=FALSE, header = FALSE,
                             sep = "\t", col.names = title.line,
                             quote="", comment.char="")
      return(tmpNodes)
    })
    
    edgesNew <- reactive({
      infile <- input$fileE
      if (is.null(infile)){
        return(NULL)
      }
      title.line <- readLines(infile$datapath, n=1)
      title.line = strsplit(title.line, "\t")[[1]]
      
      tmpEdges <- read.table(infile$datapath, skip=1,
                             stringsAsFactors=FALSE, header = FALSE,
                             sep = "\t", col.names = title.line,
                             quote="", comment.char="")
      return(tmpEdges)
    })
    
    # fetch nodes table accordint to the selected network
    getNodes <- reactive({
      if (input$lkn == "Arabidopsis thaliana") {
        return(nodesAth)
      }
      if (input$lkn == "Solanum tuberosum") {
        return(nodesSoTub)
      }
      if (input$lkn == "Custom network") {
        return(nodesNew())
      }
      if (input$lkn == "QuickAppTest(Ath123)") {
        return(AthNodesTEST)
      }
    })
    
    # fetch nodes table accordint to the selected network
    getEdges <- reactive({
      if (input$lkn == "Arabidopsis thaliana") {
        return(edgesAth)
      } 
      if (input$lkn == "Solanum tuberosum") {
        return(edgesSoTub)
      }
      if (input$lkn == "Custom network") {
        return(edgesNew())
      }
      if (input$lkn == "QuickAppTest(Ath123)") {
        return(AthEdgesTEST)
      }
    })
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    
    
    
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    #### EXPERIMENTAL DATA
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    
    ########  ########  ########  ########  ########  ########  ######## #######
    # read uploaded files
    filedata <- reactive({
      # no network
      if (input$lkn == '') {
        return(NULL)
      }
      
      if (input$lkn == "QuickAppTest(Ath123)") {
        nf = 3
        myFiles0[[length(myFiles0)+1]] <<- A.thalianaE.G.Condition1
        myFiles0[[length(myFiles0)+1]] <<- A.thalianaE.G.Condition2
        myFiles0[[length(myFiles0)+1]] <<- A.thalianaE.G.Condition3
        myFileNames[[length(myFileNames)+1]] <<- "A.thaliana:E.G.Condition1"
        myFileNames[[length(myFileNames)+1]] <<- "A.thaliana:E.G.Condition2"
        myFileNames[[length(myFileNames)+1]] <<- "A.thaliana:E.G.Condition3"
        
      } else {
        
        # no file
        infile <- input$file1
        if (is.null(infile)) {
          return(NULL)
        }
        
        # http://shiny.rstudio.com/articles/progress.html
        # Create a Progress object
        progress <- shiny::Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())
        
        progress$set(message = "Reading uploaded files", value = 0)
        
        # number offiles uploaded
        nf = length(basename(infile$datapath))
        
        funcReadFiles <- function(fin){
          myfile = tryCatch(read.csv(fin, header=input$header, sep=input$sep, 
                                     quote='"', 
                                     stringsAsFactors=FALSE),
                            error = function(e){
                              return(matrix('ERROR',1,1))
                            })
          # Increment the progress bar, and update the detail text.
          progress$inc(1/nf, detail = paste(" ", length(myFiles0)+1))
          
          # Pause for 0.1 seconds to simulate a long computation.
          Sys.sleep(0.1)
          
          myFiles0[[length(myFiles0)+1]] <<- myfile
          return(myfile)
        }
        
        funcReadFileNames <- function(fin){
          myFileNames[[length(myFileNames)+1]] <<- fin
        }
      
        if (nf > 1){
          fpath <- lapply(1:dim(infile)[1], function(x) input$file1[[x, 'datapath']])
          fname <- lapply(1:dim(infile)[1], function(x) input$file1[[x, 'name']])
          lapply(fpath, funcReadFiles)
          lapply(fname, funcReadFileNames)
        } else{
          fpath <- input$file1[[1, 'datapath']]
          fname <- input$file1[[1, 'name']]
          funcReadFiles(fpath)
          funcReadFileNames(fname)
        }
        
        }
      
      names(myFiles0) <<-  unlist(myFileNames)
      
      #assign(name, value)
      myFiles <<- myFiles0

      names(myFiles) <<-  unlist(myFileNames)
      return(myFiles[[length(myFiles)]])
    })
    
    output$listOfFiles <- renderDataTable({
      # no network
      if (input$lkn == '') {
        return(NULL)
      }
      # no file
      infile <- input$file1
      if ((is.null(infile)) & (input$lkn != "QuickAppTest(Ath123)")){
        return(NULL)
      }
      
      if ((length(myFiles0)) > 1) {
        dupMat = sapply(1:(length(myFiles0)), 
                        function(x) sapply(1:length(myFiles0), 
                                           function(y) identical(myFiles0[[x]],myFiles0[[y]])))
        dupMat = as.matrix(dupMat)
        rownames(dupMat) = unlist(myFileNames)
        colnames(dupMat) = unlist(myFileNames)
        
      } else {
        dupMat = FALSE
        dupMat = as.matrix(dupMat)
        names(dupMat) = (myFileNames)
      }
      
      diag(dupMat) = rep(FALSE, sqrt(length(dupMat)))
      sapply(1:sqrt(length(dupMat)), function(x) duplicates[[x]] <<- any(dupMat[,x]))
      duplicates <<- duplicates[!(is.na(duplicates))]
      
      tmp = seq(1,length(myFiles0),1)
      tmpTable = cbind(tmp, names(myFiles0), duplicates)
      colnames(tmpTable) = c('uploading order', 'filename', 'duplicate')
      return(tmpTable)
      
    })
    
    output$selectFiles <- renderUI({
      if (input$lkn == '') return(NULL)
      df <- filedata()
      if (is.null(df)) return(NULL)
      
      items=c('',unlist(myFileNames))
      names(items)=items
      selectInput(inputId = "selectFiles",
                  label = NULL, #"Select column used for filtering according to the cut-off value below.",
                  choices = items,
                  multiple = TRUE,
                  selected = sort(items),
                  width = '100%')#250px')
    })
    
    NEWfLIST <- reactive({ 
      if (input$lkn == '') return(NULL)
      df <- filedata()
      if (is.null(df)) return(NULL)
      return(input$selectFiles)
    })
    
    changeFileList <- reactive({
      # no network
      if (input$lkn == '') {
        return(NULL)
      }
      # no file
      infile <- input$file1
      if ((is.null(infile)) & (input$lkn != "QuickAppTest(Ath123)")){
        return(NULL)
      }
      if (length(input$selectFiles) < 1)  return(NULL)
      
      tmpfn = isolate(NEWfLIST())
      newOrder <- match(tmpfn, unlist(myFileNames))
      if (length(newOrder) == 1){
        myFiles <<-  myFiles0#[[newOrder]] #####################################
        names(myFiles) <<-  tmpfn
      } else {
        myFiles <<- lapply(newOrder, function(x) myFiles0[[x]])
        names(myFiles) <<-  unlist(tmpfn)
      }
      names(myFiles) <- tmpfn

    })
    
    output$listOfFilesToUse <- renderTable({
      # no network
      if (input$lkn == '') {
        return(NULL)
      }
      # no file
      infile <- input$file1
      if ((is.null(infile)) & (input$lkn != "QuickAppTest(Ath123)")){
        return(NULL)
      }
      # new file list
      nfl <- changeFileList()
      if (is.null(nfl)) return(NULL)

      tmpfn = isolate(NEWfLIST())
      oldOrder <- match(tmpfn, unlist(myFileNames))
      newOrder <- seq(1, length(oldOrder), 1)
      tmpTable = rbind(newOrder, oldOrder, tmpfn)
      if (length(tmpTable) > 2) {
        colnames(tmpTable) = seq(1, length(oldOrder), 1)
        rownames(tmpTable) = c('Animation order', 'uploading order', 'FileName')
      } else {
        names(tmpTable) = c('Animation order', 'uploading order', 'FileName')
      }
      
      # print(is.list(myFiles))
      # print(dim(myFiles) == dim(myFiles[[1]]))
      
      return(t(tmpTable))
    })
    
    ########  ########  ########  ########  ########  ########  ######## #######
    # which column, by column name, contains gene IDs?
    output$selectID <- renderUI({
      if (input$lkn == '') {
        return(NULL)
      }
      df <- filedata()
      if (is.null(df)) return(NULL)

      items=c('',names(df))
      names(items)=items
          selectInput(inputId = "selectID",
                      label = NULL,
                      choices = items, 
                      selected = ifelse(length(items[grep('ID',toupper(items))]), 
                                        items[grep('ID',toupper(items))][1], ''))
    })
    
    # which column, by column number, contains gene IDs?
    myIDcol <- reactive({ 
      if (input$lkn == '') return(NULL)
      if (input$selectID == '') return(NULL)
      df <- filedata()
      if (is.null(df)) return(NULL)

      tmpID <- which(colnames(df) == input$selectID)
      return(tmpID)
    })
    
    ########  ########  ########  ########  ########  ########  ######## #######
    # which column, by column name, contains p-values or similar to set the cut-off threshold?
    output$cutoff1 <- renderUI({
      if (input$lkn == '') return(NULL)
      df <- filedata()
      if (is.null(df)) return(NULL)

      items=c('',names(df))
      names(items)=items
      selectInput(inputId = "cutoff1",
                  label = NULL, #"Select column used for filtering according to the cut-off value below.",
                  choices = items,
                  multiple=FALSE,
                  selected = ifelse(length(items[grep('ADJ.P',toupper(items))]), 
                                    items[grep('ADJ.P',toupper(items))][1], ''))
    })
    
    # which column, by column number, contains p-values or similar to set the cut-off threshold?
    selectedPval <- reactive({ 
      if (input$lkn == '') return(NULL)
      df <- filedata()
      if (is.null(df)) return(NULL)
      if ((input$cutoff1 == '')) return(NULL)

      tmpID <- which(colnames(df) == input$cutoff1)
      return(tmpID)
    })
    
    output$cutoffval1 <- renderUI({
      if (input$lkn == '') return(NULL)
      df <- filedata()
      if (is.null(df)) return(NULL)

      tmp=(sapply(1:length(myFiles), 
                  function(x) sapply(1:dim(myFiles[[x]])[2], 
                                     function(y) if(is.numeric(myFiles[[x]][,y])){(max(myFiles[[x]][,y]))})))
      maksimus = max(unlist(tmp[,1]))
      numericInput("cutoffval1", "cut-off value (e.g. p-val):", 0.05, min = 0, max = maksimus)
    })
    
    # define cut-off threshold for e.g. adjusted p-values
    cutP <- reactive({ 
      if (input$lkn == '') return(NULL)
      if (input$cutoff1 == '') return(NULL)
      df <- filedata()
      if (is.null(df)) return(NULL)

      return(input$cutoffval1)
    })
    
    ########  ########  ########  ########  ########  ########  ########
    # which column, by column name, contains logFC-values or similar to set the additional cut-off threshold?
    output$cutoff2 <- renderUI({
      if (input$lkn == '') return(NULL)
      df <- filedata()
      if (is.null(df)) return(NULL)

      items=c('',names(df))
      names(items)=items
      selectInput(inputId = "cutoff2",
                  label = NULL, # "Select column used for additional filtering and colour assignment according to the cut-off value below.",
                  choices = items,
                  multiple=FALSE,
                  selected = ifelse(length(items[grep('FC',toupper(items))]), 
                                    items[grep('FC',toupper(items))][1], ''))
    })
    
    # which column, by column number, contains logFC-values or similar to set the additional cut-off threshold?
    selectedLogFC <- reactive({ 
      if (input$lkn == '') return(NULL)
      if (input$cutoff2 == '') return(NULL)
      df <- filedata()
      if (is.null(df)) return(NULL)

      tmpID <- which(colnames(df) == input$cutoff2)
      return(tmpID)
    })
    
    output$cutoffval2 <- renderUI({
      if (input$lkn == '') return(NULL)
      df <- filedata()
      if (is.null(df)) return(NULL)

      tmp=(sapply(1:length(myFiles),
                  function(x) sapply(1:dim(myFiles[[x]])[2],
                                     function(y) if(is.numeric(myFiles[[x]][,y])){(max(myFiles[[x]][,y]))})))
      maksimus = max(unlist(tmp[,1]))
      numericInput("cutoffval2", "additional cut-off, absolute value (e.g. logFC):", 
                   0, 
                   min = 0, 
                   max = maksimus)
    })
    
    #  
    cutFC <- reactive({ 
      if (input$lkn == '') return(NULL)
      if (input$cutoff2 == '') return(NULL)
      df <- filedata()
      if (is.null(df)) return(NULL)

      return(input$cutoffval2)
    })
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    
    
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    #### USER COLOUR PALETTE
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    
    ########  ########  ########  ########  ########  ########  ########
    # create colour palette from three selected colours
    # first colour
    observeEvent(input$col1, {
      colourpicker::updateColourInput(session, inputId = "col1", label = NULL,
                        value = input$text, showColour = input$showColour,
                        allowTransparent = input$allowTransparent,
                        palette = input$palette,
                        returnName = input$returnName)
    })
    # second colour
    observeEvent(input$col2, {
      colourpicker::updateColourInput(session, "col2",
                        value = input$text, showColour = input$showColour,
                        allowTransparent = input$allowTransparent,
                        palette = input$palette,
                        returnName = input$returnName)
    })
    # third colour
    observeEvent(input$col3, {
      colourpicker::updateColourInput(session = session, 
                        inputId = "col3",
                        label = NULL,
                        value = input$text, showColour = input$showColour,
                        allowTransparent = input$allowTransparent,
                        palette = input$palette,
                        returnName = input$returnName)
    })
    
    # create palette
    mypalette <- reactive({
      if (is.null(input$createPalette)) return(NULL)
      
      selectedLogFC1 = isolate(selectedLogFC())
      if  (is.null(dim(myFiles))) {
        tmpRange = (sapply(1:length(myFiles), 
                           function(x) c(min(myFiles[[x]][,selectedLogFC1]), 
                                         max(myFiles[[x]][,selectedLogFC1]))))
        maxE = max(tmpRange[2,])
        minE = min(tmpRange[1,])
      } else {
        tmpRange = c(min(myFiles[,selectedLogFC1]),
                     max(myFiles[,selectedLogFC1]))
        maxE = max(tmpRange[2])
        minE = min(tmpRange[1])
      }
      
      testcol  <- colorRampPalette(c(input$col1, input$col2, input$col3))
      # testcol = colorRampPalette(rainbow(6))
      
      testcol(71)
      ramp <- colorRamp(testcol(71))
      tmppalette = rgb( ramp(seq(0, 1, length = 71)), max = 255)
      n=length(tmppalette)
      
      tmplist = list(c(minE, maxE), tmppalette)
      names(tmplist) = c('minmax', 'palette')
      
      return(tmplist)
      
    })
    
    output$colMaxMinPalette <- renderUI({
      if (input$lkn == '') return(NULL)
      df <- filedata()
      if (is.null(df)) return(NULL)
      
      if  (is.null(dim(myFiles))) {
        tmp=(sapply(1:length(myFiles),
                    function(x) sapply(1:dim(myFiles[[x]])[2],
                                       function(y) if(is.numeric(myFiles[[x]][,y])){(max(myFiles[[x]][,y]))})))
        maksimus = max(unlist(tmp[,1]))
      } else {
        maksimus = max(unlist(lapply(1:dim(myFiles)[2], 
                                      function(x) if(is.numeric(myFiles[,x])) {max(myFiles[,x])} )))
      }
      numericInput("colMaxMinPalette", "Upper/lower boundary palette threshold", 
                   value = 2.5, 
                   min = 0, 
                   max = maksimus)
    })
    
    colMaxMin <- reactive({ 
      if (input$lkn == '') return(NULL)
      if (input$cutoff2 == '') return(NULL)
      df <- filedata()
      if (is.null(df)) return(NULL)
      
      return(abs(input$colMaxMinPalette))
    })
    
    output$piecolours <- renderPlot({
      input$createPalette
      n = length(isolate(mypalette())$palette)
      myminmax = colMaxMin()
      pie(rep(1,n), col=(mypalette())$palette,
          labels = c(paste0('<= -', as.character(format(myminmax, nsmall = 2))), 
                     '', '', '', '', '', '', '', '',
                     paste0('(-', as.character(format(myminmax, nsmall = 2)), ',', '-', format((cutFC()), nsmall = 2), ')'), 
                     rep('',25), 
                     format(0, nsmall = 2),
                     rep('',25),
                     paste0('(', format((cutFC()), nsmall = 2), ',', as.character(format(myminmax, nsmall = 2)), ')'),
                     '', '', '', '', '', '', '', '',
                     paste0('>= ', as.character(format(myminmax, nsmall = 2)))), 
          clockwise = TRUE, radius = 1.0, border = FALSE, init.angle = 90,
          las=3) 
    }) 
    
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    # jump to animated vis
    observe({
      if(input$createPalette){
        # print('12')
        updateNavbarPage(session, inputId = 'tabselected', selected = "12")
      }
    })
    # jump to network info
    observe({
      if(input$lkn != ''){
        # print('3')
        updateNavbarPage(session, inputId = 'tabselected', selected = "3")
      }
    })
    # jump to files
    observe({
      if((!is.null(input$selectFiles)) & (input$lkn != "QuickAppTest(Ath123)")){
        # print('1')
        updateNavbarPage(session, inputId = 'tabselected', selected = "1")
      }
    })
    observe({
      if(!is.null(input$link_to_START1)){
          updateNavbarPage(session, inputId = 'tabselected', selected = "14")
      }
    })
    observe({
      if(!is.null(input$link_to_START2)){
        updateNavbarPage(session, inputId = 'tabselected', selected = "14")
      }
    })
    observe({
      if(!is.null(input$link_to_START3)){
        updateNavbarPage(session, inputId = 'tabselected', selected = "14")
      }
    })
    observe({
      if(!is.null(input$link_to_START4)){
        updateNavbarPage(session, inputId = 'tabselected', selected = "14")
      }
    })
    observe({
      if(!is.null(input$link_to_START5)){
        updateNavbarPage(session, inputId = 'tabselected', selected = "14")
      }
    })
    observe({
      if(!is.null(input$link_to_START6)){
        updateNavbarPage(session, inputId = 'tabselected', selected = "14")
      }
    })
    observe({
      if(!is.null(input$link_to_START7)){
        updateNavbarPage(session, inputId = 'tabselected', selected = "14")
      }
    })
    observe({
      if(!is.null(input$link_to_START8)){
        updateNavbarPage(session, inputId = 'tabselected', selected = "14")
      }
    })
    observe({
      if(!is.null(input$link_to_START9)){
        updateNavbarPage(session, inputId = 'tabselected', selected = "14")
      }
    })
    observe({
      if(!is.null(input$link_to_START10)){
        updateNavbarPage(session, inputId = 'tabselected', selected = "14")
      }
    })
    observe({
      if(!is.null(input$link_to_START11)){
        updateNavbarPage(session, inputId = 'tabselected', selected = "14")
      }
    })
    observe({
      if(!is.null(input$link_to_START12)){
        updateNavbarPage(session, inputId = 'tabselected', selected = "14")
      }
    })
    observe({
      if(!is.null(input$link_to_START13)){
        updateNavbarPage(session, inputId = 'tabselected', selected = "14")
      }
    })
    # jump to vis static
    # observe({
    #   if(!is.null(input$timepoint)){
    #     if(input$timepoint > 1){
    #       # print('11')
    #       updateNavbarPage(session, inputId = 'tabselected', selected = "11")
    #     }
    #   }
    # })
    # jump to BCKG
    # observe({
    #   tmp = (trimws(input$nameType))
    #   if((tmp != 'geneX') & (tmp != '')){
    #     if(input$timepoint > 1){
    #       print('10')
    #       updateNavbarPage(session, inputId = 'tabselected', selected = "10")
    #     }
    #   }
    # })
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    
    
    
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    #### FOR STATIC CLUSTER PLOTS: node degree, timepoint
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    
    # define above which degree simplified background cluster will be plotted
    output$mydegree <- renderUI({
      numericInput(inputId = "mydegree", 
                   label = NULL, #"Filter background network cluster by minimal node degree threshold:", 
                   value = 0, 
                   min = 0,
                   step = 1)
    })
    
    # define which time point you want to plot?
    output$timepoint <- renderUI({
      df <- filedata()
      if (is.null(df)) return(NULL)
      input$selectFiles
      numericInput(inputId = "timepoint", 
                   label = NULL, #"For which timepoint/condition you would like to see static DE plot?", 
                   value = 1,
                   min = 1, 
                   max = max(1, maxTP()))
    })
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    
    
    
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    

    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    #### HISTOGRAMS
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    output$myhist1 <- renderPlot({
      
      input$lkn
      
      isolate({
        if (is.null(getNodes())) return(NULL)
        yy = getNodes()
      })
      
      isolate({
        if (is.null(getEdges())) return(NULL)
        xx = getEdges()
      })
      
      yy = yy[yy$clusterID !=0,]
      
      t = (as.data.frame(table(yy$clusterID)))
      d = t[with(t, order(-Freq, Var1)), ]
      d$Position = seq(1, dim(d)[1], 1)
      yrange = range(d$Freq)
      xrange = range(as.numeric(as.character(d$Var1)))
      ## using xaxt="n" to avoid showing the x-axis
      barplot(d$Freq, 
              #breaks = sort(unique(d$Freq)),
              xlim = c(1,2*max(xrange) + 10),
              ylim = c(0,max(yrange) + 150),
              xlab = paste0('Number of clusters: ', length(unique(yy$clusterID))),
              ylab = 'Number of nodes in cluster',
              main = paste0('\n', 'Number of nodes per cluster in ',  input$lkn, ' network'),
              xaxt="n",
              yaxt="n",
              col = rainbow(dim(d)[1]),
              space=1)
      ## draw the x-axis with user-defined tick-marks
      axis(side=1, at=seq(xrange[1],2*xrange[2],2)+0.5, 
           labels = as.numeric(as.character(d$Var1)), 
           las=2)
      axis(side=2, at=d$Freq, labels = d$Freq)
      
      
    })
    
    output$nodesClusters <- renderDataTable({
      
      input$lkn
      
      isolate({
        if (is.null(getNodes())) return(NULL)
        yy = getNodes()
      })
      
      isolate({
        if (is.null(getEdges())) return(NULL)
        xx = getEdges()
      })  
      
      yy = yy[yy$clusterID !=0,]
      myCols = toupper(c('geneID', 'shortName', 'clusterID'))
      onlyCols = sapply(myCols, function(x) grep(paste("^",x,"$", sep=""), toupper(colnames(yy))))
      yy = yy[,onlyCols]
      t = (as.data.frame(table(yy$clusterID)))
      ind = match(yy[,3], t[,1])
      yy$NumberOFnodesPERcluster = t[ind,2]
      
      return(yy)
      
    })
    
    
    output$myhist2 <- renderPlot({
      
      isolate({
        if (is.null(getNodes())) return(NULL)
        yy = getNodes()
      })
      
      isolate({
        if (is.null(getEdges())) return(NULL)
        xx = getEdges()
      })
      
      yy = yy[yy$clusterID !=0,]
      
      # http://statmethods.net/advgraphs/layout.html
      layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE), widths=c(1,1), heights=c(1,1.5))
      
      t = (as.data.frame(table(yy$clusterSimplifiedNodeDegree))) ####  ####  FIXmz
      d = t[with(t, order(-Freq, Var1)), ]
      myThreshold = round(quantile(as.numeric(as.character(d$Freq)),0.75)[[1]])
      if (myThreshold < max(d$Freq)) {
        d = d[d$Freq > round(quantile(as.numeric(as.character(d$Freq)),0.75)[[1]]),]
      }
      d$Position = seq(1, dim(d)[1], 1)
      yrange = range(d$Freq)
      xrange = range((as.numeric(d$Position)))
      ## using xaxt="n" to avoid showing the x-axis
      barplot(d$Freq, 
              #breaks = sort(unique(d$Freq)),
              xlim = c(1,2*max(xrange) + 10),
              ylim = c(0,max(yrange) + 250),
              xlab = 'Cluster node degree, not counting multiple relations and loops', #### FIXmz
              ylab = paste0('Number of nodes per degree (', yrange[1], ' - ',  yrange[2],')'),
              main = paste0('\n',
                "Number of nodes in ", input$lkn, ' network: ', dim(yy)[1], "\n",
                "Highest node degree: ", max(yy$clusterSimplifiedNodeDegree, yy$clusterSimplifiedNodeDegree),  "\n",
                "Lowest node degree: ", min(yy$clusterSimplifiedNodeDegree, yy$clusterSimplifiedNodeDegree), "\n"),
              xaxt="n",
              yaxt="n",
              col = rainbow(dim(d)[1]),
              space=1)
      ## draw the x-axis with user-defined tick-marks
      axis(side=1, at=seq(xrange[1],2*xrange[2],2)+0.5, 
           labels = as.numeric(as.character(d$Var1)), 
           las=2)
      axis(side=2, at=d$Freq, labels = d$Freq)
      
      ####  ####  ####  ####  ####  
      
      t = (as.data.frame(table(yy$clusterSimplifiedNodeDegree))) ####  ####  FIXmz
      d = t[with(t, order(-Freq, Var1)), ]
      d = d[d$Freq <= round(quantile(as.numeric(as.character(d$Freq)),0.75)[[1]]),]
      d$Position = seq(1, dim(d)[1], 1)
      yrange = range(d$Freq)
      xrange = range((as.numeric(d$Position)))
      ## using xaxt="n" to avoid showing the x-axis
      # https://stat.ethz.ch/R-manual/R-devel/library/graphics/html/barplot.html
      barplot(d$Freq, 
              #breaks = sort(unique(d$Freq)),
              ylim = c(1,6*max(xrange) + 10),
              xlim = c(0,max(yrange) + 1),
              ylab = 'Cluster node degree, not counting multiple relations and loops',
              xlab = paste0('Number of nodes per degree (', yrange[1], ' - ',  yrange[2],')'),
              main = '',
              xaxt="n",
              yaxt="n",
              col = rainbow(dim(d)[1]),
              space=2,
              horiz=TRUE,
              cex.lab=1.0,
              cex.axis=0.5,
              width = 2)
      ## draw the x-axis with user-defined tick-marks
      axis(side=2, at=seq(xrange[1],6*xrange[2],6)+4.0, 
           labels = as.numeric(as.character(d$Var1)), 
           las=2, cex.axis=0.75)
      axis(side=1, at=d$Freq, labels = d$Freq, cex.axis=1.0)
    })
    
    
    output$myhist3 <- renderPlot({
      
      isolate({
        if (is.null(getNodes())) return(NULL)
        yy = getNodes()
      })
      
      isolate({
        if (is.null(getEdges())) return(NULL)
        xx = getEdges()
      })     
      
      t = (as.data.frame(table(xx$reactionType)))
      d = t[with(t, order(-Freq, Var1)), ]
      d$Position = seq(1, dim(d)[1], 1)
      yrange = range(d$Freq)
      xrange = range(as.numeric(as.character(d$Position)))
      ylim <- c(0, 1.1*max(d$Freq))
      ## using xaxt="n" to avoid showing the x-axis
      par(mar = c(max(5.1, max(nchar(as.character(d$Var1)))/1.0), 4.1, 4.1 ,2.1))
      
      bp = barplot(d$Freq, 
                   #breaks = sort(unique(d$Freq)),
                   xlim = c(1,2*max(xrange) + 10),
                   ylim = ylim,
                   xlab = '',
                   ylab = 'Number of edges per reaction type',
                   main = paste0('\n',
                     'Reaction types in ', input$lkn, ' network', "\n",
                     "Number of edges: ", dim(xx)[1], "\n",
                     "Number of loops: ", dim(xx[xx$geneID1 == xx$geneID2,])[1]
                   ),
                   xaxt="n",
                   yaxt="n",
                   col = rainbow(dim(d)[1]),
                   space=1)
      text(x = bp, y = d$Freq, label = d$Freq, pos = 3, cex = 0.8, col = "black")
      ## draw the x-axis with user-defined tick-marks
      axis(side=1, at=seq(xrange[1],2*xrange[2],2)+0.5, 
           labels = (as.character(d$Var1)), 
           las=2)
      axis(side=2, at=d$Freq, labels = d$Freq)
      
      par(mar = c(5.1, 4.1, 4.1, 2.1))
      
      
    })
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    
    
    
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    #### WORDCLOUDS
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    output$mywordcloud1 <- renderWordcloud2({ #renderd3Cloud({
      

      strtoi(trimws(input$mydegree))
      mytimepoint = strtoi(trimws(input$timepoint))
      input$createPalette
      ncluID = strtoi(trimws(input$icluID))
      
      
      yy <- isolate(animatoRNE())$n
      yy1 = yy$MapManBin

      # if (input$lkn != "Custom network") {
        tmpstr = yy1
        
        if (length(tmpstr)){
          a = unlist(strsplit(tmpstr, ' | ',  fixed = TRUE))
          b = strsplit(a,'_')
          c=sapply(1:length(b), function(x) b[[x]][1])
          t = (as.data.frame(table(c)))
          colnames(t) = c('Var1', 'Freq')
          d = t[with(t, order(-Freq, Var1)), ]
          rownames(d) = d[,1]
          colnames(d) = c('word', 'freq')
          # typeof(d$word)
          # typeof(demoFreq$freq)

          
          wordcloud2(d, #size = 2, gridSize = 1, ellipticity = 0.6)
                     shape = 'cardioid',
                     gridSize = 1.5,
                     ellipticity = 0.6,
                     size = 1.0, 
                     minRotation = -pi/2, maxRotation = -pi/2,
                     #figPath = paste0(getwd(),"/M2jeo.jpg"),
                     color = "random-light")#, backgroundColor = "grey")
          # d3Cloud(text = d[,1], size = d[,2])
        }
      # }

      
      
    }
    )
    
    
    output$mywordcloud2MapManTable <- renderDataTable({
      
      if (is.null(changeFileList())) return(NULL)

      strtoi(trimws(input$mydegree))
      mytimepoint = strtoi(trimws(input$timepoint))
      input$createPalette
      ncluID = strtoi(trimws(input$icluID))
      

      yy <- isolate(animatoRNE())$n

      yy1 = yy$MapManBin

      if (input$lkn != "Custom network") {
        tmpstr = yy1#yy1[yy1!= '-']
        
        if (length(tmpstr)){
          a = unlist(strsplit(tmpstr, ' | ',  fixed = TRUE))
          b = strsplit(a,'_')
          c=sapply(1:length(b), function(x) b[[x]][1])
          d=sapply(1:length(b), function(x) b[[x]][2])
          e = cbind(c, d)
          f = unique(e)
          rownames(f) = f[,1]
          colnames(f) = c('bin', 'description')
          g = as.data.frame(f)
          h = g[with(g, order(bin, description)), ]

        } 
      } else {
        h = (as.data.frame(table(yy1)))
        colnames(h) = c('bin(s)', 'Freq')
      }
      
      return(h)
      
    })
    

    # animatoR last uploaded time-point static plot
    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    observeEvent(input$plot1_dblclick, {
      brush <- input$plot1_brush
      if (!is.null(brush)) {
        ranges$x <- c(brush$xmin, brush$xmax)
        ranges$y <- c(brush$ymin, brush$ymax)
        
      } else {
        ranges$x <- NULL
        ranges$y <- NULL
      }
    })
    
    output$mycluPlot <- renderVisNetwork({#renderPlot({
      
      if (is.null(changeFileList())) return(NULL)
      
      strtoi(trimws(input$mydegree))
      mytimepoint = strtoi(trimws(input$timepoint))
      input$createPalette
      trimws(input$nameType)
      strtoi(trimws(input$icluID))
      
      n <- isolate(animatoRNE())$n
      e <- isolate(animatoRNE())$e
      ncluID = unique(n$clusterID)
      
        if ((dim(e)[1] <= 2^14) & (sum(e$geneID1 != e$geneID2) >= 1)){
          whatToPlot(node1 = n, edge1 = e, cluID = ncluID, mytimepoint = mytimepoint, isOK = 1)
        } else {
          whatToPlot(node1 = n, edge1 = e, cluID = ncluID, mytimepoint = mytimepoint, isOK = 0)
        }
    }) 
    
    # https://github.com/datastorm-open/visNetwork/issues/138
    output$downloadVisStatClu <- downloadHandler(
      
      filename = function() {
        paste0('LKN:',
               input$lkn, 
               '_',
               'clu:',
               unique(isolate(animatoRNE())$n$clusterID), 
               '_',
               'deg:',
               trimws(input$mydegree),
               '_',
               'tp:',
               strtoi(trimws(input$timepoint)), 
               '.html')
      },
      
      content = function(con) {
        n <- isolate(animatoRNE())$n
        e <- isolate(animatoRNE())$e
        if ((dim(e)[1] <= 2^14) & (sum(e$geneID1 != e$geneID2) >= 1)){
          myVisStat = whatToPlot(node1 = n, edge1 = e, cluID = unique(n$clusterID), mytimepoint = strtoi(trimws(input$timepoint)), isOK = 1)
        } else {
          myVisStat = whatToPlot(node1 = n, edge1 = e, cluID = unique(n$clusterID), mytimepoint = strtoi(trimws(input$timepoint)), isOK = 0)
        }
        
        myVisStat$width = 1850
        myVisStat$height = 850
        myVisStat %>%
          visOptions(highlightNearest = TRUE) %>% 
          visExport() %>% 
          visSave(con)
      }
    )
    
    
    myNDTVout <- reactive({
      
      if (is.null(changeFileList())) return(NULL)
      cutDeg = strtoi(trimws(input$mydegree))
      trimws(input$nameType)
      strtoi(trimws(input$icluID))
      mytimepoint = strtoi(trimws(input$timepoint))
      input$createPalette
      input$downloadNDTV
      input$varSpeed

      n <- isolate(animatoRNE())$n
      e <- isolate(animatoRNE())$e
      
      mypalette = (isolate(mypalette()))
      ### other variables: ###
      # column number for gene IDs
      myIDcol1 = isolate(myIDcol())
      # column number for e.g. adjusted p-value cut-off
      selectedPval1 = isolate(selectedPval())
      # column number for e.g. logFC cut-off and colour/size of node assignment
      selectedLogFC1 = isolate(selectedLogFC())
      # threshold for e.g. adjusted p-value cut-off
      cutP1 = isolate(cutP())
      # threshold for e.g. logFC cut-off
      cutFC1 = isolate(cutFC())
      
      mythList = howPatient()
      eThreshold = strtoi(mythList$eThreshold)
      nThreshold = strtoi(mythList$nThreshold)
      # cat(eThreshold, nThreshold, '\n\n')
      
      minmax = isolate(colMaxMin())
      
      mymodplus = as.numeric(input$varSpeed)
      mysteps = seq(0, length(myFiles), as.numeric(input$varSpeed))

      tic=proc.time()[3]
        return(
        capture.output(type = "message", split = FALSE,{
          cat("", "\n")
          myd3movie(n, e, cutDeg, mymodplus,
                    mypalette, myFiles,
                    myIDcol1, selectedPval1, selectedLogFC1, cutP1, cutFC1,
                    eThreshold, nThreshold,
                    minmax)
          print("Steps:")
          print(mysteps)
          print("Time:")
          print((proc.time()[3] - tic)/60)
          }
        )
        )
    })
    
    observe({
      input$downloadNDTV
      output$ndtvPopUp <- renderPrint(print(myNDTVout()))
    })
    

    # output$ndtvPopUp <- renderPrint({
    #   input$downloadNDTV
    #   print(myNDTVout())
    # })
      
    # https://www.rdocumentation.org/packages/ndtv/versions/0.10.0/topics/ndtvAnimationWidget
    output$downloadNDTV2 <- downloadHandler(

      filename = function() {
        paste0('LKN:',
               input$lkn,
               '_',
               'clu:',
               unique(isolate(animatoRNE())$n$clusterID),
               '_',
               'deg:',
               trimws(input$mydegree),
               '_',
               'tp:',
               strtoi(trimws(input$timepoint)),
               'NDTV.html')
      },

      content = function(file) {
        
        if (is.null(myNDTVout())) return(NULL)
        file.copy("mytempHTML.html", file)
      }
    )
    
    
    output$mycluPlot2 <- renderPlot({
      
      if (is.null(changeFileList())) return(NULL)
      
      strtoi(trimws(input$mydegree))
      mytimepoint = strtoi(trimws(input$timepoint))
      input$createPalette
      trimws(input$nameType)
      strtoi(trimws(input$icluID))
      
      n <- isolate(animatoRNE())$n
      e <- isolate(animatoRNE())$e
      ncluID = unique(n$clusterID)
      
      subsetNid = intersect(n[(data.table::between(n$x, ranges$x[1], ranges$x[2])),1],
                        n[(data.table::between(n$y, ranges$y[1], ranges$y[2])),1])
      subsetN = n[match(subsetNid, n$geneID),]
      subsetE = e[(e$geneID1 %in% subsetNid) & (e$geneID2 %in% subsetNid),]
      
      if(dim(subsetN)[1] != 0){
        if ((dim(e)[1] <= 2^14) & (sum(e$geneID1 != e$geneID2) >= 1)){
          whatToPlot2(node1 = subsetN, edge1 = subsetE, cluID = ncluID, mytimepoint = mytimepoint, isOK = 1)
        } else {
          whatToPlot2(node1 = n, edge1 = e, cluID = ncluID, mytimepoint = mytimepoint, isOK = 0)
        }
      } else {
      if ((dim(e)[1] <= 2^14) & (sum(e$geneID1 != e$geneID2) >= 1)){
        whatToPlot2(node1 = n, edge1 = e, cluID = ncluID, mytimepoint = mytimepoint, isOK = 1)
      } else {
        whatToPlot2(node1 = n, edge1 = e, cluID = ncluID, mytimepoint = mytimepoint, isOK = 0)
      }
      }
      
      
    }) 
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    
    
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    
    

    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    #### SUBNETWORKS FOR CLUSTER ANIMATIONS AND PLOTS
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    maxCLU <- reactive({
      if (input$lkn == '') {
        return(NULL)
      }
      df <- filedata()
      if (is.null(df)) return(NULL)
      if (length(input$selectFiles) < 1)  return(NULL)

      isolate({
        if (is.null(getNodes())) return(NULL)
        yy = getNodes()
      })

      return(max(yy$clusterID))
    })
    
    output$icluID <- renderUI({
      if (input$lkn == '') {
        return(NULL)
      }
      df <- filedata()
      if (is.null(df)) return(NULL)
      if (length(input$selectFiles) < 1)  return(NULL)
      if (is.null(maxCLU)) return(NULL)
      
      numericInput(inputId = 'icluID', 
                   label = NULL, #'Filter by cluster ID', 
                   value = 1, 
                   min = 1, 
                   max = isolate(maxCLU()))
    })
    # outputOptions(output, 'maxCLU', suspendWhenHidden = FALSE)
    #### #### #### #### #### AnimatoR for #### #### #### #### #### #### #### ###
    maxTP <- reactive({
      if (input$lkn == '') {
        return(NULL)
      }
      df <- filedata()
      if (is.null(df)) return(NULL)
      if (length(input$selectFiles) < 1)  return(NULL)
      
      #return(length(myFiles))
      return(length(input$selectFiles))
    })
    
    output$mystep <- renderUI({
      if (input$lkn == '') {
        return(NULL)
      }
      df <- filedata()
      if (is.null(df)) return(NULL)
      if (length(input$selectFiles) < 1)  return(NULL)
      if (is.null(maxTP)) return(NULL)

      numericInput(inputId = "mystep", 
                   label = NULL,
                   value = 0.01,
                   min = 0.00, 
                   max = max( 0, (maxTP())),
                   step = 0.1)
    })
    
    output$animatoRslider <- renderUI({
      if (input$lkn == '') {
        return(NULL)
      }
      df <- filedata()
      if (is.null(df)) return(NULL)
      if (length(input$selectFiles) < 1)  return(NULL)
      if (is.null(maxTP)) return(NULL)
      
      # cat('myFiles', length(myFiles), '\n')

      sliderInput(inputId = "tanim",
                  label = "Time/Condition",
                  min = 0,
                  max = max( 0, (maxTP())),
                  value = 0,
                  step = input$mystep,# max(0.01, round((maxTP()*(2^-8)), 2)),
                  animate = animationOptions(interval=250,loop=TRUE)
      )
    })
    
    tanim2 <- reactive ({
      input$selectFiles
      return(input$tanim)
    })
    
    tanimVis <- reactive ({
      input$selectFiles
      return(input$tanim)
    })
    

    # AB animatoR animation
    animatoRNE <- reactive({
      if (is.null(maxTP)) return(NULL)
      isolate({
        if (is.null(changeFileList())) return(NULL)
      })
      input$createPalette
      strtoi(trimws(input$mydegree))

      isolate({
        if (is.null(getNodes())) return(NULL)
        yy = getNodes()
      })
      
      # all edges for selected network
      isolate({
        if (is.null(getEdges())) return(NULL)
        xx = getEdges()
      })
      
      # user filtering by geneID or cluster
      iname <- (trimws(input$nameType)) # for testing purpose use: iname = 'MIR834'
      ncluID <- strtoi(trimws(input$icluID)) # for testing purpose use: ncluID = 49
      nname <- (nchar(iname))
      
      # if filtering by geneID:
      ## find node cluster
      ## select onlynodes within the cluster
      ## select edges in which one node is within the cluster
      if ((nname > 0) & (iname !='geneX') & (iname !='')) {
        tmp = yy[grep(paste("^",iname,"$", sep=""), yy$geneID),]
        yy <- yy[which(tmp$clusterID == yy$clusterID),]
        xx <- xx[union(which(tmp$clusterID == xx$clusterID_geneID1),
                       which(tmp$clusterID == xx$clusterID_geneID2)),]
        ncluID = unique(yy$clusterID)
        
      } else {
        # if filtering by cluster:
        ## select onlynodes within the cluster
        ## select edges in which one node is within the cluster
        #if (ncluID > 0){
          yy <- yy[which(ncluID == yy$clusterID),]
          xx <- xx[union(which(ncluID == xx$clusterID_geneID1),
                         which(ncluID == xx$clusterID_geneID2)),]
        #}
        
      }

      
      node1 = yy
      edge1 = xx
      cutDeg = strtoi(trimws(input$mydegree)) # for testing purpose use: cutDeg = 0
      # label <- FALSE
      dimnames(node1)[[1]] <- node1$geneID
      clusterID = unique(node1$clusterID)
      clusterID == ncluID
      
      coln = NULL # sth about colours form AB script
      cole = NULL # sth about colours form AB script
      
      # this first dim(n) should be == dim(node1)
      n <- node1[which(node1$clusterID%in%clusterID),]
      yLimits = range(n$y) ####  ####  ####  ####  ####  ####  ####  ####  ####  FIXmz
      xLimits = range(n$x) ####  ####  ####  ####  ####  ####  ####  ####  ####  FIXmz
      sel = which(n$clusterSimplifiedNodeDegree >= cutDeg)
      n = n[sel,]
      # dim(e) is not == dim(edge1)
      select <- which((edge1$clusterID_geneID1 %in% clusterID)|(edge1$clusterID_geneID2 %in% clusterID))
      e <- edge1[select,]
      select <- which((e$geneID1 %in% n$geneID)&(e$geneID2 %in% n$geneID))
      e <- e[select,]
      selectNonLoop <- which(e$geneID1 != e$geneID2)  ####  FIXmz
      eNonLoop = e[selectNonLoop,] ####  ####  ####  ####  ####  ####  ####  FIXmz
      sel = sort(which(n$geneID %in% union(eNonLoop$geneID1, eNonLoop$geneID2)))  ####  FIXmz
      n = n[sel,]   ####  ####  ####  ####  ####  ####  ####  ####  ####  FIXmz
      select <- which((e$geneID1 %in% n$geneID)&(e$geneID2 %in% n$geneID))
      e <- e[select,]
      
      return(list(n=n, e=e, yLimits=yLimits, xLimits=xLimits)) ####  ####  ####  ####  ####  ####  ####  ####  ####  FIXmz
      
    }) 
    
    VIScluster <- reactive({
      
      # if (is.null(changeFileList())) return(NULL)
      
      strtoi(trimws(input$mydegree))
      trimws(input$nameType)
      strtoi(trimws(input$icluID))
      input$createPalette
      
      
      n <- isolate(animatoRNE())$n
      e <- isolate(animatoRNE())$e
      

      
      if ((dim(e)[1] <= 2^14) & (sum(e$geneID1 != e$geneID2) >= 1) ){
        mytmplist = ABanimatoRfunctionVIS(n = n, e = e, clusterID = unique(n$clusterID), isOK = 1)
      } else {
        mytmplist = ABanimatoRfunctionVIS(n = n, e = e, clusterID = unique(n$clusterID), isOK = 0)
      }

      # cat("cluster: ", unique(n$clusterID), "\n")

      return(mytmplist)
      
    })
    
    
    output$network_proxy_nodes <- renderVisNetwork({
      

      if (is.null(VIScluster())) return(NULL)
      mytmplist = VIScluster()


        ########################################################################
        n = mytmplist[[1]]
        e = mytmplist[[2]]
        palette = mytmplist[[3]]
        clusterID = mytmplist[[4]]
        ########################################################################
        if ((length(e)) & (sum(e[[1]]$geneID1 != e[[1]]$geneID2) >= 1)) {

            vnNE = plotAnimatedNetworksVisAnimated(nlist = n, elist = e, palette = mypalette, clusterID = clusterID, t0 = 0)
      
            # myCols = toupper(c("id", "shape","title","x","y","physics", 'value', 'color'))
            # onlyCols = sapply(myCols, function(x) grep(paste("^",x,"$", sep=""), toupper(clnames(vnNE$n))))
            # onlyCols = setdiff(colnames(vnNE$n), 'label')
            # vnNE$n = vnNE$n[,onlyCols]
            # myCols = toupper(c("from","to","label","width","color","title","physics"))
            # onlyCols = sapply(myCols, function(x) grep(paste("^",x,"$", sep=""), toupper(colnames(vnNE$e))))
            # e = vnNE$e#[,onlyCols]
      
            ledges <- data.frame(color = rev(unique(isolate(mypalette())$palette)), 
                                 label =  rep('',length(unique(isolate(mypalette())$palette))))
            
            vng = visNetwork(nodes = vnNE$n, edges = vnNE$e, height = "1000px", width = '100%', main = vnNE$main)
            igraphlayout <- list(type = "square")
            vng$x$igraphlayout <- igraphlayout
            
            vng$x$edges$hidden = (as.vector(vng$x$edges$to) == as.vector(vng$x$edges$from))
            vng$x$edges$title = NULL
            vng$x$edges$label = NULL
            # vng$x$nodes$label = rep('', length(vng$x$nodes$label))
            
            # cat("renderVisNetwork", '\n')
    
            vn = vng %>% 
              visEdges(smooth = FALSE) %>% 
              # visNodes(scaling = list(min = 20, max = 60)) %>% 
              visPhysics(stabilization = FALSE) %>% 
              visLegend(useGroups = F, addNodes = ledges, position = 'right', # stepX = 25, stepY = 25, 
                        width = 0.05) %>% 
              visInteraction(navigationButtons = TRUE,
                             keyboard = FALSE, 
                             tooltipDelay = 0) 
            
    
        } else {
            vn = visNetwork(LjubljanaNodes, LjubljanaEdges, 
                            height = "100%", width = "100%", 
                            main = 'Subnetwork has to many/less edges to be nicely shown.</br>Check if nonexistent cluster or gene ID</br><span style="color: #f00;"> OR </span></br>increase/decrease minimal node degree threshold.</br>',
                            footer = 'Ljubljana graph') %>% 
              visIgraphLayout()
        }

    })
    
    observe({
      
      # input$icluID
      # input$nameType

      if (is.null(tanimVis())) return(NULL)
      

      mytmplist = VIScluster()


      ########################################################################
      n = mytmplist[[1]]
      e = mytmplist[[2]]
      palette = mytmplist[[3]]
      clusterID = mytmplist[[4]]
      ########################################################################
      
      
      vnNE = plotAnimatedNetworksVisAnimated(nlist = n, elist = e, palette = palette, clusterID = clusterID, t0 = tanimVis())
      

      #### #### ######## #### #### ######## #### #### ######## #### #### ########
      #### #### ######## #### #### ######## #### #### ######## #### #### ########
      visNetworkProxy("network_proxy_nodes") %>%
        visUpdateNodes(data.frame(id = vnNE$n$id, 
                                  label = vnNE$n$label,
                                  value = vnNE$n$value, 
                                  color = vnNE$n$color,
                                  font.size = vnNE$n$font.size,
                                  borderWidth = vnNE$n$borderWidth
                       ))  %>%
        visUpdateEdges(data.frame(id = vnNE$e$id, 
                                  width = vnNE$e$width, 
                                  color = vnNE$e$color))
      #### #### ######## #### #### ######## #### #### ######## #### #### ########
      #### #### ######## #### #### ######## #### #### ######## #### #### ########
      
      # cat ("visNetworkProxy", '\n')

    })

    # output$ABanimatoRAB <- renderPlot({
    myABanimatoRABplot <- function(){
      
      # isolate({
        if (is.null(changeFileList())) return(NULL)
      # })
      strtoi(trimws(input$mydegree))
      trimws(input$nameType)
      strtoi(trimws(input$icluID))
      mytimepoint = strtoi(trimws(input$timepoint))
      input$createPalette

      
      n <- isolate(animatoRNE())$n
      e <- isolate(animatoRNE())$e
      

      
      if ((dim(e)[1] <= 2^14) & (sum(e$geneID1 != e$geneID2) >= 1)){
        ABanimatoRfunctionAB(n = n, e = e, clusterID = unique(n$clusterID), isOK = 1)
      } else {
        ABanimatoRfunctionAB(n = n, e = e, clusterID = unique(n$clusterID), isOK = 0)
      }
      
    }
    output$ABanimatoRAB <- renderPlot({
      myABanimatoRABplot()
    })
    # downloadHandler contains 2 arguments as functions, namely filename, content
    ## call the plot function when downloading the image
    output$down <- downloadHandler(
      filename =  function() {
        paste0('LKN:',
               input$lkn, 
               '_',
               'clu:',
               strtoi(trimws(input$icluID)),
               '_',
               'deg:',
               trimws(input$mydegree),
               '_',
               'tp:',
               input$tanim, 
               '.',
               input$var3)
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        if(input$var3 == "png")
          png(file, width = 1000, height = 750) # open the png device
        else
          pdf(file, width = 28, height = 20) # open the pdf device
        myABanimatoRABplot() 
        dev.off()  # turn the device off
        
      } 
    )
    
    
    # plot background cluster containing only nodes with simplified degree 
    # (simplified means: not counting multiple edges or loops)
    # within the network above user defined threshold (min = 0)
    output$BCKGN <- renderPlotly({
      
      if (is.null(changeFileList())) return(NULL)
      
      strtoi(trimws(input$mydegree))
      mytimepoint = strtoi(trimws(input$timepoint))
      input$createPalette
      trimws(input$nameType)
      strtoi(trimws(input$icluID))
      
      n <- isolate(animatoRNE())$n
      e <- isolate(animatoRNE())$e
      
      

      if ((dim(e)[1] > 2^14) | (sum(e$geneID1 != e$geneID2) < 1)){
        myplotlys(n = n, e = e, clusterID = unique(n$clusterID), isOK = 0)
      } else {
        myplotlys(n = n, e = e, clusterID = unique(n$clusterID), isOK = 1)
      }

    })
    # https://plot.ly/r/shinyapp-plotly-events/#code
    # output$hover <- renderPrint({
    #   d <- event_data("plotly_hover")
    #   if (is.null(d)) "Hover events appear here (unhover to clear)" else d
    # })
    
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    
    
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    
    
    
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    #### TABLES PER CLUSTER, NODES AND EDGES
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    
    output$currentTime <- renderText({
      invalidateLater(1000, session)
      paste("The current date and time: ", Sys.time())
    })
    
    
    # Edges table per cluster nad/or geneID
    output$myText1 <- renderDataTable({
      
      if (is.null(changeFileList())) return(NULL)
      
      input$selectFiles
      mytimepoint = strtoi(trimws(input$timepoint))
      input$createPalette

      degreeCutoff <- strtoi(trimws(input$mydegree))
      iname <- (trimws(input$nameType))
      ncluID <- strtoi(trimws(input$icluID))
      nname <- (nchar(iname))
      
      
      isolate({
        if (is.null(getNodes())) return(NULL)
        yy = getNodes()
      })
      
      # all edges for selected network
      isolate({
        if (is.null(getEdges())) return(NULL)
        xx = getEdges()
      })
      
      if ((nname > 0) & (iname !='geneX') & (iname !='')) {
        tmp = yy[grep(paste("^",iname,"$", sep=""), yy$geneID),]
        xx <- xx[union(which(tmp$clusterID == xx$clusterID_geneID1),
                       which(tmp$clusterID == xx$clusterID_geneID2)),]
        
      } else {
        xx <- xx[union(which(ncluID == xx$clusterID_geneID1),
                       which(ncluID == xx$clusterID_geneID2)),]
      }
      
      sel = intersect(which(xx$clusterSimplifiedNodeDegree_geneID1 >= degreeCutoff), #### FIXmz
                      which(xx$clusterSimplifiedNodeDegree_geneID2 >= degreeCutoff)) #### FIXmz
      xx = xx[sel,]

      if (input$lkn != "Custom network") {
        colNames = toupper(c("geneID1", "geneID2", "reactionType", 
                             "clusterID_geneID1", "clusterID_geneID2", 
                             #'superClusterID_geneID1', 'superClusterID_geneID2',
                             #'networkSimplifiedNodeDegree_geneID1', 'networkSimplifiedNodeDegree_geneID2',
                             #'superClusterSimplifiedNodeDegree_geneID1', 'superClusterSimplifiedNodeDegree_geneID2',
                             "clusterSimplifiedNodeDegree_geneID1", "clusterSimplifiedNodeDegree_geneID2"))
      } else {
        colNames = toupper(colnames(xx)[-ncol(xx)])
      }
      
      showCols = sapply(colNames, function(x) grep(paste("^",x,"$", sep=""), toupper(colnames(xx))))
      xx = xx[,showCols]
      
      if (input$lkn != "Custom network") {
        colnames(xx) = c('STARTgene', 'ENDgene', 
                         'reactionTYPE', 
                         'STARTcluster', 'ENDcluster',
                         #'STARTsuperClu', 'ENDsuperClu',
                         #'netSTARTnodeDegree', 'netENDnodeDegree',
                         #'superCluSTARTnodeDegree', 'superCluENDnodeDegree',
                         'cluSTARTnodeDegree', 'cluENDnodeDegree')
      } else {
        colnames(xx) = colnames(xx)
      }
      
      return(xx)
    })
    
    # Nodes table per cluster nad/or geneID
    output$myText2 <- renderDataTable({
      
      if (is.null(changeFileList())) return(NULL)
      
      input$selectFiles
      mytimepoint = strtoi(trimws(input$timepoint))
      input$createPalette
      
      degreeCutoff <- strtoi(trimws(input$mydegree))
      iname <- (trimws(input$nameType))
      ncluID <- strtoi(trimws(input$icluID))
      nname <- (nchar(iname))
      

      n <- isolate(animatoRNE())$n
      e <- isolate(animatoRNE())$e

      tmp = n
      if (input$lkn != "Custom network") {
        colNames = toupper(c("geneID", "shortDescription", "shortName", "MapManBin", 
                             #"networkSimplifiedNodeDegree", 
                             #'superClusterSimplifiedNodeDegree',
                             "clusterSimplifiedNodeDegree", 
                             #'superClusterID',
                             "clusterID"))
      } else {
        colNames = toupper(colnames(tmp)[-ncol(tmp)])
      }
      
      showCols = sapply(colNames, function(x) grep(paste("^",x,"$", sep=""), toupper(colnames(tmp))))
      yy = tmp[,showCols]

      if (input$lkn != "Custom network") {
        colnames(yy) = c('geneID',           # 1
                         'shortDESCRIPTION', # 2
                         'shortNAME',        # 3
                         'MapManBIN',        # 4
                         #'networkNodeDEGREE',
                         #'superCluNodeDEGREE',
                         'cluNodeDEGREE',
                         #'GENEsuperClu',
                         'GENEclu')
      } else {
        colnames(yy) = colnames(yy)
      }
      return(yy)
      
    })
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    
    
    
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    #### REFRESH SESSION AND ERROR TEXT
    #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    
    observeEvent(input$refresh, {
      print(sessionInfo())
      js$refresh();
    })
    
    output$errorTXT1 <- renderUI({ 
      # tags$a(href="https://zagorgit.github.io/", "Quick help")
      # tags$a(href="https://raw.githubusercontent.com/NIB-SI/DiNARdata/master/QuickTutorial.webm", "Quick tutorial")
      str0 <- '<br>'
      str1 <- "Nothing to see here."
      str2 <- 'You have skipped some important steps indicated in interactive menu.'
      str3 <- "Follow the required procedure to see results."
      str4 <- "In case of uncertainties read 'Quick help' or check 'Quick tutorial'."
      str5 <- "Return to START Tab."
      mytext <- HTML(paste(str0, str1, str2, str3, str4, str5, sep = '<br/>'))
      actionLink("link_to_START1", mytext)
    })
    output$errorTXT2 <- renderUI({ 
      str0 <- '<br>'
      str1 <- "Nothing to see here."
      str2 <- 'You have skipped some important steps indicated in interactive menu.'
      str3 <- "Follow the required procedure to see results."
      str4 <- "In case of uncertainties read 'Quick help' or check 'Quick tutorial'."
      str5 <- "Return to START Tab."
      mytext <- HTML(paste(str0, str1, str2, str3, str4, str5, sep = '<br/>'))
      actionLink("link_to_START2", mytext)
    })
    output$errorTXT3 <- renderUI({ 
      str0 <- '<br>'
      str1 <- "Nothing to see here."
      str2 <- 'You have skipped some important steps indicated in interactive menu.'
      str3 <- "Follow the required procedure to see results."
      str4 <- "In case of uncertainties read 'Quick help' or check 'Quick tutorial'."
      str5 <- "Return to START Tab."
      mytext <- HTML(paste(str0, str1, str2, str3, str4, str5, sep = '<br/>'))
      actionLink("link_to_START3", mytext)
    })
    output$errorTXT4 <- renderUI({ 
      str0 <- '<br>'
      str1 <- "Nothing to see here."
      str2 <- 'You have skipped some important steps indicated in interactive menu.'
      str3 <- "Follow the required procedure to see results."
      str4 <- "In case of uncertainties read 'Quick help' or check 'Quick tutorial'."
      str5 <- "Return to START Tab."
      mytext <- HTML(paste(str0, str1, str2, str3, str4, str5, sep = '<br/>'))
      actionLink("link_to_START4", mytext)
    })
    output$errorTXT5 <- renderUI({ 
      str0 <- '<br>'
      str1 <- "Nothing to see here."
      str2 <- 'You have skipped some important steps indicated in interactive menu.'
      str3 <- "Follow the required procedure to see results."
      str4 <- "In case of uncertainties read 'Quick help' or check 'Quick tutorial'."
      str5 <- "Return to START Tab."
      mytext <- HTML(paste(str0, str1, str2, str3, str4, str5, sep = '<br/>'))
      actionLink("link_to_START5", mytext)
    })
    output$errorTXT6 <- renderUI({ 
      str0 <- '<br>'
      str1 <- "Nothing to see here."
      str2 <- 'You have skipped some important steps indicated in interactive menu.'
      str3 <- "Follow the required procedure to see results."
      str4 <- "In case of uncertainties read 'Quick help' or check 'Quick tutorial'."
      str5 <- "Return to START Tab."
      mytext <- HTML(paste(str0, str1, str2, str3, str4, str5, sep = '<br/>'))
      actionLink("link_to_START6", mytext)
    })
    output$errorTXT7 <- renderUI({ 
      str0 <- '<br>'
      str1 <- "Nothing to see here."
      str2 <- 'You have skipped some important steps indicated in interactive menu.'
      str3 <- "Follow the required procedure to see results."
      str4 <- "In case of uncertainties read 'Quick help' or check 'Quick tutorial'."
      str5 <- "Return to START Tab."
      mytext <- HTML(paste(str0, str1, str2, str3, str4, str5, sep = '<br/>'))
      actionLink("link_to_START7", mytext)
    })
    output$errorTXT8 <- renderUI({ 
      str0 <- '<br>'
      str1 <- "Nothing to see here."
      str2 <- 'You have skipped some important steps indicated in interactive menu.'
      str3 <- "Follow the required procedure to see results."
      str4 <- "In case of uncertainties read 'Quick help' or check 'Quick tutorial'."
      str5 <- "Return to START Tab."
      mytext <- HTML(paste(str0, str1, str2, str3, str4, str5, sep = '<br/>'))
      actionLink("link_to_START8", mytext)
    })
    output$errorTXT9 <- renderUI({ 
      str0 <- '<br>'
      str1 <- "Nothing to see here."
      str2 <- 'You have skipped some important steps indicated in interactive menu.'
      str3 <- "Follow the required procedure to see results."
      str4 <- "In case of uncertainties read 'Quick help' or check 'Quick tutorial'."
      str5 <- "Return to START Tab."
      mytext <- HTML(paste(str0, str1, str2, str3, str4, str5, sep = '<br/>'))
      actionLink("link_to_START9", mytext)
    })
    output$errorTXT10 <- renderUI({ 
      str0 <- '<br>'
      str1 <- "Nothing to see here."
      str2 <- 'You have skipped some important steps indicated in interactive menu.'
      str3 <- "Follow the required procedure to see results."
      str4 <- "In case of uncertainties read 'Quick help' or check 'Quick tutorial'."
      str5 <- "Return to START Tab."
      mytext <- HTML(paste(str0, str1, str2, str3, str4, str5, sep = '<br/>'))
      actionLink("link_to_START10", mytext)
    })
    output$errorTXT11 <- renderUI({ 
      str0 <- '<br>'
      str1 <- "Nothing to see here."
      str2 <- 'You have skipped some important steps indicated in interactive menu.'
      str3 <- "Follow the required procedure to see results."
      str4 <- "In case of uncertainties read 'Quick help' or check 'Quick tutorial'."
      str5 <- "Return to START Tab."
      mytext <- HTML(paste(str0, str1, str2, str3, str4, str5, sep = '<br/>'))
      actionLink("link_to_START11", mytext)
    })
    output$errorTXT12 <- renderUI({ 
      str0 <- '<br>'
      str1 <- "Nothing to see here."
      str2 <- 'You have skipped some important steps indicated in interactive menu.'
      str3 <- "Follow the required procedure to see results."
      str4 <- "In case of uncertainties read 'Quick help' or check 'Quick tutorial'."
      str5 <- "Return to START Tab."
      mytext <- HTML(paste(str0, str1, str2, str3, str4, str5, sep = '<br/>'))
      actionLink("link_to_START12", mytext)
    })
    output$errorTXT13 <- renderUI({ 
      str0 <- '<br>'
      str1 <- "Nothing to see here."
      str2 <- 'You have skipped some important steps indicated in interactive menu.'
      str3 <- "Follow the required procedure to see results."
      str4 <- "In case of uncertainties read 'Quick help' or check 'Quick tutorial'."
      str5 <- "Return to START Tab."
      mytext <- HTML(paste(str0, str1, str2, str3, str4, str5, sep = '<br/>'))
      actionLink("link_to_START13", mytext)
    })

    #   output$errorTXT1 <- renderUI({ 
    #   h2(span(paste0("Nothing to see here, yet. ", '\n', "First create custom palette.")),
    #      style = "color:green")
    #   # h2(span(paste0("")), 
    #   #    style = "color:green")
    # })
    # output$errorTXT2 <- renderUI({ 
    #   
    #   h2(span(paste0("Nothing to see here. ", '</br>', 'You have skipped some steps.','</br>', "Please follow the required procedure to see results.")),
    #      style = "color:green")
    #   # h2(span(paste0("")), 
    #   #    style = "color:green")
    # })
    
    # datasetInput <- reactive({
    # 
    #   if (input$dataset == ''){
    #     return(NULL)
    #   }
    #   
    #   if (input$dataset == 'AthGSE56094'){
    #     url = "https://github.com/NIB-SI/tstD/raw/master/AthGSE56094.rar"
    #     download(url, dest=paste0(getwd(), '/temp'), mode="wb") 
    #   }
    #   if (input$dataset == 'SoTubGSE58593'){
    #     url = "https://github.com/NIB-SI/tstD/raw/master/SoTubGSE58593.rar"
    #     download(url, dest=paste0(getwd(), '/temp'), mode="wb") 
    #   }
    #   if (input$dataset == 'Network'){
    #     url = "https://github.com/NIB-SI/tstD/blob/master/STRINGwrky33.rar?raw=true"
    #     download(url, dest=paste0(getwd(), '/temp'), mode="wb") 
    #   }
    #   #return(temp)
    # })
    # 
    # output$downloadData <- downloadHandler(
    #   filename = function() {
    #     paste0(input$dataset, '.rar')
    #   },
    #   content = function(file) {
    #     file.copy(from = datasetInput(), to = file,  overwrite = TRUE)
    #   }
    # )
    

    getPage<-function() {
      # return(includeHTML("http://conferences.nib.si/camo2017/78aw64VzX1yAk7Yz/FdLI6cnm2jZ54u76/jI7Fw9J8zf6xO51q/jI7Fw9J8zf6xO51q.html"))
      return(includeHTML("https://github.com/NIB-SI/DiNAR/raw/master/DiNARscripts/jI7Fw9J8zf6xO51q"))
    }
    
    output$inc<-renderUI({
      getPage()
    })
    
    
    myLog <- reactive({
      
      # trigger
      input$createPalette

      con <- file("test.log")
      sink(con, append=TRUE)
      sink(con, append=TRUE, type="message")
      print("\n")
      print("# ## #### ######## ################ ################################")
      print("# ## #### ######## ################ ################################")
      print("The current date: ")
      print(Sys.Date())
      print("Session info: ")
      print(sessionInfo())
      print("# ## #### ######## ################ ################################")

        print("LKN: ")
        if(input$lkn != '') print(input$lkn)
        print("Node table dimension:")
        if (!is.null(getNodes)) print(dim(isolate(getNodes())))
        print("Edge table dimension:")
        if (!is.null(getEdges)) print(dim(isolate(getEdges())))
        print("Number of clusters:")
        if (!is.null(maxCLU)) print((maxCLU()))
        print("#### ####")
        print("Files: ")
        if (length(myFileNames)) print(myFileNames)
        print("Use:")
        print((NEWfLIST()))
        print("Number of conditions:")
        if (!is.null(maxTP)) print((maxTP()))
        print("#### ####")
        print("ID column: ")
        if (!is.null(myIDcol)) print((myIDcol()))
        print("stat test column: ")
        if (!is.null(selectedPval)) print((selectedPval()))
        print("logFC or similar column: ")
        if (!is.null(selectedLogFC)) print((selectedLogFC()))
        print("stat test cut-off: ")
        if (!is.null(cutP)) print((cutP()))
        print("second cut-off, colourng: ")
        if (!is.null(cutFC)) print((cutFC()))
        print("#### ####")
        print("palette")
        print(isolate(mypalette())$palette)
        print("upper/lower boundary palette threshold:")
        if (!is.null(colMaxMin)) print((colMaxMin()))
        print("#### ####")
        # print("node ID")
        # iname = trimws(input$nameType)
        # nname = nchar(iname)
        # if ((nname > 0) & (iname !='geneX') & (iname !='')) print(iname)
        # print("cluster ID")
        # print(strtoi(trimws(input$icluID)))
        # print("#### ####")
        print("d3movie rendering threshold, max edges, max nodes, speed")
        mythList = howPatient()
        print(strtoi(mythList$eThreshold))
        print(strtoi(mythList$nThreshold))
        print(as.numeric(input$varSpeed))
        print("# ## #### ######## ################ ################################")
        print("# ## #### ######## ################ ################################")
        print(mem_used())

      # Restore output to console
      sink()
      sink(type="message")
      close(con)
    }) 
    
    output$log <- renderPrint({
      if (input$createPalette == 0) {
        file.create("test.log")
        mypoem = paste0("poem", sample(6, replace = TRUE)[1], ".txt")
        # print(mypoem)
        x<-read.delim(mypoem, sep='\n')
        names(x) = 'Return to START and follow the steps or read a poem'
        return(x)
      } else {
        myLog()
        x<-read.delim("test.log", sep='\n')
        names(x) = 'LOG'
        return(x)
      }
    })
    
    output$downloadLOG <- downloadHandler(
      
      filename = function() {
        paste0(gsub(" ", "_", Sys.time()), '_log.txt')
      },
      
      content = function(file) {
        if (input$createPalette != 0) {
          file.copy("test.log", file)
        } else {
          file.copy("poem.txt", file)
        }
      }
    )
    
  # save.image("test.RData")  

  }) 

# THE END
    
