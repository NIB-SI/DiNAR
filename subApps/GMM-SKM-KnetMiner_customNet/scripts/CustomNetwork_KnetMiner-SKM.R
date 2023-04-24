# if (require(devtools)) install.packages("devtools")
# devtools::install_github('andrewsali/shinycssloaders')
# devtools::install_github("AnalytixWare/ShinySky")
# install.packages("fontawesome")

# https://fontawesome.com/v4/icons/


library(shiny)
library(shinycssloaders)
library(shinysky)
library(fontawesome)

# Define UI
ui <- fluidPage(
  

  # App title
  titlePanel("Custom Network creation"),
  
  # Sidebar layout
  sidebarLayout(
    
    # Sidebar panel
    sidebarPanel(
      


      checkboxInput("header1", "My file containes header", TRUE),
      

      radioButtons("sep1", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      

      radioButtons("quote1", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      

      fileInput("file1", "Choose nodes file export from SKM PSS",
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
                   selected = ","),
      

      radioButtons("quote2", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      fileInput("file2", "Choose edges file export from SKM PSS",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),

      
      tags$hr(),
      
      fileInput("file3", "Choose json export from KnetMiner",
                multiple = FALSE,
                accept = c("json")),
      
      tags$hr(),
      
      p("Check both Import Tabs and then click button to confirm selection"),
      actionButton(inputId = "checkButton", 
                   "Checked", 
                   label = tags$div(HTML(paste(tags$b(tags$span(style="color:#009900", "Checked")), sep = ""))),
                   style="color: #C8E500;"),
      br(),
      br(),
      p("Click to generate Custom Network Nodes and Connections Tables"),
      actionButton(inputId = "goButton", 
                   label = tags$div(HTML(paste(tags$b(tags$span(style="color:#db205b", "Generate!")), sep = ""))),
                   style="color: #C8E500;"),
        


      tags$hr(),
      
      HTML('<i class="fa fa-code" aria-hidden="true"></i>', 
           paste(tags$a("Vlick for more info at GitHub", 
                        href = "https://github.com/NIB-SI/DiNAR/tree/master/subApps/KnetMiners'_customNet", 
                        target = "_blank"))),
      
      

      
      
    ),
    

    mainPanel(
      

      navbarPage('Options',
      tabPanel(title = 'PSS-import',
               value = "1",
               br(),
               p(strong("Upload"), " files and ", strong("select "), strong("id"), " column and columns of interest, in correct order, that will serve as: ", strong("geneID/nodeID, shortName, shortDescription, MapManBin"), ", and ", strong("geneID1, geneID2, reactionType"), '.'),
               p("If undecisive, leave as it is."), 
               br(),
               DT::dataTableOutput('ex1'),
               verbatimTextOutput('y22'), # tableOutput("contents2")
               br(),
               br(),
               DT::dataTableOutput('ex2'),
               verbatimTextOutput('y33')
      ),
      tabPanel(title = 'KnetMiner-import',
               value = "2",
               br(),
               p(strong("Upload"), " files and ", strong("select "), strong("id"), " column and columns of interest, in correct order, that will serve as: ", strong("geneID/nodeID (pid), shortName, shortDescription, MapManBin"), strong("x"), " and ", strong("y"), " coordinates,", " and ", strong("geneID1, geneID2, reactionType"), '.'),
               p("If undecisive, leave as it is."), 
               br(),
               DT::dataTableOutput('ex3'),
               verbatimTextOutput('y55'), # tableOutput("contents2")
               br(),
               br(),
               DT::dataTableOutput('ex4'),
               verbatimTextOutput('y66')
      ),


      tabPanel(title = 'CustomNetwork-export',
               value = "3",
               br(),
               br(),
               conditionalPanel(
                 condition = "(output.goButton!=0 & output.checkButton!=0)",
                 busyIndicator(text = "Loading, please wait...", wait = 400), 
                 DT::dataTableOutput('CustomVertices'), # %>% withSpinner(color="#A9F5F2", proxy.height = "50px"),
                 br(),
                 br(),
                 DT::dataTableOutput('CustomEdges'),
                 #br(),
                 #br(),
                 #DT::dataTableOutput('y46'),
                 #br(),
                 #br(),
                 #DT::dataTableOutput('y47')
                 ),
               br()
               ),
      
      id = "tabselected")
      
      
      
      
    )
    
  )
)


server <- function(input, output, session) {
  
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
    
    DT::datatable(df1, options = list(pageLength = 5), selection = list(target = 'column', selected = c(1,2,5,4,7)))
    

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
    
    DT::datatable(df2, options = list(pageLength = 5), selection = list(target = 'column', selected = c(1,2,3)))
  })
  

  output$y33 = renderPrint({
    if (!is.na(req(input$file2))[1]) {
      cat("Columns selected: ")
      input$ex2_columns_selected      
    }
    })
  
  output$ex3 <- DT::renderDataTable({
    
    req(input$file3)
    
    tryCatch(
      {
       
        net = jsonlite::fromJSON(input$file3$datapath)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    e = net$elements$edges$data
    n = cbind(net$elements$nodes$data, net$elements$nodes$position)
    
    DT::datatable(n, options = list(pageLength = 5), selection = list(target = 'column', selected = c(13,4, 14, 1,3, 17,18)))
  })
  output$ex4 <- DT::renderDataTable({
    
    req(input$file3)
    
    tryCatch(
      {
        
        net = jsonlite::fromJSON(input$file3$datapath)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    e = net$elements$edges$data
    n = cbind(net$elements$nodes$data, net$elements$nodes$position)
    
    DT::datatable(e, options = list(pageLength = 5), selection = list(target = 'column', selected = c(4,7,5)))
  })
  
  
  output$y55 = renderPrint({
    if (!is.na(req(input$file3))[1]) {
      cat("Columns selected: ")
      input$ex3_columns_selected      
    }
  })
  output$y66 = renderPrint({
    if (!is.na(req(input$file3))[1]) {
      cat("Columns selected: ")
      input$ex4_columns_selected      
    }
  })
  
  
  ntextChecked <- eventReactive(input$checkButton, {
    return(TRUE)
  })
  
  
  ntextGO <- eventReactive(input$goButton, {
    return(TRUE)
  })
  
  myoutputPSS = reactive({
    if (!is.na(req(input$file2))[1] & (!is.na(req(input$file1))[1]) & ntextGO() & ntextChecked()) {
      
      tryCatch(
        {
          
          PSSe <- read.csv(input$file2$datapath,
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
          
          PSSn<- read.csv(input$file1$datapath,
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



      
      if ((length(input$ex1_columns_selected) == 5) & (length(input$ex2_columns_selected) == 3)) {
        # geneID 	shortName 	shortDescription 	MapManBin
        PSSn = PSSn[, input$ex1_columns_selected]
        # geneID1 	geneID2 	reactionType
        PSSe = PSSe[, input$ex2_columns_selected]
        
        # PSSn = PSSn[, c(1,2,5,4,7)]
        # PSSe = PSSe[, c(1,2,3)]
        
        en = c("geneID1", "geneID2", "reactionType")
        colnames(PSSe) = en
        nn = c("id", "geneID", "shortName", "shortDescription", "MapManBin")
        colnames(PSSn) = nn
        
        PSSn$geneID = gsub('<b>', '', gsub("</b>","_",  PSSn$geneID))
        PSSn$MapManBin = gsub(",","|||",  PSSn$MapManBin)
        
        realID = PSSn[, 1:2]
        colnames(PSSe)[grep('geneID1', colnames(PSSe))] = 'id'
        colnames(realID)[grep('geneID', colnames(realID))] = 'geneID1'
        PSSe = merge(PSSe, realID, by = 'id', all.x = TRUE, all.y = FALSE)
        PSSe = PSSe[, -grep('^id$', colnames(PSSe))]
        colnames(PSSe)[grep('geneID2', colnames(PSSe))] = 'id'
        colnames(realID)[grep('geneID1', colnames(realID))] = 'geneID2'
        PSSe = merge(PSSe, realID, by = 'id', all.x = TRUE, all.y = FALSE)
        PSSe = PSSe[, -grep('^id$', colnames(PSSe))]
        ind = match(en, colnames(PSSe))
        PSSe = PSSe[, ind]
        
        PSSn = PSSn[, -grep('^id$', colnames(PSSn))]
        PSSn$name = PSSn$geneID
        
        PSSn$expressed = 1
        
        PSSe$exists = 1
        
        PSSe$from = PSSe$geneID1
        PSSe$to = PSSe$geneID2

        
        

        
        g <- igraph::graph.data.frame(PSSe, vertices = PSSn, directed = TRUE)
        
        # igraph::vertex_attr_names(g)
        # igraph::edge_attr_names(g)
        
        igraph::E(g)$geneID1 =  igraph::E(g)$from
        igraph::E(g)$geneID2 =  igraph::E(g)$to
        igraph::V(g)$geneID =  igraph::V(g)$name
        
        
        l1 = igraph::layout_on_grid(g, dim = 2)
        

        l2 = igraph::layout_with_kk(g, coords = l1, dim = 2,
                                    maxiter = 999 * igraph::vcount(g),
                                    epsilon = 0, kkconst = igraph::vcount(g),
                                    minx = NULL, maxx = NULL,
                                    miny = NULL, maxy = NULL,
                                    minz = NULL,maxz = NULL)

        z = round(igraph::ecount(g)/igraph::vcount(g))
        l2 = l2*2*z
        colnames(l2) = c('x', 'y')
        l2 = as.data.frame(l2)
        l2$geneID = igraph::V(g)$name
        
        igraph::V(g)$x = l2$x
        igraph::V(g)$y = l2$y
        # plot(0, type = "n",
        #      axes = FALSE,
        #      xlim = extendrange(igraph::V(g)$x),
        #      ylim = extendrange(igraph::V(g)$y),
        #      xlab = '',
        #      ylab = '')
        # plot(g, layout = cbind(igraph::V(g)$x, igraph::V(g)$y),
        #      edge.label = '',
        #      vertex.label = igraph::V(g)$shortName,
        #      # vertex.size = rep(1, vcount(g)),
        #      vertex.color = 'grey45',
        #      rescale = FALSE, add = TRUE,
        #      vertex.label.cex = 0.75,
        #      edge.arrow.size = 0.25,
        #      edge.arrow.width = 0.25,
        #      edge.lty = 'solid',
        #      edge.color = 'gray',
        #      edge.width = 0.25,
        #      edge.label.cex = 0.25)

        
        
      igraph::V(g)$clusterID <- rep(2, igraph::vcount(g))
      igraph::V(g)$x <- as.numeric(l2[,1])
      igraph::V(g)$y <- as.numeric(l2[,2])
      mydeg = igraph::degree(g, loops = FALSE, normalized = FALSE,  mode = "all")
      igraph::V(g)$clusterSimplifiedNodeDegree <- mydeg
      
      igraph::vertex_attr_names(g)
      igraph::edge_attr_names(g)
      
        
      igraph::E(g)$clusterID_geneID1 = igraph::V(g)$clusterID[match(igraph::E(g)$geneID1,  igraph::V(g)$geneID)]
      igraph::E(g)$clusterID_geneID2 = igraph::V(g)$clusterID[match(igraph::E(g)$geneID2,  igraph::V(g)$geneID)]
      igraph::E(g)$clusterSimplifiedNodeDegree_geneID1 = igraph::V(g)$clusterSimplifiedNodeDegree[match(igraph::E(g)$geneID1,  igraph::V(g)$geneID)]
      igraph::E(g)$clusterSimplifiedNodeDegree_geneID2 = igraph::V(g)$clusterSimplifiedNodeDegree[match(igraph::E(g)$geneID2,  igraph::V(g)$geneID)]
        
        
      v = igraph::vertex_attr_names(g) 
      e = igraph::edge_attr_names(g)
        
      df1 = matrix(NA, igraph::vcount(g), length(v))
      for (i in 1:length(v)) {
        df1[,i] = igraph::vertex_attr(g,v[i])
      }
      df1 = as.data.frame(df1, stringsAsFactors = FALSE)
      colnames(df1) = v
      
      
      colNames = toupper(c("geneID", "shortDescription", "shortName", "MapManBin",
                           "clusterID", "x", "y", "clusterSimplifiedNodeDegree", "expressed"))
      
      importantColsE = unlist(sapply(colNames,
                                     function(x) grep(paste("^",x,"$", sep = ""),
                                                      toupper((v)))))
      
      df1 = df1[,importantColsE]
      
      df2 = matrix(NA, igraph::ecount(g), length(e))
      for (i in 1:length(e)) {
        df2[,i] = igraph::edge_attr(g,e[i])
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
        
      
      PSS = list(df1, df2)
      names(PSS) = c('PSSvertices', 'PSSedges')
      return(PSS)
        
        
      }
    }
  })
  
 
  myoutputKM = reactive({
    if (!is.na(req(input$file3))[1] & ntextGO() & ntextChecked()) {
    
    tryCatch(
      {
    
        net = jsonlite::fromJSON(input$file3$datapath)
    
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
 
    
    KMe = net$elements$edges$data
    KMn = cbind(net$elements$nodes$data, net$elements$nodes$position)
      
      
      if ((length(input$ex3_columns_selected) == 7) & (length(input$ex4_columns_selected) == 3)) {
        # geneID 	shortName 	shortDescription 	MapManBin
        KMn = KMn[, input$ex3_columns_selected]
        # geneID1 	geneID2 	reactionType
        KMe = KMe[, input$ex4_columns_selected]
        

        # KMn = n[, c(13,4, 14, 1,3, 17,18)]
        # KMe = e[, c(4,7,5)]
        
        
        en = c("geneID1", "geneID2", "reactionType")
        colnames(KMe) = en
        nn = c("id", "geneID", "shortName", "shortDescription", "MapManBin", 'x', 'y')
        colnames(KMn) = nn
        
        # KMn = KMn[KMn$geneID != '', ]
        # KMe = KMe[KMe$geneID1 %in% KMn$id & KMe$geneID2 %in% KMn$id, ]
        KMn$geneID[KMn$geneID == ''] = KMn$id[KMn$geneID == '']
        # table(duplicated(KMn$geneID))
        
        realID = KMn[, 1:2]
        colnames(KMe)[grep('geneID1', colnames(KMe))] = 'id'
        colnames(realID)[grep('geneID', colnames(realID))] = 'geneID1'
        KMe = merge(KMe, realID, by = 'id', all.x = TRUE, all.y = FALSE)
        KMe = KMe[, -grep('^id$', colnames(KMe))]
        colnames(KMe)[grep('geneID2', colnames(KMe))] = 'id'
        colnames(realID)[grep('geneID1', colnames(realID))] = 'geneID2'
        KMe = merge(KMe, realID, by = 'id', all.x = TRUE, all.y = FALSE)
        KMe = KMe[, -grep('^id$', colnames(KMe))]
        ind = match(en, colnames(KMe))
        KMe = KMe[, ind]
        
        KMn = KMn[, -grep('^id$', colnames(KMn))]
        KMn$name = KMn$geneID
        
        KMn$expressed = 1
        
        KMe$exists = 1
        
        KMe$from = KMe$geneID1
        KMe$to = KMe$geneID2
        
        KMn$y = -(KMn$y)
        
        
        
        g <- igraph::graph.data.frame(KMe, vertices = KMn, directed = TRUE)
        
        # igraph::vertex_attr_names(g)
        # igraph::edge_attr_names(g)
        
        igraph::E(g)$geneID1 =  igraph::E(g)$from
        igraph::E(g)$geneID2 =  igraph::E(g)$to
        igraph::V(g)$geneID =  igraph::V(g)$name
        
        
# 
#         plot(0, type = "n",
#              axes = FALSE,
#              xlim = extendrange(igraph::V(g)$x),
#              ylim = extendrange(igraph::V(g)$y),
#              xlab = '',
#              ylab = '')
#         plot(g, layout = cbind(igraph::V(g)$x, igraph::V(g)$y),
#              edge.label = '',
#              vertex.label = igraph::V(g)$shortName, # igraph::V(g)$name
#              # vertex.size = rep(1, vcount(g)),
#              vertex.color = 'grey45',
#              rescale = FALSE, add = TRUE,
#              vertex.label.cex = 0.75,
#              edge.arrow.size = 0.25,
#              edge.arrow.width = 0.25,
#              edge.lty = 'solid',
#              edge.color = 'gray',
#              edge.width = 0.25,
#              edge.label.cex = 0.25)

        # igraph::vertex_attr_names(g)
        # igraph::edge_attr_names(g)
        
        
        igraph::V(g)$clusterID <- rep(1, igraph::vcount(g))
        mydeg = igraph::degree(g, loops = FALSE, normalized = FALSE,  mode = "all")
        igraph::V(g)$clusterSimplifiedNodeDegree <- mydeg
        
        igraph::E(g)$clusterID_geneID1 = igraph::V(g)$clusterID[match(igraph::E(g)$geneID1,  igraph::V(g)$geneID)]
        igraph::E(g)$clusterID_geneID2 = igraph::V(g)$clusterID[match(igraph::E(g)$geneID2,  igraph::V(g)$geneID)]
        igraph::E(g)$clusterSimplifiedNodeDegree_geneID1 = igraph::V(g)$clusterSimplifiedNodeDegree[match(igraph::E(g)$geneID1,  igraph::V(g)$geneID)]
        igraph::E(g)$clusterSimplifiedNodeDegree_geneID2 = igraph::V(g)$clusterSimplifiedNodeDegree[match(igraph::E(g)$geneID2,  igraph::V(g)$geneID)]
        
        
        v = igraph::vertex_attr_names(g) 
        e = igraph::edge_attr_names(g)
        
        df1 = matrix(NA, igraph::vcount(g), length(v))
        for (i in 1:length(v)) {
          df1[,i] = igraph::vertex_attr(g,v[i])
        }
        df1 = as.data.frame(df1, stringsAsFactors = FALSE)
        colnames(df1) = v
        
        
        colNames = toupper(c("geneID", "shortDescription", "shortName", "MapManBin",
                             "clusterID", "x", "y", "clusterSimplifiedNodeDegree", "expressed"))
        
        importantColsE = unlist(sapply(colNames,
                                       function(x) grep(paste("^",x,"$", sep = ""),
                                                        toupper((v)))))
        
        df1 = df1[,importantColsE]
        
        df2 = matrix(NA, igraph::ecount(g), length(e))
        for (i in 1:length(e)) {
          df2[,i] = igraph::edge_attr(g,e[i])
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
        
        
        KM = list(df1, df2)
        names(KM) = c('KMvertices', 'KMedges')
        
        
        # print(names(KM))
        
        return(KM)

        
        
      }
    }
  })



  myoutputNodes = reactive({
    if (!is.na(req(input$file2))[1] & (!is.na(req(input$file1))[1])  & !is.na(req(input$file3))[1] & ntextGO() & ntextChecked()) {

     PSS = (isolate(myoutputPSS()))
     KM = (isolate(myoutputKM()))
     
     # print('####')
     # 
     # print(names(KM))
     # print(names(PSS))
     
     nodes = rbind(PSS$PSSvertices, KM$KMvertices)
     # edges = cbind(PSSedges, KMedges)


    return(nodes)


    }
  })
  
  myoutputEdges = reactive({
    if (!is.na(req(input$file2))[1] & (!is.na(req(input$file1))[1])  & !is.na(req(input$file3))[1] & ntextGO() & ntextChecked()) {
      
      PSS = (isolate(myoutputPSS()))
      KM = (isolate(myoutputKM()))
      
      # nodes = cbind(PSSvertices, KMvertices)
      edges = rbind(PSS$PSSedges, KM$KMedges)
      
      
      return(edges)
      
      
    }
  })
  
  


  
  output$CustomVertices <- DT::renderDataTable(DT::datatable({myoutputNodes()},
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
  
  output$CustomEdges <- DT::renderDataTable(DT::datatable({myoutputEdges()},
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
    if( (!is.na(req(input$file2))[1]) & (!is.na(req(input$file1))[1]) & (!is.na(req(input$file3))[1])  & ntextGO() & ntextChecked() & (!is.null(myoutputNodes())) & (!is.null(myoutputEdges()))){
      updateNavbarPage(session, inputId = 'tabselected', selected = "3")
    }
  })
  

  
}

# Shiny app
shinyApp(ui, server)
