# ui.R zagor

# # # # control points: # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# selectedLKN 
# UserNetwork 
# fileUploaded
# # # # # # # # # # # # # # # #  # # # # # # # # # # # # # # # # # # # # # # # #

shinyUI(
  fluidPage(includeCSS("bootstrap.css"),
            tags$head(tags$style(HTML('
                                      
                                      .shinysky-busy-indicator {
                                      z-index: 1200;
                                      
                                      }'))),
            tags$head(tags$style(HTML('

                        .modal-lg {
                        width: auto; height: 500px;

                        }
                      '))),
            tags$head(tags$link(rel="shortcut icon", href="dinarNoBg.ico")),
            headerPanel(h6(title = "Differential Network Analysis in R"), windowTitle = "DiNAR"),

    titlePanel(
      fluidRow(
        # http://shiny.rstudio.com/tutorial/lesson2/
        column(3, tags$a(href="http://conferences.nib.si/DiNAR/", img(width = 261, src = "clu.jpg", class = "pull-left"))#,
               # br()
        ),
        column(6,  tags$div(HTML(paste0('<h1><i><b>Di</b>fferential <b>N</b>etwork <b>A</b>nalysis in <span style="color: #0000CC"><b>R</b></span></i></h1>'))), 
               offset = 1, class = "center")
      )
    ),
    sidebarLayout(
      sidebarPanel(
        ########  ########  ########  ########  ########  ########  ######## ###
        ######## choose Large Knowledge Network (LKN) ######## ######## ######## 
        conditionalPanel(
          condition = "((!input.createPalette) & (!output.fileUploaded)) | (input.tabselected==13)",
          #helpText(span("Please select Large Knowledge Network"), style = "color:blue"),
          box(width = NULL, 
            title = uiOutput("helpLKN"),
            selectInput(inputId = "lkn",
                        label = NULL,
                        choices = c("", 
                                    "Arabidopsis thaliana",
                                    "Solanum tuberosum",
                                    "Custom network",
                                    "QuickAppTest(Ath123)"),
                        selected = "")
          )
        ),
        
        
        br(), #tags$hr(),
        
        
        ########  ########  ########  ########  ########  ########  ######## ###
        ######## IF Custom network is selected upload nodes and edges tables ### 
        conditionalPanel(
          condition = "(((!input.createPalette) & (!output.fileUploaded) & (output.UserNetwork))) | (input.tabselected==13)",
          box(width = NULL, 
              title = uiOutput("helpUserNodes"),
              fileInput(inputId = 'fileN', 
                        label = NULL, 
                        accept=c('text/csv', 
                                 'text/comma-separated-values,text/plain', 
                                 '.csv'), multiple = FALSE)
              ),
          box(width = NULL, 
              title = uiOutput("helpUserEdges"),
              fileInput(inputId = 'fileE', 
                        label = NULL, 
                        accept=c('text/csv', 
                                 'text/comma-separated-values,text/plain', 
                                 '.csv'), multiple = FALSE)
              )
        ),
        br(), #tags$hr(),
        
        
        ########  ########  ########  ########  ########  ########  ######## ###        
        ######## Define file format (header and separator) ######## ######## ###
        conditionalPanel(
          condition = "((!output.fileUploaded) & (output.selectedLKN>-1) & (!input.createPalette))  | (input.tabselected==13)",
          
          box(width = NULL, 
              title = uiOutput("helpHeader"),
              checkboxInput(inputId = 'header', 
                            label = 'Header',
                            value = TRUE)
          ),
          
          box(width = NULL, 
              title = uiOutput("helpSeparator"),
              radioButtons(inputId = 'sep', 
                           label = 'Separator',
                           choices = c(Comma=',',
                             Semicolon=';',
                             Tab='\t'),
                           selected  = '\t')
          )
        ),
        
        
        ########  ########  ########  ########  ########  ########  ######## ###
        ######## Upload experimental data files ######## ######## ######## #####
        conditionalPanel(
          condition = "((!input.createPalette) & (output.selectedLKN>-1) & (!output.TESTNetwork))  | (input.tabselected==13)",
          box(width = NULL, 
              title = uiOutput("helpExperimentalDataUpload"),
            fileInput(inputId = 'file1', 
                      label = NULL, 
                      accept=c('text/csv', 
                               'text/comma-separated-values,text/plain', 
                               '.csv',
                               '.tsv'), 
                      multiple = TRUE)
            )
        ),

        ########  ########  ########  ########  ########  ########  ######## ###
        ######## Define important columns ######## ######## ######## ######## ##
        ######## ########  i.e. IDs, adjusted p-values and logFCs ######## #####
        ######## ######## also select numerical thresholds to cut/filter data ###
        conditionalPanel(
          condition = "((output.fileUploaded) & (output.selectedLKN>-1) & (!input.createPalette))  | (input.tabselected==13)",
          box(width = NULL, 
              title = uiOutput("helpColID"),
              uiOutput("selectID")
          ),
          box(width = NULL, 
              title = uiOutput("helpColPval"),
              uiOutput("cutoff1")
          ),
          uiOutput("cutoffval1"),
          box(width = NULL, 
              title = uiOutput("helpCollogFC"),
              uiOutput("cutoff2")
            ),
          uiOutput("cutoffval2")
        ),
        
        ########  ########  ########  ########  ########  ########  ######## ###
        ######## Select files for analysis ######## ######## ###
        conditionalPanel(
          condition = "((!input.createPalette) & (output.selectedLKN>-1) & (output.fileUploaded))  | (input.tabselected==13)",
          box(width = NULL, 
              title = uiOutput("selectFileshelp"),
              uiOutput("selectFiles")
          )
        ),
        
        br(), #tags$hr(),
        conditionalPanel(
          condition = "((output.selectedLKN>-1) & (output.fileUploaded) & (input.tabselected!=12) & (input.tabselected!=11) & (input.tabselected!=10) & (input.tabselected!=15) & (input.tabselected!=16))  | (input.tabselected==13)",
          actionButton(inputId = "createPalette", label = tags$div(HTML(paste(tags$b(tags$span(style="color:#009900", "Proceed!")), sep = ""))),
                       style="color: #C8E500;")
        ),
        ########  ########  ########  ########  ########  ########  ######## ###
        ######## Create custom palette, from three colours ######## ######## ###
        br(), #tags$hr(),
        conditionalPanel(
          condition = "((output.selectedLKN>-1) & (output.fileUploaded) & (input.tabselected!=12) & (input.tabselected!=11) & (input.tabselected!=10) & (input.tabselected!=15) & (input.tabselected!=16))  | (input.tabselected==13)",
          # actionButton(inputId = "createPalette", label = tags$div(HTML(paste(tags$b(tags$span(style="color:#009900", "Proceed!")), sep = ""))), 
          #              style="color: #C8E500;"),#'padding:4px; font-size:80%') # flow control button
          box(width = NULL, 
              title = uiOutput("helpPalette")
          ),
          htmlOutput("helpPalette2"),
          colourpicker::colourInput("col1", "Selected lower boundary colour", "#003366"),
          colourpicker::colourInput("col2", "Selected intermediate colour", "#EBEBEB"),
          colourpicker::colourInput("col3", "Selected upper boundary colour", "#AA1111"),
          uiOutput("colMaxMinPalette")#,
          # actionButton(inputId = "createPalette", label = tags$div(HTML(paste(tags$b(tags$span(style="color:#009900", "Proceed!")), sep = ""))),
          #              style="color: #C8E500;")#'padding:4px; font-size:80%') # flow control button
        ),
        
        
        br(), #tags$hr(),
        

        ########  ########  ########  ########  ########  ########  ######## ###
        ######## Time slider for animation ######## ######## ######## ######## #
        conditionalPanel(
          condition =  "((output.selectedLKN>-1) & (output.fileUploaded==1) & (input.createPalette!=0))  | (input.tabselected==13)",
          box(width = NULL,
              title = uiOutput("animatoRtime"),
              uiOutput("animatoRslider")
          ),
          box(width = NULL,
              title = uiOutput("stepSelection"),
              uiOutput("mystep")
          )
        ),
        # br(),
        # br(),
        
        
        ########  ########  ########  ########  ########  ########  ######## ### 
        ########  search and filter options: geneID and cluID ######## ######## 
        conditionalPanel(
          condition = "((output.selectedLKN>-1) & (output.fileUploaded==1) & (input.createPalette!=0))  | (input.tabselected==13)",
          br(), #tags$hr(),
          box(width = NULL, 
              title = uiOutput("geneNameSelection"),
              textInput(inputId = 'nameType', 
                        label = NULL, #'Search by gene ID. Case sensitive!', 
                        value = 'geneX')
          )
          ),
          conditionalPanel(
            condition = "((output.selectedLKN>-1) & (output.fileUploaded==1) & (input.createPalette!=0))  | (input.tabselected==13)",
            box(width = NULL, 
                title = uiOutput("clusterIDselection"),
                uiOutput("icluID")
            )
          ),
        conditionalPanel(
          condition = "((output.selectedLKN>-1) & (output.fileUploaded==1) & (input.createPalette!=0))  | (input.tabselected==13)",
            box(width = NULL, 
                title = uiOutput("minimalClusterDegreeSelection"),
                uiOutput("mydegree")
            )
          ),
        conditionalPanel(
          condition = "((output.selectedLKN>-1) & (output.fileUploaded==1) & (input.createPalette!=0))  | (input.tabselected==13)",
            box(width = NULL, 
                title = uiOutput("timePointSelection"),
                uiOutput("timepoint")
            )
        ),
        
        
        br(), #tags$hr(),
        br(),
        
        # ########  ########  ########  ########  ########  ########  ######## ###
        # ######## Time slider for animation ######## ######## ######## ######## #
        # conditionalPanel(
        #   condition =  "((output.selectedLKN>-1) & (output.fileUploaded==1) & (input.createPalette!=0))  | (input.tabselected==13)",
        #   box(width = NULL,
        #       title = uiOutput("animatoRtime"),
        #       uiOutput("animatoRslider")
        #   ),
        #   box(width = NULL,
        #       title = uiOutput("stepSelection"),
        #       uiOutput("mystep")
        #   )
        # ),
        # br(),
        # br(),
        tags$hr(),
        # selectInput("dataset", "Download example files",
        #            choices = c('', "Ath GSE56094", "SoTub GSE58593", "Custom Network")),
        HTML('<i class="fa fa-download" aria-hidden="true"></i>',paste(tags$b(tags$span(style="color:#000000", "Download example set")), sep = "</br>")),
        helpText(a(tags$code("Ath GSE56094 experimental data"), href = "https://github.com/NIB-SI/DiNAR/raw/master/GEODataAnalysis/AthGSE56094.rar", target = "_blank")),
        helpText(a(tags$code("SoTub GSE58593 experimental data (partial)"), href = "https://github.com/NIB-SI/DiNAR/raw/master/GEODataAnalysis/SoTubGSE58593.rar", target = "_blank")),
        helpText(a(tags$code("Custom Network example"), href = "https://github.com/NIB-SI/DiNAR/blob/master/subApps/clustering/examples/Ath-PR1-fromSTRINGdb/STRINGpr1.rar?raw=true", target = "_blank")),
        # br(),
        br(),
        HTML('<i class="fa fa-code" aria-hidden="true"></i>', paste(tags$a("Click Here for the Source Code on Github!", href = "https://github.com/NIB-SI/DiNAR/tree/master/DiNARscripts", target = "_blank"))),
        
        br(),
        br(),
        tags$hr(),
        useShinyjs(),
        extendShinyjs(text = jscode, functions = c("winprint")),
        box(width = NULL, 
            title = uiOutput("RefBut"),
            # http://fontawesome.io/icons/
            actionButton(inputId = "refresh", label = tags$div(HTML(paste(tags$i(tags$span(style="color:red", "Start over")), sep = "</br>"))))
        ),
        tags$hr()
      ),
      
      ########  ########  ########  ########  ########  ########  ########
      
      
      ########  ########  ########  ########  ########  ########  ########
      # https://gist.github.com/aagarw30/c593799bc7d8557dc863411bb552e4f4
      
      mainPanel(
        navbarPage(title = NULL,
                   tabPanel("START", value=14,
                            htmlOutput("text13"),
                            br(),
                            #img(width = 500, src = "clu.jpg"),
                            tags$video(src = "dn.webm", type = "video/mp4", autoplay = "autoplay", width = 500, loop = TRUE),
                            htmlOutput("text14"),
                            tags$hr(),
                            br(),
                            # htmlOutput("textHELP"),
                            tags$a(href="http://conferences.nib.si/DiNAR/", target = "_blank", htmlOutput("textHELP")),
                            #htmlOutput("inc"),style = 'width:100%;',#includeHTML("shortInstructions.html"),#htmlOutput("inc"),
                            tags$head(tags$style(HTML("
                               body {
                                                      width: 100% !important;
                                                      max-width: 100% !important;
                                                      }
                                                      
                                                      "))),
                            # br(),
                            tags$a(href="http://conferences.nib.si/DiNAR/QuickTutorial.webm", target = "_blank", htmlOutput("textHELPwebm")),
                            br(),
                            tags$hr(),
                            tags$a(href="https://github.com/NIB-SI/DiNAR", target = "_blank", htmlOutput("AdditionalInformation")),
                            tags$a(href="https://github.com/NIB-SI/DiNAR/tree/master/GEODataAnalysis", target = "_blank", htmlOutput("GEODataAnalysis")),
                            tags$a(href="https://github.com/NIB-SI/DiNAR/tree/master/subApps", target = "_blank", htmlOutput("subApps0")),
                            tags$a(href="https://nib-si.shinyapps.io/pre-processing/", target = "_blank", htmlOutput("subApps1")),
                            tags$a(href="https://nib-si.shinyapps.io/clustering/", target = "_blank", htmlOutput("subApps2")),
                            tags$a(href="http://www.gomapman.org/", target = "_blank", htmlOutput("GoMapMan")),
                            br(),
                            tags$hr(),
                            br(),
                            helpText(a(href = "https://en.wikipedia.org/wiki/Glossary_of_graph_theory_terms", target = "_blank", htmlOutput("wikiG"))),
                            htmlOutput("text18")
                   ),
                   tabPanel("Experimental data", value=1,
                            conditionalPanel(
                            condition = "(output.selectedLKN!=-1) & (output.fileUploaded==1)",
                            # http://shiny.rstudio.com/articles/tag-glossary.html
                            tags$b('Uploaded'),
                            DT::dataTableOutput('listOfFiles'),
                            tags$b('Order for dynamic visualisation'),
                            tableOutput("listOfFilesToUse")
                            ),
                            conditionalPanel(
                              condition = "(output.selectedLKN==-1) | (output.fileUploaded==0)",
                              htmlOutput("errorTXT1")
                            )
                    ),
                            
                   tabPanel("DE palette",  value=2,
                            #p("Here are the selected colours"),
                            conditionalPanel(
                            condition = "(output.selectedLKN!=-1) & (output.fileUploaded==1) & (input.createPalette!=0)",
                            busyIndicator(wait = 3000),
                            plotOutput('piecolours', height = "1000px"), style = 'width:100%;'
                            ),
                            conditionalPanel(
                            condition = "(output.selectedLKN==-1) | (output.fileUploaded==0) | (input.createPalette==0)",
                            htmlOutput("errorTXT2")
                            )
                   ), 
                   navbarMenu("Network Information",
                              tabPanel("Clusters",  value=3,
                                       conditionalPanel(
                                        condition = "(output.selectedLKN!=-1)", 
                                       plotOutput('myhist1', height = "1000px"), style = 'width:100%;'
                                       ),
                                       conditionalPanel(
                                         condition = "(output.selectedLKN!=-1)", 
                                         busyIndicator(wait = 3000),
                                         DT::dataTableOutput('nodesClusters')
                                        ),
                                       conditionalPanel(
                                         condition = "(output.selectedLKN==-1)", 
                                         htmlOutput("errorTXT3")
                                        )
                              ), 
                              tabPanel("Nodes Degree",  value=4,
                                       conditionalPanel(
                                         condition = "(output.selectedLKN!=-1)", 
                                         busyIndicator(wait = 3000),
                                       plotOutput('myhist2', height = "2000px"), style = 'width:100%;'
                                       ),
                                       conditionalPanel(
                                         condition = "(output.selectedLKN==-1)", 
                                         htmlOutput("errorTXT4")
                                       )
                              ), 
                              tabPanel("Reactions types", value=5,
                                       conditionalPanel(
                                        condition = "(output.selectedLKN!=-1)", 
                                        busyIndicator(wait = 3000),
                                       plotOutput('myhist3', height = "1000px"), style = 'width:100%;'
                                       ),
                                       conditionalPanel(
                                         condition = "(output.selectedLKN==-1)", 
                                         htmlOutput("errorTXT5")
                                       )
                              )
                   ), # navbarMenu
                   navbarMenu("Cluster Information",
                              tabPanel("Background clusters", value=10,
                                       #width = 10,
                                       conditionalPanel(
                                         condition = "(output.selectedLKN==1) & (output.fileUploaded==1) & (input.createPalette!=0)",
                                         busyIndicator(wait = 3000),
                                         # verbatimTextOutput("hover"),
                                         plotlyOutput('BCKGN', height = "1000px"), 
                                         style = 'width:100%;',
                                         htmlOutput("text19")
                                       ),
                                       conditionalPanel(
                                         condition = "(output.selectedLKN==-1) | (output.fileUploaded==0) | (input.createPalette==0)",
                                         htmlOutput("errorTXT9")
                                       )
                                       # conditionalPanel(
                                       #   condition = "(output.selectedLKN==0) | (output.fileUploaded==0) | (input.createPalette==0)",
                                       #   htmlOutput("errorTXT")
                                       # )
                              ),
                              tabPanel("Nodes", value=9,
                                       #width = 10,
                                       conditionalPanel(
                                         condition = "(output.selectedLKN!=-1) & (output.fileUploaded==1) & (input.createPalette!=0)",
                                         busyIndicator(wait = 3000),
                                         DT::dataTableOutput('myText2'),style = 'width:100%;'
                                       ),
                                       conditionalPanel(
                                         condition = "(output.selectedLKN==-1) | (output.fileUploaded==0) | (input.createPalette==0)",
                                         htmlOutput("errorTXT8")
                                       )
                              ),
                              tabPanel("Edges", value=8,
                              #width = 10,
                                conditionalPanel(
                                  condition = "(output.selectedLKN!=-1) & (output.fileUploaded==1) & (input.createPalette!=0)",
                                  busyIndicator(wait = 3000),
                                  DT::dataTableOutput('myText1'),style = 'width:100%;'
                                ),
                                conditionalPanel(
                                  condition = "(output.selectedLKN==-1) | (output.fileUploaded==0) | (input.createPalette==0)",
                                  htmlOutput("errorTXT7")
                                )
                              ),
                              tabPanel("MapMan BINs", value=6,
                                       #tags$b('MapMan bins/cluster'),
                                       #column(width = 4, wordcloud2Output('mywordcloud1', height = "500px")),  
                                       #tags$b('Short names/cluster'),
                                       #column(width = 8, wordcloud2Output('mywordcloud2', height = "500px")),
                                       conditionalPanel(
                                         condition = "(output.selectedLKN!=-1) & (output.fileUploaded==1) & (input.createPalette!=0)",
                                         busyIndicator(wait = 3000),
                                         wordcloud2Output('mywordcloud1', height = "500px"), style = 'width:100%;'),
                                       # d3CloudOutput('mywordcloud1', height = "500px")),
                                       conditionalPanel(
                                         condition = "(output.selectedLKN!=-1) & (output.fileUploaded==1) & (input.createPalette!=0)",
                                         DT::dataTableOutput('mywordcloud2MapManTable')),
                                       #DT::dataTableOutput('mywordcloud2MapManTable')
                                       conditionalPanel(
                                         condition = "(output.selectedLKN==-1) | (output.fileUploaded==0) | (input.createPalette==0)",
                                         htmlOutput("errorTXT6")
                                       )
                              )
                   ), # navbarMenu
                   navbarMenu("Differential expression per cluster",
                              tabPanel("Static-frozen", value=15,
                                       conditionalPanel(
                                         condition = "(output.selectedLKN!=-1) & (output.fileUploaded==1) & (input.createPalette!=0) & (input.zoom!=0)",
                                         #p("The selected file expression:"),
                                         busyIndicator(wait = 3000),
                                         plotOutput('mycluPlot2',
                                                    dblclick = "plot1_dblclick",
                                                    brush = brushOpts(
                                                      id = "plot1_brush",
                                                      resetOnNew = TRUE
                                                    ),
                                                    height = "1000px"), style = 'width:100%;',
                                         htmlOutput("text15")
                                       ),
                                       conditionalPanel(
                                         condition = "(output.selectedLKN==-1) | (output.fileUploaded==0) | (input.createPalette==0)",
                                         htmlOutput("errorTXT10")
                                       )
                              ),
                              tabPanel("Static-interactive", value=11,
                                conditionalPanel(
                                condition = "(output.selectedLKN!=-1) & (output.fileUploaded==1) & (input.createPalette!=0)",
                                #p("The selected file expression:"),
                                busyIndicator(wait = 3000),
                                # plotOutput
                                visNetworkOutput('mycluPlot',
                                                height = "1000px"), style = 'width:100%;',
                                br(),
                                htmlOutput("text16"),
                                br(),
                                # https://github.com/datastorm-open/visNetwork/issues/138
                                downloadButton(outputId = 'downloadVisStatClu', label = 'Download .html')
                                ),
                                conditionalPanel(
                                  condition = "(output.selectedLKN==-1) | (output.fileUploaded==0) | (input.createPalette==0)",
                                  htmlOutput("errorTXT11")
                                )
                              ),
                              tabPanel("Dynamic-visNetwork", value=12,
                                conditionalPanel(
                                  condition = "(output.selectedLKN!=-1) & (output.fileUploaded==1) & (input.createPalette!=0) & (input.zoom!=0)",
                                  visNetworkOutput("network_proxy_nodes", height = "1000px"), style = 'width:100%;',
                                  htmlOutput("text17")
                                ),
                                conditionalPanel(
                                  condition = "(output.selectedLKN==-1) | (output.fileUploaded==0) | (input.createPalette==0)",
                                  htmlOutput("errorTXT12")
                                )
                              ),
                              tabPanel("Dynamic-animatoR", value=16,
                                       conditionalPanel(
                                         condition = "(output.selectedLKN!=-1) & (output.fileUploaded==1) & (input.createPalette!=0) & (input.zoom!=0)",
                                         #p("animatoR output:"),
                                         plotOutput('ABanimatoRAB', height = "1000px"), style = 'width:100%;',
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         fluidRow(
                                           splitLayout(cellWidths = c("30%", "70%"), 
                                                       radioButtons(inputId = "var3", label = "Select the file type", choices = list("pdf", "png"), selected = "pdf"), 
                                                       radioButtons(inputId = "varSpeed", label = "Select speed", choices = list(0.1, 0.2, 0.25, 0.5, 1), selected = 0.5)
                                                       # radioButtons(inputId = "varSpeed", label = "Select speed", choiceNames = list("0.10 (slowest rendering, more details)", "0.20 (slower rendering, more details)", "0.25 (slow rendering, more details)", "0.50 (moderate rendering, less details)", "1.00 (fastest rendering, least details)"), choiceValues = list(0.1, 0.2, 0.25, 0.5, 1), selected = 0.5)
                                           )),
                                         fluidRow(
                                           splitLayout(cellWidths = c("30%", "70%"), 
                                                       downloadButton(outputId = "down", label = "Download static image"),
                                                       actionButton(inputId = "downloadNDTV", label = "Create dynamic .html")
                                           )),
                                         fluidRow(
                                           splitLayout(cellWidths = c("30%", "70%"), 
                                                       p(""),
                                                       htmlOutput("textD3")
                                           ))
                                         #p("Download .html. Depending on the cluster size, speed and number of conditions,this could take some time. Number of vertices should be lower than 2^10"),
                                       ),
                                       conditionalPanel(
                                         condition = "(output.selectedLKN==-1) | (output.fileUploaded==0) | (input.createPalette==0)",
                                         htmlOutput("errorTXT13")
                                       )
                              )
                   ), # navbarMenu
                   tabPanel("All Controls", value=13,
                            htmlOutput("text12"),
                            br(),
                            tags$hr(),
                            br(),
                            verbatimTextOutput("log"),
                            downloadButton(outputId = 'downloadLOG', label = 'Download log')

                   ),
                   # http://stackoverflow.com/questions/24265980/reset-inputs-button-in-shiny-app#24269691
                   # https://github.com/daattali/advanced-shiny/blob/master/busy-indicator/app.R
                   bsModal(#shinyjs::useShinyjs(),
                           id = "modalndtv", 
                           title = "Render out a web-based animation",
                           trigger = "downloadNDTV", size = "large",
                           busyIndicator(wait = 3000),
                           verbatimTextOutput("ndtvPopUp"),
                           downloadButton('downloadNDTV2', 'Download dynamic .html')),
                   # tabPanel("HELP", value=17,
                   #          br(),
                   #          helpText(a(tags$code("Quick help"), href = "https://zagorgit.github.io/", target = "_blank")),
                   #          helpText(a(tags$code("Quick tutorial"), href = "https://raw.githubusercontent.com/NIB-SI/DiNARdata/master/QuickTutorial.webm", target = "_blank")),
                   #          br()
                   # ),
                   id = "tabselected"
        )#
      ) # mainPanel
    ),# sidebarLayout
    br(),
    # print date and time
    div(textOutput("currentTime"), class = "pull-right",  
        tags$style("#currentTime{color: grey; font-size: 10px;font-style: italic;}"))#,
    
  ) # fluidPage
) # shinyUI