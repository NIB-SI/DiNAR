######## ######## ########  animator statis cluster  ######## ######## ######## 
plot.net <- function(node1, edge1, clusterID, isOK){
  
  if (!isOK) {
      visNetwork(LjubljanaNodes, LjubljanaEdges, 
                 height = "100%", width = "100%", 
                 main = 'Subnetwork has to many/less edges to be nicely shown.</br>Check if nonexistent cluster or gene ID</br><span style="color: #f00;"> OR </span></br>increase/decrease minimal node degree threshold.</br>',
                 footer = 'Ljubljana graph') %>% 
        visIgraphLayout()
      

  } else {

    label <- TRUE
    coln=NULL
    cole=NULL
    main=""
    # select nodes within THE cluster
    n <- node1
    # select edges within THE same cluster
    e <- edge1
  
    # 
    if(is.null(coln)) coln <- isolate(mypalette())$palette
    ncol <- length(coln)


    minmax = max(abs(isolate(mypalette())$minmax))

    if(is.null(cole)) cole = gray.colors(n = 16, start = 0.85, end = 0.1, gamma = 0.5, alpha = NULL)
    bkg <- '#E6E6E6' # '#D9D9D9'
  
    sel <- 1:nrow(e)  ####  ####  ####  ####  ####  ####  ####  ####  ####  ####
    selA <- which(e[,ncol(e)] !=0)
    exprCol <- ncol(n)  # last column of n is expression
    edgeCol <- ncol(e)  # last column of e is 0 absence / 1 presence of the edge
  
    coln1 <- n[,exprCol]
    coln1[is.na(coln1)] <- 0
    selnNonDE <- which(coln1==0) # not DE in point i

    abs1 = abs(n[match(e$geneID1, n$geneID),exprCol][sel])
    max1 = max(abs1, 1)
    abs2 = abs(n[match(e$geneID2, n$geneID),exprCol][sel])
    max2 = max(abs2, 1)
    max3 = max(abs1/max1 + abs2/max2)
    lwdx <- (abs1/max1 + abs2/max2)/max(max3, 1)
    lwdx[is.na(lwdx)] <- 0
    lwdx[setdiff(seq(1,length(lwdx), 1), selA)] <- 0 ####  ####  ####  ####  ###
    isex <- e[,edgeCol][sel]   # edge strength/presence
  
    ncol = length(coln) 
    allInd = seq(1,length(coln1),1) # node order
    nodeCex = (abs(coln1))/max(abs(coln1), 1)
    
    minmax = isolate(colMaxMin())
    
    bigPostitiveInd = which(coln1>=minmax)
    bigNegativeInd = which(coln1<=(-minmax))
    othersInd = setdiff(allInd, union(bigPostitiveInd, bigNegativeInd))
    
    nodeColourOthers = coln[round(ceiling(ncol/2) + floor(ncol/2)*(coln1[othersInd]/minmax))]
    nodeColourBigPostitive = rep(coln[ncol], length(bigPostitiveInd))
    nodeColourBigNegative = rep(coln[1], length(bigNegativeInd))
    colx = rep('white', length(nodeCex))
    colx[bigPostitiveInd] = nodeColourBigPostitive
    colx[bigNegativeInd] = nodeColourBigNegative
    colx[othersInd] = nodeColourOthers
    # not xpressed between two points/conditions stay gray
    colx[selnNonDE] = '#F3F3F3' # '#E6E6E6' # '#D9D9D9'
    
    temp = lwdx*16+1
    temp[which(temp > length(cole))] = length(cole)
    colex = cole[temp]

    minDegreeN = strtoi(trimws(input$mydegree))
    main <- paste("Cluster:",paste(clusterID, collapse=", "),
                  paste0(" [minDegree: ", minDegreeN, "]"),
                  "\nn:",nrow(n),"e:",nrow(e))
    

    nc <- length(coln)

    
    ##################################################################
    # # Active
    # # short node name
    mytext = ifelse(n$shortName !='-', n$shortName, n$geneID)
    tmptext = strsplit(mytext, '')
    temp = list(as.vector(rep(' ', max(unlist(lapply(tmptext, function(x) (length(x)))))+1)))
    
    temp = list(as.vector(rep(' ',20)))
    temp = lapply(1:length(tmptext), function(x) temp[[x]] = temp[[1]])
    temp = lapply(1:length(temp), function(x) {
        temp[[x]][1:length(tmptext[[x]])] = tmptext[[x]]
        temp[[x]] = paste(temp[[x]], collapse = '')
        })
    mytext = temp
    # mytext = ''
    mytext = paste0(mytext,'\n              [',as.numeric(formatC(coln1, format = "f", digits = 2)), ']')
    mytext[which(coln1 == 0)] = ''
    # nodes
    cexx <- 4*nodeCex + 1.5

    # edges
    # lwdx <- 4*lwdx + 0.5
    lwdx <- 5*lwdx + 0.5 ####  ####  ####  ####  ####  ####  ####  ####  ####  ###

    # print('VisStat')
    # print(selA)

    # active

    tmp = unique(e$reactionType)
    act = grep('act', tmp)
    inh = grep('inh', tmp)
    bind = grep('bind', tmp)
    unkn = grep('unk', tmp)
    rest = setdiff(seq(1, length(tmp), 1), union(act, union(inh, union(bind, unkn))))
    
    myarrows = unlist(sapply(e$reactionType, 
                            function(x){
                              if ((x %in% tmp[act])){
                                return('to;middle')
                              }
                            if (x %in% tmp[inh]){
                              return('')
                            }
                              if (x %in% tmp[bind]){
                                  return('to;from;middle')
                              }
                              if (x %in% tmp[unkn]){
                                return('')
                              }
                              if (x %in% tmp[rest]){
                                return('')
                              }
                            }
                            )
                          )
    names(myarrows) = seq(1, dim(e)[1], 1)

    mydashes = unlist(sapply(e$reactionType, 
                             function(x){
                               if ((x %in% tmp[act])){
                                 return('FALSE')
                               }
                               if (x %in% tmp[inh]){
                                 return('FALSE')
                               }
                               if (x %in% tmp[bind]){
                                 return('TRUE')
                               }
                               if (x %in% tmp[unkn]){
                                 return('TRUE')
                               }
                               if (x %in% tmp[rest]){
                                 return('TRUE')
                               }
                             }
    )
    )
    
    names(mydashes) = seq(1, dim(e)[1], 1)
    

    nodes <- data.frame(id = n$geneID,
                        # add labels on nodes
                        label = mytext,
                        # size adding value
                        value = cexx,
                        # size = cexx,
                        # http://stackoverflow.com/questions/39674927/how-have-labels-inside-the-scaled-nodes-in-visnetwork
                        font.size = 2*cexx, 
                        borderWidth = cexx,
                        # borderWidthSelected = cexx,
                        # scaling.label = FALSE,
                        # control shape of nodes
                        shape = rep('circle', length(colx)),
                        # tooltip (html or character), when the mouse is above
                        title = paste0("<p><b>ID: ", n$geneID, "<br>shortName: ", n$shortName, '<br>description: ', n$shortDescription, '<br>BIN: ', n$MapManBin, "</b></p>"),
                        # color
                        color = colx,
                        # x,y
                        x = n$x,
                        y = -n$y,
                        physics = FALSE)

    edges <- data.frame(from = e$geneID1, to = e$geneID2,
                        label = sapply(1:length(e$exists), function(x) ifelse(e$exists[x]!=0, e$reactionType[x], '')),
                        width = lwdx,
                        color = colex,
                        title = paste0("<p><b>(", e$geneID1, " ", e$reactionType, " ", e$geneID2, ")</b></p>"),
                        physics = FALSE,
                        arrows = myarrows,
                        dashes = as.logical(mydashes))
    


    sidePalette <- data.frame(color = (unique(isolate(mypalette())$palette))[seq(1,nc,2.5)],
                              label =  formatC(round(seq(-minmax, minmax, 
                                                         length.out=nc)[seq(1,nc,2.5)],2), 2, format = "f"),
                              shape = 'circle',
                              circle = list(size = c(2)))
    
    
    tmpdf <- data.frame(color = rep("gray32", 5), 
                        label = c("activation", "inhibition", "binding", "unknown", "others"),
                        arrows = c("to;middle", "", "to;from;middle", "", ""),
                        dashes = c(FALSE, FALSE, TRUE, TRUE, TRUE),
                        size = c(1,2,3,4,5))
    
    
    vn = visNetwork(nodes, edges, height = "100%", width = "100%", main = main)
    vn$x$edges$label = NULL
    vn %>%
      visEdges(smooth = list(enabled = TRUE, type = "diagonalCross"),
               dashes = TRUE) %>%
      visNodes(scaling = list(min = 20, max = 60)) %>% 
      visPhysics(stabilization = FALSE) %>% 
      visOptions(highlightNearest = list(enabled = TRUE, hover = FALSE, degree = 1, 
                                         algorithm = "all", hideColor = 'rgba(255,255,255,0)'),
                 nodesIdSelection = list(enabled = TRUE, 
                                         useLabels = TRUE, style = 'background: #003300;width: 200px;
                                         color: #FFFFFF;
                                         border:none;
                                         outline:none;')
                 ) %>%
      visInteraction(navigationButtons = TRUE,
                     keyboard = TRUE, 
                     tooltipDelay = 0) %>%
      visLegend(main = list(text = "Legend",
                            style = "font-family:Comic Sans MS;color:#003300;font-size:12px;text-align:center;"),
                useGroups = FALSE,
                enabled = TRUE,
                addEdges = tmpdf, 
                addNodes = sidePalette, 
                position = 'right', 
                width = 0.2, 
                ncol = 1,
                zoom = TRUE) %>%
      visIgraphLayout()
    

  }
}



whatToPlot <- function(node1, edge1, cluID, mytimepoint,isOK){

  # global user variables, e.g.:
  ### ID column, p-value column, logFC column, p-val cut-off and logFC cut-off
  myIDcol1 = isolate(myIDcol())
  selectedPval1 = isolate(selectedPval())
  selectedLogFC1 = isolate(selectedLogFC())
  cutP1 = isolate(cutP())
  cutFC1 = isolate(cutFC())

  # for which tp/cond
  tp = myFiles[[mytimepoint]]

  # match gene identifiers
  ind = match(node1[,1], tp[,myIDcol1])
  ind2 = which(!is.na(ind))



  # assign gene expression value
  ### if p-val < threshold -> logFC, else expression = 0
  node1[,ncol(node1)]= rep(0.00, length(node1[,ncol(node1)]))

  # cluster node ID that exist in experimental data: node1$geneID[ind2]
  # node ID in experimental data that match cluster node ID : tp[ind[ind2],myIDcol1]
  # any(node1$geneID[ind2] != tp[ind[ind2],myIDcol1])

  # first filter by e.g. adjusted p-value
  node1[,ncol(node1)][ind2] = ifelse(tp[ind[ind2],selectedPval1] < cutP1, tp[ind[ind2],selectedLogFC1], 0.00)

  # then additionaly filter by e.g. logFC absolute threshold
  node1[,ncol(node1)][ind2] = ifelse(abs(node1[,ncol(node1)][ind2]) >= cutFC1, node1[,ncol(node1)][ind2], 0.00)


  # match cluster node IDs with cluster edgeIDs
  ind3 = match(edge1$geneID1, node1$geneID)
  ind4 = match(edge1$geneID2, node1$geneID)

  edge1[,ncol(edge1)] = rep(0.00, length(edge1[,ncol(edge1)]))
  # assign edge expression
  ### if both end nodes are DE -> edge expression = 1, edge expression = 0
  edge1[,ncol(edge1)] = ifelse(abs(node1[,ncol(node1)][ind3]*node1[,ncol(node1)][ind4]) != 0.00, 1, 0.00)

  plot.net(node1, edge1, clusterID = cluID, isOK = isOK)
}
