######## ######## ########  animator statis cluster  ######## ######## ######## 
plot.net2 <- function(node1, edge1, clusterID, isOK){
  
  if (!isOK) {
      plot(Ljubljana, layout = LjubljanaLayout,
           vertex.color = '#009900',
           edge.label='',
           vertex.label = '',
           vertex.size = rep(4, length(V(Ljubljana))),
           # rescale=FALSE, add=TRUE,
           vertex.label.cex= 0.5,
           edge.arrow.size = 0.25,
           edge.arrow.width = 0.25,
           edge.lty = 'solid',
           edge.color = '#009900',
           edge.width = 0.25,
           # edge.arrow.mode = 0,
           edge.label.cex = 0.25,
           xlab='Ljubljana graph',
           main = paste0("\n",
                         "Subnetwork has to many/less edges to be nicely shown.",
                         "\n",
                         "Check if nonexistent cluster or gene ID",
                         "\n",
                         "OR",
                         "\n",
                         "increase/decrease minimal node degree threshold.", 
                         "\n")
      )
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
    bkg <- '#E6E6E6' #'#D9D9D9'
    
    sel <- which(e[,ncol(e)] !=0) # 1:nrow(e) for all
    exprCol <- ncol(n)  # last column of n is expression
    edgeCol <- ncol(e)  # last column of e is 0 absence / 1 presence of the edge
    
    coln1 <- n[,exprCol]
    coln1[is.na(coln1)] <- 0
    selnNonDE <- which(coln1==0) # not DE in point i
    
    abs1 = abs(n[match(e$geneID1, n$geneID),exprCol][sel])
    max1 = max(abs1, 1)
    abs2 = abs(n[match(e$geneID2, n$geneID),exprCol][sel])
    max2 = max(abs2, 1)
    max3 = max(abs1/max1+abs2/max2)
    lwdx <- (abs1/max1 + abs2/max2)/max(max3, 1)
    lwdx[is.na(lwdx)] <- 0
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
    colx[selnNonDE] = '#E6E6E6' # '#D9D9D9'
    
    selFIX = which(lwdx != 0)
    temp = lwdx*16+1
    temp[which(temp > length(cole))] = length(cole)
    colex = cole[temp]
    
    minDegreeN = strtoi(trimws(input$mydegree))

    # bottom, left, top, right  
    par(oma=c(2,1,1,2))  # outer margins
    par(mar=c(3,1,2,5))  # margin, lines

    main <- paste("Cluster:",paste(clusterID, collapse=", "),
                  paste0(" [minDegree: ", minDegreeN, "]"),
                  "\nn:",nrow(n),"e:",nrow(e))
    
    yLimits = range(n$y) # isolate(animatoRNE())$yLimits # range(n$y)  ####  ####  ####  ####  ####  ####  ####  ####  ####
    xLimits = range(n$x) # isolate(animatoRNE())$xLimits # range(n$x)  ####  ####  ####  ####  ####  #### 
    
    
    newplot(xlim=c(range(xLimits)[1], range(xLimits)[2]+10),
            ylim=yLimits + c(-0, 0),
            pty="m") #, asp=12/16)
    
    title(main,xpd=TRUE)
    
    nc <- length(coln)
    # palette
    points(rep(max(xLimits) + diff(range(xLimits))*0.05 + 2, nc),
           max(yLimits)-(nc:1)/nc*diff(range(yLimits)), 
           cex=2,pch=22,bg=coln,xpd=TRUE,col=NA)
    
    text(# rep(max(xLimits) + diff(range(xLimits))*0.05 + 3 + 1.0*(diff(range(xLimits))*0.05)/2, nc)[seq(1,71,10)], 
      rep(max(xLimits) + diff(range(xLimits))*0.05 + 2 + strwidth("//", cex=2.0)*2.5, nc)[seq(1,71,10)], 
      (max(yLimits)-(nc:1)/nc*diff(range(yLimits)))[seq(1,nc,10)], 
      col = coln[seq(1,nc,10)], 
      cex = 0.75, 
      xpd=TRUE,
      labels = formatC(round(seq(-minmax, minmax, length.out=71)[seq(1,nc,10)],2), 2, format = "f"))

    pp=par("usr")
    
    # # Background
    # pie(x=c(1,1,1), col=c('#D9D9D9', '#E6E6E6', '#F3F3F3'), labels = c('#D9D9D9', '#E6E6E6', '#F3F3F3'))
    # edges
    segments(
      #n[e$geneID1,"x"],
      n[match(e$geneID1, n$geneID),"x"],
      #n[e$geneID1,"y"],
      n[match(e$geneID1, n$geneID),"y"],
      #n[e$geneID2,"x"],
      n[match(e$geneID2, n$geneID),"x"],
      #n[e$geneID2,"y"],
      n[match(e$geneID2, n$geneID),"y"],
      col='#F3F3F3', # bkg,
      lwd=0.5)
    # 
    # # Background
    # vertices
    points(n$x,n$y,
           col="#E6E6E6", # bkg,
           pch=16, cex=1.5)# cex=0.75) # Background
    
    
    ##################################################################
    # # Active
    # # short node name
    mytext = ifelse(n$shortName !='-', n$shortName, n$geneID)
    mytext = paste0(mytext,'\n[',round(coln1,2), ']')
    mytext[which(coln1 == 0)] = ''
    # nodes
    cexx <- 4*nodeCex + 1.5
    
    # edges
    lwdx <- 2*lwdx + 0.5
    
    segments(
      n[match(e$geneID1[sel], n$geneID),"x"],
      n[match(e$geneID1[sel], n$geneID),"y"],
      n[match(e$geneID2[sel], n$geneID),"x"],
      n[match(e$geneID2[sel], n$geneID),"y"],
      col=colex,
      lwd=lwdx)
    
    # print("staticAB")
    # print(sel)
    


    # active
    act = setdiff(seq(1, dim(n)[1], 1), selnNonDE)
    points(n[act,]$x,n[act,]$y, pch=21,
           bg = colx[act],
           col = colx[act],#"white", # white circle around node, actice nodes should cover inactive ones
           cex = cexx[act])
    
    # THIS TRIGGERS FLICKER
    # txtcx = abs(h(cex1,cex2,t))/(max(abs(h(cex1,cex2,t)), 1)*2)
    # tst(txtcx)
    # textCol = rgb(0,0,0, alpha = 0.5 +   tst(txtcx))
    # textCol = rgb(0,0,0, alpha = 0.8)
    
    text(x = n$x, 
         y = n$y,
         labels = mytext,
         cex = 3*cexx/(3*max(cexx,1)),
         offset = 0.0,
         col = 'black')
  }
}



whatToPlot2 <- function(node1, edge1, cluID, mytimepoint,isOK){


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
  node1[,ncol(node1)] = rep(0.00, length(node1[,ncol(node1)]))

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

  plot.net2(node1, edge1, clusterID = cluID, isOK = isOK)
}
