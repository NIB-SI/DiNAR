myd3movie <- function(n, e, cutDeg, mymodplus, 
                      mypalette, myFiles, 
                      myIDcol1, selectedPval1, selectedLogFC1, cutP1, cutFC1,
                      eThreshold, nThreshold,
                      minmax){
  
  # con <- file("ndtv.log")
  # sink(con, append=TRUE)
  # sink(con, append=TRUE, type="message")
  

  loopInd = which(e[,1]==e[,2])
  if (length(loopInd) > 0) {
    e = e[-loopInd,]
  }
  
  dupl <- duplicated(e[,1:2])
  dupl <- as.numeric(dupl)
  duplInd = which(dupl ==1)
  duplInfo = e[duplInd,1:3]
  
  e$reactionType = paste0(e$geneID1, '->', e$reactionType, '->', e$geneID2)
  
  if (length(duplInd) > 0) {
    tmpset1 = paste0(e[,1],'_',e[,2])
    tmpset2 = paste0(e[duplInd,1],'_',e[duplInd,2])
    fixme = lapply(tmpset2, function(x) which(tmpset1 %in% x)) 
    
    tmpr = lapply(fixme, function(x) {
      paste(sapply(x, function(y) e[y,]$reactionType), collapse = '/')
    })
    
    # 2021-02-24 ## e[unlist(fixme),]$reactionType = unlist(tmpr)
    map = setNames(fixme, tmpr)
    e[unlist(fixme),]$reactionType = names(unlist(map))
  }
  
  dupl <- duplicated(e[,1:2])
  dupl <- as.numeric(dupl)
  duplInd = which(dupl ==1)
  if (length(duplInd) > 0) {
    e = e[-duplInd,]
  }
  
  unconnInd = which(n[,1] %ni% c(e[,1], e[,2]))
  if (length(unconnInd) > 0) {
    n = n[-unconnInd, ]
  }
  
  if ((dim(e)[1] < eThreshold) & (dim(n)[1] < nThreshold)){
    
    # DATA PREPARATION - as for animatoR
    ######################################################################## 
    ######################################################################## 
    if(!is.null(dim(myFiles))) {
      n.of.tp = 1
    } else {
      n.of.tp = length(myFiles)
    }
    

    node1 = n
    edge1 = e
    
    mySubnetworkNodes = list()
    ########################################################################
    for (i in 1:n.of.tp){
      # which timepoint
      if (n.of.tp == 1){
        tp = myFiles[[1]]
      } else {
        tp = myFiles[[i]]
      }
      # match gene identifiers
      ind = match(node1[,1], tp[,myIDcol1])
      ind2 = which(!is.na(ind))
      # assign gene expression value
      ### if p-val < treashold -> logFC, else expression = 0
      node1[, ncol(node1)]= rep(0.00, length(node1[, ncol(node1)]))
      # first filter by e.g. adjusted p-value
      node1[, ncol(node1)][ind2] = ifelse(tp[ind[ind2],selectedPval1] < cutP1, tp[ind[ind2],selectedLogFC1], 0.00)
      # then additionaly filter by e.g. logFC absolute threshold
      node1[, ncol(node1)][ind2] = ifelse(abs(node1[, ncol(node1)][ind2]) >= cutFC1, node1[, ncol(node1)][ind2], 0.00)
      # match cluster node IDs with cluster edgeIDs
      ind3 = match(edge1$geneID1, node1$geneID)
      ind4 = match(edge1$geneID2, node1$geneID)
      edge1[, ncol(edge1)] = rep(0.00, length(edge1[, ncol(edge1)]))
      # assign edge expression
      ### if both end nodes are DE -> edge expression = 1, edge expression = 0
      edge1[, ncol(edge1)] = ifelse(abs(node1[, ncol(node1)][ind3]*node1[, ncol(node1)][ind4]) != 0.00, 1, 0.00)
      mySubnetworkNodes[[i]] = node1 # nodes
    }
    nodes0 =  mySubnetworkNodes[1]
    nodes0[[1]][,dim(nodes0[[1]])[2]] = rep(0, dim(nodes0[[1]])[1])
    mySubnetworkNodes = c(nodes0, mySubnetworkNodes)
    
    mySubnetworkEdges = list()
    ############################################################################
    for (i in 1:n.of.tp){
      # which timepoint
      if (n.of.tp == 1){
        tp = myFiles[[1]]
      } else {
        tp = myFiles[[i]]
      }
      # match gene identifiers
      ind = match(node1[,1], tp[,myIDcol1])
      ind2 = which(!is.na(ind))
      # assign gene expression value
      ### if p-val < treashold -> logFC, else expression = 0
      node1[, ncol(node1)] = rep(0.00, length(node1[, ncol(node1)]))
      # cluster node ID that exist in experimental data: node1$geneID[ind2]
      # first filter by e.g. adjusted p-value
      node1[, ncol(node1)][ind2] = ifelse(tp[ind[ind2],selectedPval1] < cutP1, tp[ind[ind2],selectedLogFC1], 0.00)
      # then additionaly filter by e.g. logFC absolute threshold
      node1[, ncol(node1)][ind2] = ifelse(abs(node1[, ncol(node1)][ind2]) >= cutFC1, node1[, ncol(node1)][ind2], 0.00)
      # match cluster node IDs with cluster edgeIDs
      ind3 = match(edge1$geneID1, node1$geneID)
      ind4 = match(edge1$geneID2, node1$geneID)
      edge1[, ncol(edge1)]= rep(0.00, length(edge1[, ncol(edge1)]))
      # assign edge expression
      ### if both end nodes are DE -> edge expression = 1, edge expression = 0
      edge1[, ncol(edge1)] = ifelse(abs(node1[, ncol(node1)][ind3]*node1[, ncol(node1)][ind4]) != 0.00, 1, 0.00)
      mySubnetworkEdges[[i]] = edge1 # nodes
    }
    
    edges0 =  mySubnetworkEdges[1]
    edges0[[1]][,dim(edges0[[1]])[2]] = rep(0, dim(edges0[[1]])[1])
    mySubnetworkEdges = c(edges0, mySubnetworkEdges)
    
    n = mySubnetworkNodes
    e = mySubnetworkEdges
    palette = mypalette
    clusterID = unique(n[[1]]$clusterID)
    
    
    # HOMOTOPY - animatoR
    ########################################################################
    ########################################################################
    nlist = n
    elist = e
    palette =  palette
    clusterID = clusterID
    
    if ((length(nlist) < 2) | (length(elist) < 2)) return(NULL)
    if(!is.null(dim(nlist))) nlist <- list(nlist)
    if(!is.null(dim(elist))) elist <- list(elist)
    label = TRUE  # label nodes
    .test <- !TRUE
    ######## ######## ######## test function ### ######## ######## ######## ######
    tst <- function(x) if(.test) cat(deparse(substitute(x)),":",x,"\n")
    ######## ######## ######## ######## ######## ######## ######## ######## ######
    
    # background + number of conditions/time points
    tst(length(nlist))
    # total animation time
    ntp <- length(nlist)
    
    
    if (mymodplus == 0.10) mymod = 10
    if (mymodplus == 0.20) mymod = 5
    if (mymodplus == 0.25) mymod = 4
    if (mymodplus == 0.50) mymod = 2
    if (mymodplus == 1.00) mymod = 1
    
    myFrames = seq(0, length(myFiles), mymodplus)*mymod
    myframesNum = length(myFrames)
    mytext0 = matrix(data = 0, nrow = dim(nlist[[1]])[1], ncol = myframesNum)
    colx0 = matrix(data = 0, nrow = dim(nlist[[1]])[1], ncol = myframesNum)
    cexx0 = matrix(data = 0, nrow = dim(nlist[[1]])[1], ncol = myframesNum)
    colex0 = matrix(data = 0, nrow = dim(elist[[1]])[1], ncol = myframesNum)
    lwdx0 = matrix(data = 0, nrow = dim(elist[[1]])[1], ncol = myframesNum)
    # nodeEX0 = matrix(data = 0, nrow = dim(nlist[[1]])[1], ncol = myframesNum)
    # edgeEX0 = matrix(data = 0, nrow = dim(elist[[1]])[1], ncol = myframesNum)
    
    for (t0 in (myFrames/mymod)){
      dtime <- t0
      # integer part of tanim2
      day <- dtime%/%1
      # decimal part of tanim2
      t <- dtime%%1
      # number of vertices
      m <- dim(nlist[[1]])[1]
      tst(dtime)
      tst(t)
      tst(day)
      tst(clusterID)
      tst(palette$palette)
      tst(ntp)
      ######## ######## ######## ######## ######## ######## ######## #######
      # number of nodes and vertices per selected cluster
      n <- nlist[[1]]
      #if(.test) print(head(n))
      e <- elist[[1]]
      #if(.test) print(head(e))
      main <- paste("Cluster:",paste(clusterID, collapse=", "),
                    "\nn:",nrow(n),"e:",nrow(e),
                    "\nminDegree:",cutDeg, ', ', "speed:",mymodplus)
      #
      # colors palette for expression and background
      #
      # nodes
      if(length(palette)!=2){
        coln <- colorRampPalette(c("yellow3","white", "darkblue"))( 70 )
        coln <- colorRampPalette(c("darkblue","grey90", "darkred"))( 71 )
      } else coln <- palette$palette
      nc <- length(coln)
      # edges
      cole = gray.colors(n = 16, start = 0.85, end = 0.1, gamma = 0.5, alpha = NULL)
      bkg <- '#E3E3E3' # '#D9D9D9' # gray(0.8)
      #
      ####  ####  ####  ####  ####  ####  ####  ####  ####  ####  ####  ####  ##
      # sel is used for partial plotting (future ...) can be used for selection of active edges
      #
      sel <- 1:nrow(elist[[1]])
      exprCol <- ncol(nlist[[1]])  # last column of n is expression
      edgeCol <- ncol(elist[[1]])  # last column of e is 0 absence / 1 presence of the edge
      #
      # background (expression 0) + expression in day x
      i <- day + 1 
      tst(i)
      coln1 <- nlist[[i]][,exprCol]
      selnNonDE1 <- which(coln1==0) # not DE in point i
      tst(coln1)
      coln1[is.na(coln1)] <- 0
      cex1 <- nlist[[i]][,exprCol] # any(coln1 != cex1) FALSE
      cex1[is.na(cex1)] <- 0  # if no NA cex1 == coln1

      minmax = max(abs(isolate(colMaxMin())))

      # strength of the edge abs(DEnode1) + abs(DEnode2); sel::allEdges
      abs1 = abs(nlist[[i]][match(elist[[i]]$geneID1, nlist[[i]]$geneID),exprCol][sel])
      max1 = max(abs1, 1)
      abs2 = abs(nlist[[i]][match(elist[[i]]$geneID2, nlist[[i]]$geneID),exprCol][sel])
      max2 = max(abs2, 1)
      max3 = max(abs1/max1 + abs2/max2)
      lwd1 <- (abs1/max1 + abs2/max2)/max(max3, 1)
      lwd1[is.na(lwd1)] <- 0
      lwd1 = ifelse(elist[[i]][,edgeCol] !=0, lwd1, 0) ####  ####  ####  ####  #
      ise1 <- elist[[i]][,edgeCol][sel]   # edge strength/presence
      tst(max(lwd1))
      
      
      # background (expression 0) + expression in day x + 1
      i <- min(day+2,ntp)
      
      coln2 <- nlist[[i]][,exprCol]
      selnNonDE2 <- which(coln2==0)
      coln2[is.na(coln1)] <- 0
      cex2 <- nlist[[i]][,exprCol]
      cex2[is.na(cex2)] <- 0
      
      abs1 = abs(nlist[[i]][match(elist[[i]]$geneID1, nlist[[i]]$geneID),exprCol][sel])
      max1 = max(abs1, 1)
      abs2 = abs(nlist[[i]][match(elist[[i]]$geneID2, nlist[[i]]$geneID),exprCol][sel])
      max2 = max(abs2, 1)
      max3 = max(abs1/max1 + abs2/max2)
      lwd2 <- (abs1/max1 + abs2/max2)/max(max3, 1)
      lwd2[is.na(lwd1)] <- 0
      lwd2 = ifelse(elist[[i]][,edgeCol] !=0, lwd2, 0) ####  ####  ####  ####  ##
      ise2 <- elist[[i]][,edgeCol][sel]   # edge strngth/presence
      tst(max(lwd2))   
      
      # par(oma=c(0,0,0,0),mar=c(2,1,3,2))
      selnNonDE <- intersect(selnNonDE1, selnNonDE2)
      
      
      ######################################################################
      # Nodes expression: cex1 and cex2 (DE)
      # Edges expression: lwd1 and lwd2 [0,1]
      ###################################################################### 
      # this is absolute threshold for boundary colour assignment - user defined
      minmax = minmax 
      ncol = length(coln) 
      allInd = seq(1,length(cex2),1) # node order
      #### #### #### size in discrete time point/condition ### #### #### ###
      # day x
      nodeCex1 = (abs(cex1))/max(abs(cex1), 1)/5
      # day x + 1
      nodeCex2 = (abs(cex2))/max(abs(cex2), 1)/5
      ######## ######## ######## in tanim2 ######## ########  ########  ####
      # vertice cex in time t
      nodeCex = h(nodeCex1, nodeCex2, t)
      # DE edges in time t
      isex = h(ise1,ise2,t)
      tst(max(isex))
      #### #### #### colours in time point/condition t0 ### #### #### #### #
      ccex = h(cex1, cex2, t)
      # those nodes should be coloured palette$palette[71] in tp
      bigPostitiveInd = which(ccex>=minmax)
      # those nodes should be coloured palette$palette[1] in tp
      bigNegativeInd = which(ccex<=(-minmax))
      # palette$palette[2:70]
      othersInd = setdiff(allInd, union(bigPostitiveInd, bigNegativeInd))
      nodeColourOthers = coln[round(ceiling(ncol/2) + floor(ncol/2)*(ccex[othersInd]/minmax))]
      nodeColourBigPostitive = rep(coln[ncol], length(bigPostitiveInd))
      nodeColourBigNegative = rep(coln[1], length(bigNegativeInd))
      colx = rep('white', length(ccex))
      colx[bigPostitiveInd] = nodeColourBigPostitive
      colx[bigNegativeInd] = nodeColourBigNegative
      colx[othersInd] = nodeColourOthers
      # not xpressed between two points/conditions stay gray
      colx[selnNonDE] = '#E3E3E3' # '#D9D9D9'
      # edge width in time t
      lwdx = h(lwd1, lwd2, t)
      tst(max(lwdx))
      # edge colour in time t
      tst(max(lwdx*16+1))
      temp = lwdx*16+1
      temp[which(temp > length(cole))] = length(cole)
      colex = cole[temp]
      ######## ######## ######## ######## ######## ######## ######## #######
      ######## ######## ######## ######## ######## ######## ######## #######
      ######## ######## ######## ######## ######## ######## ######## #######
      # shiny slider keeps time running
      i <- day + 1
      nt <- ntp
      tst(nt)
      ##################################################################
      # # Active
      # # short node name
      mytext = ifelse(n$shortName !='-', n$shortName, n$geneID)
      ts = round(h(coln1,coln2,t),2)
      # mytext = paste0(mytext,'\n[', ts, ']')
      mytext[which(round(abs(ts)) == 0)] = ''
      # nodes
      
      a=seq(1,nThreshold,1)
      b=log(a)
      c=max(b)
      d=(c-b)/1+1.0
      cexx <- d[length(nodeCex)]*nodeCex + 0.25
      tst(max(cexx))
      tst(min(cexx))
      # edges
      lwdx <- 2*lwdx
      tst(max(lwdx))
      tst(min(lwdx))
      whereAmI = t0*mymod+1
      # cat('time', t0, ' ', whereAmI, '\n')
      mytext0[,whereAmI] = mytext
      colx0[,whereAmI] = colx
      cexx0[,whereAmI] = cexx
      colex0[,whereAmI] = colex
      lwdx0[,whereAmI] = lwdx
    }
    
    # NDTV
    ########################################################################
    ########################################################################
    # 
    mymatch1 = match(elist[[1]][,1], nlist[[1]][,1])
    mymatch2 = match(elist[[1]][,2], nlist[[1]][,1])
    mye0 =   cbind(mymatch1, mymatch2)
    mymytext = ifelse(nlist[[1]]$shortName !='-', nlist[[1]]$shortName, '-')
    mymyname = nlist[[1]]$geneID
    
    mysubg <- graph_from_edgelist(as.matrix(mye0[,1:2]), directed = TRUE)
    myx = nlist[[1]]$x
    myy = nlist[[1]]$y
    
    net<-intergraph::asNetwork(mysubg)
    # plot(net)
    
    vs <- data.frame(onset=0, terminus=myframesNum, 
                     vertex.id=1:dim(nlist[[1]])[1])
    es <- data.frame(onset=0, terminus=myframesNum, 
                     head=as.matrix(net, matrix.type="edgelist")[,1],
                     tail=as.matrix(net, matrix.type="edgelist")[,2])
    # mytext
    # 3*cexx/(3*max(cexx,1))
    # colx
    # cexx
    # colex
    # lwdx
    for (i in 0:(myframesNum-1)){
      activate.vertex.attribute(net,
                                "color", 
                                colx0[,i+1], 
                                onset=i,
                                terminus=i+1)
      activate.vertex.attribute(net,
                                "cex", 
                                cexx0[,i+1]/1, 
                                onset=i,
                                terminus=i+1)
      activate.vertex.attribute(net,
                                "name", 
                                mytext0[,i+1], 
                                onset=i,
                                terminus=i+1)
      activate.vertex.attribute(net,
                                "textcex", 
                                2*cexx0[,i+1]/(3*max(cexx0[,i+1],1)), 
                                onset=i,
                                terminus=i+1)
      activate.edge.attribute(net,
                              "color",
                              colex0[,i+1], 
                              onset=i,
                              terminus=i+1)
      activate.edge.attribute(net,
                              "lwd",
                              lwdx0[,i+1] + 0.25, 
                              onset=i,
                              terminus=i+1)
    activate.vertex.attribute(net,
                              "x",
                              myx,
                              onset=i,
                              terminus=i+1)
    activate.vertex.attribute(net,
                              "y",
                              myy,
                              onset=i,
                              terminus=i+1)
    }
    # activate.vertex.attribute(net,
    #                           "type",
    #                           elist[[1]]$reactionType,
    #                           onset=0,
    #                           terminus=myframesNum)
    
    net.dyn <- networkDynamic(base.net=net, edge.spells=es, vertex.spells=vs)
    # is.multiplex(net)
    # is.bipartite(net)
    # is.directed(net)
    # is.hyper(net)
    # has.loops(net)
    
    # Pre-compute animation coordinates
    mycoord = as.matrix(cbind(myx,myy))*10
    colnames(mycoord) = c("x", "y")
    
    compute.animation(net.dyn, 
                      animation.mode = "useAttribute",
                      # layout.par = list(x = net %v% "x", y = net %v% "y"),
                      slice.par=list(start=0, end=(myframesNum-1), interval=1,
                                     aggregate.dur=1, rule='any'),
                      seed.coords = 4*mycoord)
    
    # Make movie
    # xlab: label caption below the render, on the xaxis
    # main: main headline above the render
    # displaylabels: should vertex labels be displayed?
    # usearrows: should arrows be drawn on edges?
    # bg: background color (must be html compatible? need to check this)
    # vertex.cex: vertex expansion scale factor
    # label: labels for vertices (defaults to vertex.names)
    # label.col: color of vertex labels
    # label.cex: vertex label expansion scale factor
    # vertex.col: vertex fill colors
    # vertex.sides: number of sides for vertex polygon (shape)
    # vertex.rot: rotation for vertex polygon
    # vertex.border: color of vertex border stroke
    # vertex.lwd: width of vertex border stroke
    # edge.lwd: width of edge stroke
    # edge.col: edge stroke color
    # https://www.rdocumentation.org/packages/ndtv/versions/0.10.0/topics/render.d3movie
    # https://rdrr.io/cran/ndtv/src/R/export.movie.R
    tic=proc.time()[3]
    render.d3movie(net.dyn,
                   usearrows = FALSE,
                   displaylabels = FALSE,
                   label=mymytext,
                   vertex.border="#ffffff",
                   vertex.lwd = 'cex',
                   vertex.col = "color",
                   vertex.cex = 'cex',
                   edge.lwd = 'lwd',
                   edge.col = 'color',
                   label.cex = 'textcex',
                   plot.par = list(bg='#ffffff', main = main),
                   vertex.tooltip = paste("<b>Short Name:</b>", mymytext , "<br>",
                                          "<b>Node ID:</b>", mymyname , "<br>"),
                   edge.tooltip = paste("<b>Edge type:</b>", elist[[1]]$reactionType, "<br>"),
                   launchBrowser=FALSE,
                   output.mode = 'HTML', #htmlWidget',  # output directly as htmlwidget instead of to file#HTML
                   filename = 'mytempHTML.html',
                   render.par=list(tween.frames = 10, show.time = FALSE, initial.coords=mycoord),
                   script.type='embedded', 
                   verbose = TRUE) #FALSE,
    
    toc=proc.time()[3] - tic
    (toc/60)
    # if script.type='embedded', the scripts will be embedded directly in the output html page. 
    # This option is the most portable, but will require large file sizes. 
    # If script.type='remoteSrc', only the links to the scripts will be included, 
    # so the page will require an active internet connection to play the animation
    
    # Like most network algorithms, the time to compute layouts for animations tends
    # to scale quite badly with network size. We generally have only had enough
    # patience to generate movies for networks of less than 1000 vertices. There also
    # seems to be quite a bit of overhead in the animation package, so the generation
    # process seems to slow down considerably for longer duration networks or when
    # slice or render parameters cause lots of slices to be generated.
    
  } else {
    mysubg <- graph_from_edgelist(as.matrix(LjubljanaEdges[,1:2]), directed = FALSE)
    myx = LjubljanaNodes$x
    myy = LjubljanaNodes$y
    net<-intergraph::asNetwork(mysubg)
    vs <- data.frame(onset=0, 
                     terminus = 8, 
                     vertex.id=LjubljanaNodes$id)
    es <- data.frame(onset=c(rep(0,21), rep(1,21), rep (2,21), rep(3,21), rep (4,21), rep (5,21), rep (6,21), rep (7,21)), 
                     terminus = 8, 
                     head=as.matrix(net, matrix.type="edgelist")[,1],
                     tail=as.matrix(net, matrix.type="edgelist")[,2])
    net.dyn <- networkDynamic(base.net=net, edge.spells=es, vertex.spells=vs)
    mycoord = as.matrix(cbind(myx,myy))
    colnames(mycoord) = c("x", "y")
    compute.animation(net.dyn, 
                      animation.mode = "kamadakawai",
                      slice.par=list(start=0, end=7, interval=1,
                                     aggregate.dur=1, rule='any'),
                      seed.coords = mycoord, verbose = FALSE)
    main = "Subnetwork has to many/less edges to be nicely shown.\nCheck if nonexistent cluster or gene ID\nOR\nincrease/decrease minimal node degree threshold."
    tic=proc.time()[3]
    render.d3movie(net.dyn,
                   usearrows = FALSE,
                   displaylabels = FALSE,
                   vertex.border="#ffffff",
                   vertex.col = rainbow(dim(LjubljanaNodes)[1]),
                   edge.col =  rainbow(dim(LjubljanaEdges)[1]),
                   plot.par = list(bg='#ffffff', main = main),
                   launchBrowser=FALSE,
                   filename = 'mytempHTML.html',
                   output.mode = "HTML",#'htmlWidget',
                   render.par=list(tween.frames = 5, show.time = FALSE, initial.coords=mycoord),
                   script.type='embedded', 
                   verbose = FALSE)
    toc=proc.time()[3] - tic
    (toc/60)
    
  }
  # sink()
  # sink(type="message")
  
}
