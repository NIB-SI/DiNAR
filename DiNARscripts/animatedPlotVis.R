# sourced in global.R

################################################################################
#' Plot Animated Network
plotAnimatedNetworksVisAnimated <- function(nlist, elist, palette, clusterID, t0){
  
  palette = (isolate(mypalette()))

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

  # generate points based from ui.R
  # t0 <- (tanimVis())
  # print(t0)
  tst(t0)
  
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

  
  ######## ######## ######## ######## ######## ######## ######## ######## ######
  
  # number of nodes and vertices per selected cluster
  n <- nlist[[1]]
  #if(.test) print(head(n))
  e <- elist[[1]]
  #if(.test) print(head(e))
  
  minDegreeN = strtoi(trimws(input$mydegree))
  main <- paste("Cluster:",paste(clusterID, collapse=", "),
                paste0(" [minDegree: ", minDegreeN, "]"),
                "\nn:",nrow(n),"e:",nrow(e))

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

  # background
  bkg <- '#E3E3E3' # '#D9D9D9' # gray(0.8)
  
  #
  # sel is used for partial plotting (future ...) can be used for selection of active edges
  #
  sel <- 1:nrow(elist[[1]]) ####  ####  ####  ####  ####  ####  ####  ####  all
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
    
  # this is 'global' min max == per all tp

  minmax = max(abs(isolate(colMaxMin())))

    
  # strength of the edge abs(DEnode1) + abs(DEnode2); sel::allEdges
  abs1 = abs(nlist[[i]][match(elist[[i]]$geneID1, nlist[[i]]$geneID),exprCol][sel])
  max1 = max(abs1, 1)
  abs2 = abs(nlist[[i]][match(elist[[i]]$geneID2, nlist[[i]]$geneID),exprCol][sel])
  max2 = max(abs2, 1)
  max3 = max(abs1/max1 + abs2/max2)
  lwd1 <- (abs1/max1 + abs2/max2)/max(max3, 1)
  lwd1[is.na(lwd1)] <- 0
  lwd1 = ifelse(elist[[i]][,edgeCol] !=0, lwd1, 0) ####  ####  ####  ####  ####
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
  lwd2 = ifelse(elist[[i]][,edgeCol] !=0, lwd2, 0) ####  ####  ####  ####  ####
  ise2 <- elist[[i]][,edgeCol][sel]   # edge strngth/presence
  tst(max(lwd2))   
    
  # par(oma=c(0,0,0,0),mar=c(2,1,3,2))

  selnNonDE <- intersect(selnNonDE1, selnNonDE2)

    

  ############################################################################
  # Nodes expression: cex1 and cex2 (DE)
  # Edges expression: lwd1 and lwd2 [0,1]
  ############################################################################    
    
  # this is absolute threshold for boundary colour assignment - user defined
  minmax = isolate(colMaxMin())
  
  ncol = length(coln) 
  allInd = seq(1,length(cex2),1) # node order
  

  #### #### #### size in discrete time point/condition ### #### #### #### ####
  # day x
  nodeCex1 = (abs(cex1))/max(abs(cex1), 1)
  # day x + 1
  nodeCex2 = (abs(cex2))/max(abs(cex2), 1)
  
  ######## ######## ######## in tanim2 ######## ########  ########  ########  ##
  # vertice cex in time t
  nodeCex = h(nodeCex1, nodeCex2, t)
  # DE edges in time t
  isex = h(ise1,ise2,t)
  tst(max(isex))
  
  #### #### #### colours in time point/condition t0 ### #### #### #### #### ####
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
  selFIX = which(lwdx != 0)
  tst(max(lwdx*16+1))
  temp = lwdx*16+1
  temp[which(temp > length(cole))] = length(cole)
  colex = cole[temp]
  
  # lwdx <- 4*lwdx + 0.5 ####  ####  ####  ####  ####  ####  ####  ####  ####  #
  
  
  ######## ######## ######## ######## ######## ######## ######## ######## ######
  
  ######## ######## ######## ######## ######## ######## ######## ######## ######
  
  ######## ######## ######## ######## ######## ######## ######## ######## ######
    
  # shiny slider keeps time running
  i <- day + 1
  nt <- ntp
  tst(nt)
  
  nc <- length(coln)

  mytext = ifelse(n$shortName !='-', n$shortName, n$geneID)
  tmptext = strsplit(mytext, '')
  temp = list(as.vector(rep(' ', max(unlist(lapply(tmptext, function(x) (length(x)))))+1)))
  
  temp = lapply(1:length(tmptext), function(x) temp[[x]] = temp[[1]])
  temp = lapply(1:length(temp), function(x) {
    temp[[x]][1:length(tmptext[[x]])] = tmptext[[x]]
    temp[[x]] = paste(temp[[x]], collapse = '')
  })
  mytext = temp
  tmph = as.numeric(formatC(h(cex1, cex2, t), format = "f", digits = 2))
  mytext = paste0(mytext,'\n[',tmph, ']')
  mytext[which(tmph == 0)] = ''
  
  cexx <- 5*nodeCex + 1.5
  lwdx <- 5*lwdx + 1.0 ####  ####  ####  ####  ####  ####  ####  ####  ####  ###
  
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
  
  to <- c(-1, 1)
  from <- range(n$x, na.rm = TRUE, finite = TRUE)
  n$x <- (n$x - from[1])/diff(from) * 
    diff(to) + to[1]
  from <- range(n$y, na.rm = TRUE, finite = TRUE)
  n$y <- (n$y - from[1])/diff(from) * 
    diff(to) + to[1]
  
  
  nodes <- data.frame(id = n$geneID,
                      # add labels on nodes
                      label = mytext,
                      # size adding value
                      value = cexx,
                      # size = cexx,
                      # http://stackoverflow.com/questions/39674927/how-have-labels-inside-the-scaled-nodes-in-visnetwork
                      font.size = cexx, 
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
                      dashes = as.logical(mydashes),
                      id = seq(1, dim(e)[1], 1))
  
  
  # cat("cexx, lwdx", cexx, lwdx, '\n')
  return(list(n=nodes, e=edges, main = main))

  ######## ######## ######## ######## ######## ######## ######## ######## ######

  
}
