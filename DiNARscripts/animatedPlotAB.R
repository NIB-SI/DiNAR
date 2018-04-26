# sourced in global.R

################################################################################
#' Plot Animated Network
#'
#' @param nlist list of dataframes with node data
#' @param elist list of dataframes with edge data
#' @param palette color palette
#' @param clusterID numeric, cluster identification
#' @return NULL
#' @export
#' @seealso \code{\link{funkcija}}
#' @note
#' @references
#' @keywords graphics, animation, animatoR
#' @title Plot Animated Network
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' is.pseudoprime(13, 4) # TRUE most of the time
#' 
#' 
plotAnimatedNetworksAB <- function(nlist, elist, clusterID){
  
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
  t0 <- (tanim2())
  tst(t0)
  
  
  # To generate .pdf animation
  # subDir <- "./plots"
  # dir.create(file.path(subDir), showWarnings = FALSE)
  # myfilename = paste0("SampleGraph", length(list.files(subDir))+1, '.pdf')
  # myfilepath = file.path(subDir)
  # par(mai=c(2.0, 2.0, 2.0, 2.0))
  
  
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
  # minmax

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
  
  # bottom, left, top, right  
  par(oma=c(2,1,1,2))  # outer margins
  par(mar=c(3,1,2,5))  # margin, lines

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
  
  lwdx <- 4*lwdx + 0.5


  ######## ######## ######## ######## ######## ######## ######## ######## ######
  
  ######## ######## ######## ######## ######## ######## ######## ######## ######
  
  ######## ######## ######## ######## ######## ######## ######## ######## ######
    
  # shiny slider keeps time running
  i <- day + 1
  nt <- ntp
  tst(nt)
  
  yLimits = range(n$y) # isolate(animatoRNE())$yLimits # range(n$y)  ####  ####  ####  ####  ####  ####  ####  ####  ####
  xLimits = range(n$x) # isolate(animatoRNE())$xLimits # range(n$x)  ####  ####  ####  ####  ####  #### 
  

  newplot(xlim=c(range(xLimits)[1], range(xLimits)[2]+10),
          ylim=yLimits + c(-0, 0),
          pty="m") #, asp=12/16)
  

  title(main,xpd=TRUE)
             
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

  # time/condition scale
  # axis(1,at=seq(min(n$x),max(n$x),length=nt),labels=0:(nt-1))
  # axis(1,at=seq(min(xLimits),max(xLimits),length=nt),labels=0:(nt-1))
  pp=par("usr")
  axis(1,at=seq(min(xLimits), max(xLimits),length=nt),
       labels=0:(nt-1), 
       pos=pp[3])
  # abline(h=pp[3:4], col=2,xpd=TRUE)
  # abline(v=pp[1:2], col=2,xpd=TRUE)          
  # time 
  # points(min(n$x)+(day+t)/(nt-1)*diff(range(n$x)),par("usr")[3],pch=16,cex=2)
  # print(t)
  # print (pp)
  
  # pie(rep(1, 12), col = gray.colors(12))
  points(min(xLimits)+(day+t)/(nt-1)*diff(range(xLimits)), 
         pp[3],
         pch=16,
         cex=1.5,
         xpd=TRUE,
         col=gray.colors(12)[2])
  text(min(xLimits)+(day+t)/(nt-1)*diff(range(xLimits))+strheight("/", cex = 2)*0.75,
       pp[3]-strheight("/", cex = 2)*0.75,
       formatC(round(day+t,2), 2, format = "f"),
       col=gray.colors(12)[2], 
       xpd=TRUE)
  #mtext(side=1,line=1,round(day+t,2),adj=(day+t)/(nt-1),col=8)
  #mtext(side=1,line=-1, adj=(-0.0),"time/condition")
  text(min(xLimits), 
       pp[3]+strheight("/", cex = 2)*0.75,
       "time/condition",
       col=gray.colors(12)[2], 
       xpd=TRUE, 
       adj=(-0.0))
  
  # # Background
  # edges
  segments(
    nlist[[i]][elist[[i]]$geneID1,"x"],
    nlist[[i]][elist[[i]]$geneID1,"y"],
    nlist[[i]][elist[[i]]$geneID2,"x"],
    nlist[[i]][elist[[i]]$geneID2,"y"],
    col='#F3F3F3', # bkg,
    lwd=0.5)
  # # Background
  # vertices
  points(nlist[[i]]$x,nlist[[i]]$y,
         col='#E6E6E6', # bkg, 
         pch=16, cex=1.5)# cex=0.75) # Background
  # 
              
  ##################################################################
  # # Active
  # # short node name
  mytext = ifelse(n$shortName !='-', n$shortName, n$geneID)
  ts = round(h(coln1,coln2,t),2)
  mytext[which(round(abs(ts)) == 0)] = ''
  
  # nodes
  cexx <- 4*nodeCex + 1.5
  tst(max(cexx))
  tst(min(cexx))
  
  # edges
  tst(max(lwdx))
  tst(min(lwdx))
  

  # active
  segments(
    # nlist[[i]][elist[[i]]$geneID1[sel],"x"],
    # nlist[[i]][elist[[i]]$geneID1[sel],"y"],
    # nlist[[i]][elist[[i]]$geneID2[sel],"x"],
    # nlist[[i]][elist[[i]]$geneID2[sel],"y"],
    nlist[[i]][match(elist[[i]]$geneID1[selFIX], nlist[[i]]$geneID),"x"],
    nlist[[i]][match(elist[[i]]$geneID1[selFIX], nlist[[i]]$geneID),"y"],
    nlist[[i]][match(elist[[i]]$geneID2[selFIX], nlist[[i]]$geneID),"x"],
    nlist[[i]][match(elist[[i]]$geneID2[selFIX], nlist[[i]]$geneID),"y"],
    col=colex[selFIX],
    lwd=lwdx[selFIX])
  
  # active
  act = setdiff(seq(1, dim(nlist[[i]])[1], 1), selnNonDE)
  points(nlist[[i]][act,]$x,nlist[[i]][act,]$y, pch=21,
  bg = colx[act],
  col = colx[act], # circle around node, actice nodes should cover inactive ones
  cex = cexx[act])
  
  # THIS TRIGGERS FLICKER
  # txtcx = abs(h(cex1,cex2,t))/(max(abs(h(cex1,cex2,t)), 1)*2)
  # tst(txtcx)
  # textCol = rgb(0,0,0, alpha = 0.5 +   tst(txtcx))
  # textCol = rgb(0,0,0, alpha = 0.8)

  text(x = nlist[[i]]$x,
       y = nlist[[i]]$y,
       labels = mytext,
       cex = 3*cexx/(3*max(cexx,1)),
       offset = 0.0,
       col = 'black')
  ######## ######## ######## ######## ######## ######## ######## ######## ######
  # dev.copy2pdf(file = paste0(myfilepath, '/', myfilename), width=24, # height=18, 
  #             out.type="pdf")
  

}
