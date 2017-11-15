ABanimatoRfunctionAB <- function(n = n, e = e, clusterID = clusterID, isOK){
  
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
    
    

    if(!is.null(dim(myFiles))) {
      n.of.tp = 1
    } else {
      n.of.tp = length(myFiles)
    }

    # mypalette = mypalette
    
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
    
    node1 = n
    edge1 = e
    

    mySubnetworkNodes = list()
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
      node1[, ncol(node1)]= rep(0.00, length(node1[, ncol(node1)]))
      # cluster node ID that exist in experimental data: node1$geneID[ind2]
      # node ID in experimental data that match cluster node ID : tp[ind[ind2],myIDcol1]
      # any(node1$geneID[ind2] != tp[ind[ind2],myIDcol1])
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
      # edge1[, ncol(edge1)] = abs(node1[, ncol(node1)][ind3]*node1[, ncol(node1)][ind4])
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
      # node ID in experimental data that match cluster node ID : tp[ind[ind2],myIDcol1]
      # any(node1$geneID[ind2] != tp[ind[ind2],myIDcol1])
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
      # edge1[, ncol(edge1)] = abs(node1[, ncol(node1)][ind3]*node1[, ncol(node1)][ind4])
      edge1[, ncol(edge1)] = ifelse(abs(node1[, ncol(node1)][ind3]*node1[, ncol(node1)][ind4]) != 0.00, 1, 0.00)
      mySubnetworkEdges[[i]] = edge1 # nodes
      
    }
    
    edges0 =  mySubnetworkEdges[1]
    edges0[[1]][,dim(edges0[[1]])[2]] = rep(0, dim(edges0[[1]])[1])
    mySubnetworkEdges = c(edges0, mySubnetworkEdges)
    

    ############################################################################
    plotAnimatedNetworksAB( n = mySubnetworkNodes , 
                          e = mySubnetworkEdges , 
                          clusterID = clusterID)
    #
    ##############################################################################
    # pie(c(Sky = 78, "Sunny side of pyramid" = 17, "Shady side of pyramid" = 5),
    #     init.angle = 315, col = c("deepskyblue", "yellow", "yellow3"),
    #     border = FALSE)
  }
}
