# http://sape.inf.usi.ch/quick-reference/ggplot2/geom_segment

myplotlys <- function(n, e, clusterID, isOK) {
  
  if ((!isOK) | (sum(e$geneID1 != e$geneID2) < 1)) {
    main = 'Subnetwork has to many/less edges to be nicely shown.</br>Check if nonexistent cluster or gene ID</br><span style="color: #f00;"> OR </span>increase/decrease minimal node degree threshold.</br>'
      gg =  ggplot(data = LjubljanaNodes, aes(LjubljanaNodes$x, LjubljanaNodes$y)) +
        geom_segment(mapping = aes(x = LjubljanaNodes[LjubljanaEdges$from,"x"],
                                   y = LjubljanaNodes[LjubljanaEdges$from,"y"],
                                   xend = LjubljanaNodes[LjubljanaEdges$to,"x"],
                                   yend = LjubljanaNodes[LjubljanaEdges$to,"y"]),
                     color = LjubljanaEdges$color,
                     data = LjubljanaEdges) +
        geom_point(colour = LjubljanaNodes$color,
                   cex = 4) + #,
        scale_y_continuous("") +
        scale_x_continuous(name = "Ljubljana graph") +
        # theme(axis.text = element_text(NULL)) +
        # scale_color_gradient()
        theme(axis.line=element_blank(),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              # axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              #legend.position="none",
              legend.title=element_blank(),
              panel.background=element_blank(),
              panel.border=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              plot.background=element_blank()) +
        ggtitle(paste0(main, '</br>'))
      
      gg <- plotly_build(gg)
      # https://community.plot.ly/t/how-to-add-long-title-in-ggplotly/1507/3
      gg$layout$margin$l = 100
      gg$layout$margin$r = 200
      gg$layout$margin$b = 200
      gg$layout$margin$t = 500
      gg$layout$margin$pad = 0
      # gg$autosize = FALSE
      # gg$height = 500
      # gg$width = 500
      return(gg)
  } else {
    
    sel <- which(e[,1] != e[,2]) # select edges
    e <- e[sel,] # cannot plot loops
    main <- paste('Background network cluster - ',
                  "cluster:",paste(clusterID, collapse=", "),
                  "<br>","#n:",nrow(n),"#e:",dim(e)[1], "shown",
                  "<br>")
    
    mye = e[,c(1,3,2)]
    tmp1 <- lapply(n$geneID, function(x) mye[which((x == mye[,1])),])
    tmp2 = sapply(1:length(tmp1), 
                  function(y) paste(sapply(1:dim(tmp1[[y]])[1], 
                                           function(x) paste(tmp1[[y]][x,],collapse = "->")), collapse = "</br>")
    )
    empty = unlist(lapply(tmp2, function(x) length(grep('NA->NA->NA',x))))
    tmp2[empty != 0] = '/'
    
    
    tmptext <- paste0("gene ID: ", n$geneID, "</br></br>",
                     "short name: ", n$shortName, "</br>",
                     "short description: ", n$shortDescription, "</br>",
                     "GoMapMan bin: ", n$MapManBin, "</br>",
                     #"simplified node degree in network: ", n$networkSimplifiedNodeDegree, "</br>",
                     #"simplified node degree in super cluster: ", n$superClusterSimplifiedNodeDegree, "</br>",
                     "simplified node degree in cluster: ", n$clusterSimplifiedNodeDegree, "</br>",
                     "targets: ", "</br>", tmp2)
    
    # plotlyGoneWild - showing code also - some 'new' thingy
    nodeName = ifelse(n$shortName!='-', n$shortName, n$geneID)


    gg =  ggplot(data = n, aes(n$x, n$y)) +
      geom_segment(mapping = aes(x = n[e$geneID1,"x"],y = n[e$geneID1,"y"],
                                 xend = n[e$geneID2,"x"],yend = n[e$geneID2,"y"],
                                 color = reactionType),
                   data = e,
                   arrow=arrow()) +
      geom_point(colour = ifelse(n$shortName =='-', 'darkgrey', 'lightgrey'),
                 cex = 4) + #,
                 #aes(text = tmptext)) + #, pch = 21) +
      geom_text(aes(label = nodeName),
                check_overlap = TRUE, color = 'lightcoral', cex = 2) +
      scale_y_continuous("") +
      scale_x_continuous("") +
      theme(axis.text = element_text(NULL)) +
      # scale_color_gradient()
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            #legend.position="none",
            legend.title=element_blank(),
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank()) +
      ggtitle(main)


    gg <- plotly_build(gg)
    # gg <- ggplotly(gg)



    gg$x$data[[length(unique(e$reactionType))+1]]$text <- tmptext#ifelse(n$shortName!='-', n$shortName, n$geneID)

    gg$layout$margin$l = 100
    gg$layout$margin$r = 200
    gg$layout$margin$b = 200
    gg$layout$margin$t = 800
    gg$layout$margin$pad = 0
    gg$autosize = FALSE
    gg$height = 2000
    
    # myplotlydata = as.data.frame(n)
    # myplotlydata$text = ifelse(myplotlydata$shortName!='-', myplotlydata$shortName, myplotlydata$geneID)
    # myplotlydata$name = tmptext
    # 
    # network <- plot_ly(data = myplotlydata,
    #                    x = ~x, 
    #                    y = ~y, 
    #                    type = "scatter",
    #                    mode = "markers+text", 
    #                    text = ~text,
    #                    # name = ~name,
    #                    # hoverinfo = "name",#'text+name'
    #                    marker = list(color = ifelse(n$shortName =='-', 'darkgrey', 'lightgrey'),
    #                                  size = 12
    #                                  ),
    #                    key = ~tmptext2
    #                    ) %>% 
    #   add_markers(text = ~name, hoverinfo = "text",
    #               data = myplotlydata)
    # 
    # axis <- list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
    # 
    # mycol = rainbow(length(unique(e$reactionType)))
    # names(mycol) = unique(e$reactionType)
    # edge_shapes <- list()
    # for(i in 1:dim(e)[1]) {
    #   v0 <- e[i,]$geneID1
    #   v1 <- e[i,]$geneID2
    #   
    #   edge_shape = list(
    #     type = "line",
    #     line = list(color = mycol[e$reactionType][i], width = 0.5),
    #     x0 = n$x[n$geneID == v0],
    #     y0 = n$y[n$geneID == v0],
    #     x1 = n$x[n$geneID == v1],
    #     y1 = n$y[n$geneID == v1]
    #   )
    #   
    #   edge_shapes[[i]] <- edge_shape
    # }
    # 
    # p <- layout(
    #   title = main,
    #   shapes = edge_shapes,
    #   xaxis = axis,
    #   yaxis = axis,
    #   showlegend = FALSE,
    #   font = list(size = 6),
    #   network
    # )
    
    return(gg)
    # return(p)
    

  }
}
