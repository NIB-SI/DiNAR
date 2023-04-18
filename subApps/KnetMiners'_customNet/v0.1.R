## https://knetminer.com/poaceae/
# Keyword Search: wrky33

fn = 'knetminer_network.json'
net = jsonlite::fromJSON(fn)

e = net$elements$edges$data
n = cbind(net$elements$nodes$data, net$elements$nodes$position)
rm(net)



ind = match(c('id', 'x', 'y', 'conceptType', 'displayValue', 'flagged'), colnames(n))
n = n[, ind]
n$geneID = n$id
colnames(n) = c('id', "x", "y", "shortDescription", "shortName", "MapManBin", "geneID")

ind = match(c('source', 'target', 'label'), colnames(e))
e = e[, ind]
e$geneID1 = e$source
e$geneID2 = e$target
colnames(e) = c('source', 'target', 'reactionType', 'geneID1', 'geneID2')

# n$y = -n$y
n$x = -n$x

e$exists = 1
n$expressed = 1


library(igraph)
g = graph_from_data_frame(d = e, directed = FALSE, vertices = n)


mydeg = igraph::degree(g, loops = FALSE, normalized = FALSE,  mode = "all")
V(g)$clusterSimplifiedNodeDegree <- mydeg

V(g)$clusterID = 1



E(g)$clusterID_geneID1 = V(g)$clusterID[match(E(g)$geneID1,  V(g)$geneID)]
E(g)$clusterID_geneID2 = V(g)$clusterID[match(E(g)$geneID2,  V(g)$geneID)]
E(g)$clusterSimplifiedNodeDegree_geneID1 = V(g)$clusterSimplifiedNodeDegree[match(E(g)$geneID1,  V(g)$geneID)]
E(g)$clusterSimplifiedNodeDegree_geneID2 = V(g)$clusterSimplifiedNodeDegree[match(E(g)$geneID2,  V(g)$geneID)]


# plot(0, type = "n",
#      axes = FALSE,
#      xlim = extendrange(V(g)$x),
#      ylim = extendrange(V(g)$y),
#      xlab = '',
#      ylab = '')
# 
# plot(g, layout = cbind(V(g)$x, V(g)$y),
#      edge.label = E(g)$reactionType,
#      vertex.label = V(g)$shortName,
#      vertex.size = scales::rescale(V(g)$clusterSimplifiedNodeDegree)*1000,
#      vertex.color = 'yellow',
#      rescale = FALSE, add = TRUE,
#      vertex.label.cex = 0.75,
#      edge.arrow.size = 0.25,
#      edge.arrow.width = 0.25,
#      edge.lty = 'solid',
#      edge.color = 'gray',
#      edge.width = 0.25,
#      edge.label.cex = 0.5)


v = vertex_attr_names(g) 
e = edge_attr_names(g)

df1 = matrix(NA, vcount(g), length(v))
for (i in 1:length(v)) {
  df1[,i] = vertex_attr(g,v[i])
}
df1 = as.data.frame(df1, stringsAsFactors = FALSE)
colnames(df1) = v



colNames = toupper(c("geneID", "shortDescription", "shortName", "MapManBin",
                     "clusterID", "x", "y", "clusterSimplifiedNodeDegree", "expressed"))

importantColsE = unlist(sapply(colNames,
                               function(x) grep(paste("^",x,"$", sep = ""),
                                                toupper((v)))))

df1 = df1[,importantColsE]

df2 = matrix(NA, ecount(g), length(e))
for (i in 1:length(e)) {
  df2[,i] = edge_attr(g,e[i])
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
colnames(df2)

df2 = df2[,importantColsE]

colnames(df2)


write.table(df1, file = 'customNet-nodes.txt', 
            row.names = FALSE,
            append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".")

write.table(df2, file = 'customNet-edges.txt', 
            row.names = FALSE,
            append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".")

dummy = as.data.frame(cbind(df1$geneID, df1$MapManBin, 0))
colnames(dummy) = c('geneID', 'val', 'stat')
# dummy$val = as.logical(dummy$val)
dummy$val = ifelse(as.logical(dummy$val), 2, -0.5)

write.table(dummy, file = 'customNet-dummyExpression.txt', 
            row.names = FALSE,
            append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".")



