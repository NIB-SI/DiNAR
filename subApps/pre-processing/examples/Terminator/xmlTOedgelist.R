# http://moviegalaxies.com/movies/index/query:terminator

library(igraph)
library(rgexf)

g1=read.gexf("809.gexf.xml")
g2=read.gexf("810.gexf.xml")
g3=read.gexf("811.gexf.xml")
             
i1=gexf.to.igraph(g1)
i2=gexf.to.igraph(g2)
i3=gexf.to.igraph(g3)


el1 = get.edgelist(i1)
el2 = get.edgelist(i2)
el3 = get.edgelist(i3)

write.table(el1, file = "el1", append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = FALSE)
write.table(el2, file = "el2", append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = FALSE)
write.table(el3, file = "el3", append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = FALSE)
