######## ######## ######## global files for all server users ######## ######## #
# Start the clock!
ptm <- proc.time()

### use relative path
# DATA_PATH <- file.path(getwd(), "/78aw64VzX1yAk7Yz/FdLI6cnm2jZ54u76/jI7Fw9J8zf6xO51q")
DATA_PATH <- file.path("./78aw64VzX1yAk7Yz/FdLI6cnm2jZ54u76/jI7Fw9J8zf6xO51q")
# DATA_PATH <- file.path("https://raw.githubusercontent.com/NIB-SI/DiNARscripts/master/78aw64VzX1yAk7Yz/FdLI6cnm2jZ54u76/jI7Fw9J8zf6xO51q")
#### Ath
myfile = 'C5c4lI1bwYc39L9j'# the names are fixed, otherwise use filelist[[1]]
title.line <- readLines(paste0(DATA_PATH,'/',myfile), n=1)
title.line = strsplit(title.line, "\t")[[1]]

edgesAth <- read.table(paste0(DATA_PATH,'/',myfile), skip=1, 
                       stringsAsFactors=FALSE, 
                       header = FALSE, sep = "\t", col.names = title.line,
                       quote="", comment.char="")

myfile = 'GeI639CK5z5o7ynu'# the names are fixed, otherwise use filelist[[2]]
title.line <- readLines(paste0(DATA_PATH,'/',myfile), n=1)
title.line = strsplit(title.line, "\t")[[1]]

nodesAth <- read.table(paste0(DATA_PATH,'/',myfile), skip=1, 
                       stringsAsFactors=FALSE, 
                       header = FALSE, sep = "\t", col.names = title.line,
                       quote="", comment.char="")

# (sort(unique(nodesAth$clusterID)))


#### SoTub
myfile = 'vIHv6RJ3zk315y1o'# the names are fixed, otherwise use filelist[[3]]
title.line <- readLines(paste0(DATA_PATH,'/',myfile), n=1)
title.line = strsplit(title.line, "\t")[[1]]

edgesSoTub <- read.table(paste0(DATA_PATH,'/',myfile), skip=1, 
                         stringsAsFactors=FALSE, 
                         header = FALSE, sep = "\t", col.names = title.line,
                         quote="", comment.char="")

myfile = 'X5r56Mz7ZuxR9ot3'# the names are fixed, otherwise use filelist[[4]]
title.line <- readLines(paste0(DATA_PATH,'/',myfile), n=1)
title.line = strsplit(title.line, "\t")[[1]]

nodesSoTub <- read.table(paste0(DATA_PATH,'/',myfile), skip=1, 
                         stringsAsFactors=FALSE, 
                         header = FALSE, sep = "\t", col.names = title.line,
                         quote="", comment.char="")
# (sort(unique(nodesSoTub$clusterID)))

# Stop the clock
print(proc.time() - ptm) # in seconds
