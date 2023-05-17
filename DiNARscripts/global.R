# global.R zagor

if (!require(devtools)) {
  install.packages("devtools")
  require(devtools)
}
if (!require(remotes)) {
  install.packages("remotes")
  require(remotes)
}
if (!require(DT)) {
  remotes::install_github("rstudio/DT")
}
if (!require(animatoR)) {
  devtools::install_github("ablejec/animatoR")
}
if (!require(shinyIncubator)) {
  devtools::install_github("rstudio/shiny-incubator")
}
if (!require(shinysky)) {
  devtools::install_github("AnalytixWare/ShinySky")
}
if (!require(rWordCloud)) {
  devtools::install_github('adymimos/rWordCloud')
}

######## packages ######## 
# http://stackoverflow.com/questions/15155814/check-if-r-package-is-installed-then-load-library
lubripack <- function(...,silent=FALSE){

  #check names and run 'require' function over if the given package is installed
  requirePkg<- function(pkg){if(length(setdiff(pkg,rownames(installed.packages())))==0)
    require(pkg, quietly = TRUE,character.only = TRUE)
  }

  packages <- as.vector(unlist(list(...)))
  if(!is.character(packages))stop("No numeric allowed! Input must contain package names to install and load")

  if (length(setdiff(packages,rownames(installed.packages()))) > 0 )
    install.packages(setdiff(packages,rownames(installed.packages())),
                     repos = c("https://cran.revolutionanalytics.com/", "http://owi.usgs.gov/R/"))

  res<- unlist(sapply(packages, requirePkg))

  if(silent == FALSE && !is.null(res)) {cat("\nBellow Packages Successfully Installed:\n\n")
    print(res)
  }
}

# http://deanattali.com/2015/06/28/introducing-shinyjs-colourinput/
# https://github.com/manlius/muxViz/issues/5
# library(colourpicker)


# http://stackoverflow.com/questions/32824091/an-error-in-deploying-shiny-app-to-shinyapps-io
lubripack('V8', 'shiny', 'igraph', 'colourpicker', 'plotly', 'ggplot2', 'calibrate',
          'stringi', 'magrittr', 'yaml',
          'animatoR', 'stringr', 'wordcloud2', 'shinyjs', 'shinydashboard',
          'shinyBS', 'colorspace', 'knitr', 'markdown', 'Rcpp', 'dplyr', 'rdrop2',
          'fBasics', 'shinyIncubator', 'shinysky', 'downloader', 'visNetwork',
          'htmltools', 'htmlwidgets', "intergraph", "network", "ndtv", "shinyFiles",
          "pryr")

library('V8')
library('shiny')
library('igraph')
library('colourpicker')
library('plotly')
library('ggplot2')
library('calibrate')
library('animatoR')
library('stringi')
library('magrittr')
library('stringr')
library('yaml')
library('wordcloud2')
library('shinyjs')
library('shinydashboard')
library('shinyBS')
library('colorspace')
library('knitr')
library('markdown')
library('Rcpp')
library('dplyr')
library('rdrop2')
library('fBasics')
library('shinyIncubator')
library('shinysky')
library("downloader")
library("visNetwork")
library("htmltools")
library("htmlwidgets")
#library("DiagrammeR")
#library("metricsgraphics")
library("intergraph")
library("network")
library("ndtv")
library("shinyFiles")
library("pryr")

jscode <- "shinyjs.refresh = function() { history.go(0); }"


'%ni%' <- Negate('%in%') 
specify_decimal <- function(x, k) format(round(x, k), nsmall=k)
frac <- function(x) abs(x - trunc(x))
#.dec.to.hex()
#.hex.to.dec()

# http://mathworld.wolfram.com/LCFNotation.html
Ljubljana <- graph_from_lcf(112, 
                            c(47, -23, -31, 39, 25, -21, -31, -41, 25, 15, 29, 
                              -41, -19, 15, -49, 33, 39, -35, -21, 17, -33, 49, 
                              41, 31, -15, -29, 41, 31, -15, -25, 21, 31, -51, 
                              -25, 23, 9, -17, 51, 35, -29, 21, -51, -39, 33, -9, 
                              -51, 51, -47, -33, 19, 51, -21, 29, 21, -31, 
                              -39), 
                            2)
LjubljanaEdges = data.frame(as_edgelist(Ljubljana), physics = FALSE, color = '#009900')
names(LjubljanaEdges) = c('from', 'to', 'physics', 'color')
tmpNodes = seq(1, length(V(Ljubljana)), 1)
LjubljanaLayout = layout_in_circle(Ljubljana)
LjubljanaNodes = data.frame(tmpNodes, LjubljanaLayout[,1], LjubljanaLayout[,2], physics = FALSE, color = '#009900')
names(LjubljanaNodes) = c("id", "x", "y", 'physics', 'color')
# visNetwork(LjubljanaNodes, LjubljanaEdges, height = "100%", width = "100%", main = 'test') %>% visIgraphLayout()

source("LKNs.R")
source("ndtvd3Threshold.R")
# 
# source("staticDETPPlotVis.R")
# 
# source("staticDETPPlotAB.R")
# 
# source("interactiveBCKGPlot.R")
# 
# source("dataPreparationForAnimatorAB.R")
# source("dataPreparationForAnimatorVIS.R")
# 
# source("animatedPlotAB.R")
# source("animatedPlotVis.R")

# source('knitMe.R')

################################################################################

myfile = 'AthNodesTEST.txt'
title.line <- readLines(myfile, n=1)
title.line = strsplit(title.line, "\t")[[1]]

AthNodesTEST <- read.table(myfile, skip=1, 
                           stringsAsFactors=FALSE, 
                           header = FALSE, sep = "\t", col.names = title.line,
                           quote="", comment.char="")
myfile = 'AthEdgesTEST.txt'
title.line <- readLines(myfile, n=1)
title.line = strsplit(title.line, "\t")[[1]]

AthEdgesTEST <- read.table(myfile, skip=1, 
                           stringsAsFactors=FALSE, 
                           header = FALSE, sep = "\t", col.names = title.line,
                           quote="", comment.char="")

myfile = 'A.thalianaE.G.Condition1.tsv'
title.line <- readLines(myfile, n=1)
title.line = strsplit(title.line, "\t")[[1]]

A.thalianaE.G.Condition1 <- read.table(myfile, skip=1, 
                                       stringsAsFactors=FALSE, 
                                       header = FALSE, sep = "\t", col.names = title.line,
                                       quote="", comment.char="")
myfile = 'A.thalianaE.G.Condition2.tsv'
title.line <- readLines(myfile, n=1)
title.line = strsplit(title.line, "\t")[[1]]
A.thalianaE.G.Condition2 <- read.table(myfile, skip=1, 
                                       stringsAsFactors=FALSE, 
                                       header = FALSE, sep = "\t", col.names = title.line,
                                       quote="", comment.char="")
myfile = 'A.thalianaE.G.Condition3.tsv'
title.line <- readLines(myfile, n=1)
title.line = strsplit(title.line, "\t")[[1]]
A.thalianaE.G.Condition3 <- read.table(myfile, skip=1, 
                                        stringsAsFactors=FALSE, 
                                        header = FALSE, sep = "\t", col.names = title.line,
                                        quote="", comment.char="")

################################################################################

sessionInfo()
print(mem_used())

# http://stackoverflow.com/questions/26447118/includehtml-for-shiny-shinyapps-io-and-dropbox#26466113
