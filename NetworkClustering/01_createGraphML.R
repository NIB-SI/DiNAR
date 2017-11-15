## MZ

if (!require("devtools")) install.packages("devtools")
if (!require("RJSONIO")) install.packages("RJSONIO")
library(devtools)
library(RJSONIO)

if (!require("rPython")) install(paste0(getwd(), "/rPython"))
library(rPython)

myLKNname = isolate(myLKNname0())
print(myLKNname)

(dir0 = getwd())
dir1 = paste(dir0,'/netTables',sep='')
(filelist = sort(list.files(path = dir1,pattern = "\\.txt$")))
(length(filelist))
iter = seq(1,(length(filelist))/2,1)

cnt = 0
k = 1
# for (k in iter){
  cat ('network No.', k)

  a = filelist[k+cnt+1] # nodes
  b = filelist[k+cnt] # edges
  tmp = strsplit(b, 'edges')[[1]][1]

  # c = paste0(tmp[1], formatC(k, width = 3, format = "d", flag = "0"), '.graphml')
  c = paste0(myLKNname, '.graphml')
  # print (c)
  
  python.assign( "dir1", dir1 )
  python.assign( "a", a ) # nodes
  python.assign( "b", b ) # edges
  python.assign( "c", c )
  
  
  python.load("graphMLforRigraph.py")
  
  file.copy(from=paste0(dir1,'/',c), to=dir0, copy.mode = TRUE)
  
  cnt = cnt + 1

# }
cat ('\n', c, 'Done', '\n')



