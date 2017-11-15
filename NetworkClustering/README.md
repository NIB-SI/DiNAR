# Requirement (for Windows):
* install python 2.7.x <https://www.python.org/> and NumPy <http://www.numpy.org/> (<http://www.lfd.uci.edu/~gohlke/pythonlibs/#numpy>, <https://pypi.python.org/pypi>)
* install R-3.2 or higher <https://cran.r-project.org/>
* follow the steps described in "README.md" at https://github.com/cjgb/rPython-win
* open R and paste to console
```{r}
if (!require("devtools")) install.packages("devtools")
devtools::install_github("rstudio/shiny")
```

# Network clustering
1. Download zip
2. open server.R or ui.R in RStudio and Run App with Control+Shift+Enter (see <http://shiny.rstudio.com/tutorial/lesson1/>)
3. wait until 
```{r}
"DONE! 
You may close the app and check the results."
``` 
appears in console


# Custom Network (CN) Minimal Working Example
* Check ExampleCN folder
* Nodes table obligatory columns: nodeID ->  nodeLabel	-> shortDescription	-> shortName	-> MapManBin	-> exists
  + nodeID: string
  + nodeLabel: string
  + shortDescription: string
  + shortName: string
  + MapManBin: see <http://www.gomapman.org/ontology>
  + possible values of exists:	-3,	-2,	-1,	0,	1,	2,	3; related to node colour and size; use 1 if not sure why to use other options
* Edges table obligatory columns: ID1	-> ID2	-> type	-> exists
  + ID1 == nodeID1 (from)
  + ID2 == nodeID2 (to)
  + type (reaction type):	'NA',	'-', 'binding',	'unk_TF',	'activation',	'act_TF',	'inhibition',	'synthesis',	'inh_TF'
  + possible values of exists:	0.00,	0.25,	0.50,	0.75,	1.00; related to edge colour and thickness; use 1 if not sure why to use other options


# code ref:
* https://github.com/cjgb/rPython-win

# bonus: The Terminator network
* see Terminator folder
