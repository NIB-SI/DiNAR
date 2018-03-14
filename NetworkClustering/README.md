# Requirement (for Windows):
* install python 2.7.x <https://www.python.org/> and NumPy <http://www.numpy.org/> (<http://www.lfd.uci.edu/~gohlke/pythonlibs/#numpy>, <https://pypi.python.org/pypi>)
* install R-3.2 or higher <https://cran.r-project.org/> (*does not work with latest versions of R and updated packages*)
* follow the steps described in "README.md" at https://github.com/cjgb/rPython-win
* open R and paste to console
```{r}
if (!require("devtools")) install.packages("devtools")
devtools::install_github("rstudio/shiny")
```

# Network clustering
1. Download all scripts: ui.R, server.R, createGraphML.R, multilevel_and_spinglass_clustering.Rmd (https://github.com/NIB-SI/DiNAR/blob/master/subApps/clustering/multilevelAndSpinglassClustering.Rmd) and graphMLforRigraph.py
2. open server.R or ui.R in RStudio and Run App with Control+Shift+Enter (see <http://shiny.rstudio.com/tutorial/lesson1/>)
3. wait until 
```{r}
"DONE! 
You may close the app and check the results."
``` 
appears in console


# Custom Network (CN) Minimal Working Example
* Check Terminator folder
* Nodes table obligatory columns: nodeID ->  shortDescription	-> shortName	-> MapManBin
  + nodeID: string
  + shortDescription: string
  + shortName: string
  + MapManBin: see <http://www.gomapman.org/ontology>
* Edges table obligatory columns: ID1	-> ID2	-> type
  + geneID1 == nodeID1 (from)
  + geneID2 == nodeID2 (to)
  + reactionType (reaction type):	'NA',	'-', 'binding',	'unk_TF',	'activation',	'act_TF',	'inhibition',	'synthesis',	'inh_TF', act_binding/oligomerisation, act_catalysis/autocatalysis, act_proteindephosphorylation, act_proteinphosphorylation, act_transcription, act_translation, act_translocation, act_undefined, inh_binding/oligomerisation, inh_degradation/secretion, inh_proteinphosphorylation, inh_transcription, inh_translation, inh_undefined, unk_binding/oligomerisation, ....


# code ref:
* https://github.com/cjgb/rPython-win

