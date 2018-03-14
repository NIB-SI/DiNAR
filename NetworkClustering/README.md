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
see https://github.com/NIB-SI/DiNAR/blob/master/subApps/clustering/


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

