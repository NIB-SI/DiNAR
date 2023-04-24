<h1><i><b>Di</b>fferential <b>N</b>etwork <b>A</b>nalysis in <span style="color: #0000CC"><b>R</b></span></i></h1>

Examine your omics datasets in the prior knowledge context.

Follow the steps as indicated in interactive menu.

For the help overlay the mouse over the info button or go to Quick help section.

Large knowledge networks of Arabidopsis thaliana and Solanum tuberosum immune signalling are provided.



📋🖋 Zagorščak, M., Blejec, A., Ramšak, Ž. et al. DiNAR: revealing hidden patterns of plant signalling dynamics using Differential Network Analysis in R. Plant Methods 14, 78 (2018). https://doi.org/10.1186/s13007-018-0345-0

🔍 https://omictools.com/dinar-tool (obsolete)

🔍 https://bio.tools/dinar

🔦 http://isbe.si/2018/09/04/dinar-article-published-in-plant-methods/

🔦 https://www.facebook.com/NIBSlovenia/videos/318025485411839/



[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3723901.svg)](https://doi.org/10.5281/zenodo.3723901)



# Run DiNAR from GitHub

## install R-3.x.y or higher :

### Win
https://cran.r-project.org/

### Ubuntu
sudo apt-get install r-base</br>
sudo apt-get install r-base-dev</br>
sudo apt-get -y install libcurl4-gnutls-dev</br>
sudo apt-get -y install libssl-dev</br>
sudo apt-get install libv8-dev</br>

## open R and paste to console

### Win

```{r}
if (!require("devtools")) install.packages("devtools")
if (!require('Rcpp')) install.packages('Rcpp')
devtools::install_github("rstudio/shiny")

shiny:::runGitHub("DiNAR", "NIB-SI", subdir = "DiNARscripts/")
```

### Ubuntu

```{r}
install.packages("devtools", lib="~/R/lib")

shiny:::runGitHub("DiNAR", "NIB-SI", subdir = "DiNARscripts/")
```

*Note: this will install/load libraries: (<a href="https://cran.r-project.org/web/packages/V8/">V8</a>), <a href="https://cran.r-project.org/web/packages/igraph/index.html">igraph</a>, <a href="https://cran.r-project.org/web/packages/colourpicker/index.html">colourpicker</a>, <a href="https://cran.r-project.org/web/packages/plotly/index.html">plotly</a>, <a href="https://cran.r-project.org/web/packages/ggplot2/index.html">ggplot2</a>, <a href="https://cran.r-project.org/web/packages/calibrate/index.html">calibrate</a>, <a href="https://cran.r-project.org/web/packages/stringi/index.html">stringi</a>, <a href="https://cran.r-project.org/web/packages/magrittr/index.html">magrittr</a>, <a href="https://cran.r-project.org/web/packages/yaml/index.html">yaml</a>, <a href="https://github.com/ablejec/animatoR/">animatoR</a>, <a href="https://cran.r-project.org/web/packages/stringr/index.html">stringr</a>, <a href="https://cran.r-project.org/web/packages/wordcloud2/index.html">wordcloud2</a>, <a href="https://cran.r-project.org/web/packages/shinyjs/index.html">shinyjs</a>, <a href="https://cran.r-project.org/web/packages/shinydashboard/index.html">shinydashboard</a>, <a href="https://cran.r-project.org/web/packages/shinyBS/index.html">shinyBS</a>, <a href="https://cran.r-project.org/web/packages/colorspace/index.html">colorspace</a>, <a href="https://cran.r-project.org/web/packages/knitr/index.html">knitr</a>, <a href="https://cran.r-project.org/web/packages/markdown/index.html">markdown</a>, <a href="https://cran.r-project.org/web/packages/Rcpp/index.html">Rcpp</a>, <a href="https://cran.r-project.org/web/packages/dplyr/index.html">dplyr</a>, <a href="https://cran.r-project.org/web/packages/rdrop2/index.html">rdrop2</a>, <a href="https://cran.r-project.org/web/packages/fBasics/index.html">fBasics</a>, <a href="https://github.com/rstudio/shiny-incubator">shinyIncubator</a>, <a href="https://github.com/AnalytixWare/ShinySky">shinysky</a>, <a href="https://cran.r-project.org/web/packages/downloader/index.html">downloader</a>, <a href="https://cran.r-project.org/web/packages/visNetwork/index.html">visNetwork</a>, <a href="https://cran.r-project.org/web/packages/htmltools/index.html">htmltools</a>, <a href="https://cran.r-project.org/web/packages/htmlwidgets/index.html">htmlwidgets</a>, <a href="https://cran.r-project.org/web/packages/intergraph/index.html">intergraph</a>, <a href="https://cran.r-project.org/web/packages/network/index.html">network</a>, <a href="https://cran.r-project.org/web/packages/ndtv/index.html">ndtv</a>, <a href="https://cran.r-project.org/web/packages/shinyFiles/index.html">shinyFiles</a> and <a href="https://cran.r-project.org/web/packages/pryr/index.html">pryr</a>


# Run DiNAR from shinyapps

&#x1F34F; https://NIB-SI.shinyapps.io/DiNAR (Basic - Performance Boost; Instance Size: 8GB; Max Worker Processes: 10; Max Connections per Worker: 1; Max Instances: 3)


# Other options
1. download zip and run locally in RStudio: https://www.rstudio.com/products/rstudio/download/#download http://shiny.rstudio.com/tutorial/lesson1/
2. download zip and deploy: http://shiny.rstudio.com/articles/shinyapps.html http://shiny.rstudio.com/articles/scaling-and-tuning.html
3. download zip and https://support.rstudio.com/hc/en-us/articles/214771447-Shiny-Server-Administrator-s-Guide


# Help

http://conferences.nib.si/DiNAR/


# Additional Data Files

https://github.com/NIB-SI/DiNAR/tree/master/CKNs


# Code References

* http://deanattali.com/2015/06/28/introducing-shinyjs-colourinput/
* http://stackoverflow.com/questions/15155814/check-if-r-package-is-installed-then-load-library


# Create PDF animation
1. in animatedPlotAB.R uncomment lines: 48, 49, 50, 51, 52 and 306
2. install LaTeX (e.g. https://miktex.org/)
3. install animate Package http://tug.ctan.org/macros/latex/contrib/animate/animate.pdf
4. copy to working directory and run LaTeX template document: CreatePDFanimation.tex

# Create gif
1. in animatedPlotAB.R uncomment few lines below `# To generate .pdf animation` comment
2. replace `myfilename = paste0("SampleGraph", length(list.files(subDir))+1, '.pdf')` with `myfilename = paste0("SampleGraph", formatC(length(list.files(subDir))+1, width=4, flag="0"), '.png')`
3. add few lines of code before `newplot` to save all produced images in .png format; e.g.
```R
png(paste0(myfilepath, '/', myfilename), 
     width = 1500, height = 1200, 
     units = "px", pointsize = 12)
```
5. `add dev.off()` at the end of the function
5. run short python2 script containing the following code (take care of dependencies!):
```python
import imageio
import os
with imageio.get_writer('./my.gif', mode='I') as writer:
    for filename in sorted(os.listdir("./images/")): # images == myfilepath == where .png images of interest are
        filename="./images/"+filename
        print(filename)
        image = imageio.imread(filename)
        writer.append_data(image)
```
Find more information at: https://rfunction.com/archives/812 and https://imageio.github.io/


# sub apps
* input <b>pre-processing</b>: &#x1F34E; https://github.com/NIB-SI/DiNAR/tree/master/subApps/pre-processing (&#x1F34F; https://nib-si.shinyapps.io/pre-processing/)
* network <b>clustering</b>: &#x1F34E; https://github.com/NIB-SI/DiNAR/tree/master/subApps/clustering (&#x1F34F; https://nib-si.shinyapps.io/clustering/)
* <b>shortestPaths</b>: :grapes: https://github.com/NIB-SI/DiNAR/tree/master/subApps/shortestPaths
* CustomNetwork from <b>GMM-KnetMiner-SKM</b> combo: &#x1F98B; https://github.com/NIB-SI/DiNAR/tree/master/subApps/GMM-SKM-KnetMiner (&#x1F984; https://nib-si.shinyapps.io/GMM-SKM-KnetMiner/)


# Ath GSE56094 experimental data analysis
&#x1F34E; https://github.com/NIB-SI/DiNAR/tree/master/GEODataAnalysis

# Cross-references
* &#x1F985; [Plant Data Visualization/Orthology Bundle - Cork Oak Use Case](https://corkoak-usecase.readthedocs.io/en/latest/)
* &#x1F98A; [FAIR Data-finder for Agronomic Research (FAIDARE) ](https://urgi.versailles.inra.fr/faidare/search?db=SKM&db=KnetMiner&db=AgroLD)

# obsolete
https://github.com/NIB-SI/DiNAR/tree/master/NetworkClustering



(*) [UnicodePlus](https://unicodeplus.com/)

