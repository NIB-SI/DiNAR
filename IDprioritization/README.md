## git clone a subdirectory

`git clone --depth 1 https://github.com/NIB-SI/DiNAR.git .`

`git filter-branch --prune-empty --subdirectory-filter ./IDprioritization HEAD`

## Run RMarkdown script via the command line

`Rscript -e "rmarkdown::render('myScript.Rmd')"`

Run RMarkdown script (e.g. myScript.Rmd) with arguments via the command line

`Rscript -e "rmarkdown::render('myScript.Rmd', params=list(args = myarg))"`

## directory tree
* input
   * pis_ngs-genes :: ID translation table -- PIS node IDs to experimental IDs
   * NGS_DE :: folder containg explicitly comparison files with .txt extension
      * comparison 1 :: file containing experimental IDs, differential expression values and adjusted p-values
      * comparison 2 :: file containing experimental IDs, differential expression values and adjusted p-values
      * ...
      * comparison n :: file containing experimental IDs, differential expression values and adjusted p-values
* scripts
   * `pval.cutoff = 0.01` :: cut-off threshold for adjusted p-values, adjust variable value as prefered
   * `logFC.starts.from.col = 3` :: column number containing logFC values, adjust variable value according to the NGS_DE files
   * `organism.NGS = list.files("../input/NGS_DE/", pattern = ".txt")` :: adjust pattern if comparison files extension differes from .txt
* output

## decision tree for ID prioritisation
* select ID with max number of DE values througt all conditions; if mutiple IDs satisfy the criteria ->
   * select ID with highest mean of absolute DE values througt all conditions; if mutiple IDs satisfy the criteria ->
      * select ID with max value of absolute DE values througt all conditions; if mutiple IDs satisfy the criteria ->
         * select the first ID
