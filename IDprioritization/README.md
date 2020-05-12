## Run RMarkdown script (e.g. myScript.Rmd) via the command line

`Rscript -e "rmarkdown::render('myScript.Rmd')"`

Run RMarkdown script (e.g. myScript.Rmd) with arguments via the command line

`Rscript -e "rmarkdown::render('myScript.Rmd', params=list(args = myarg))"`

## tree
* input
   * pis_ngs-genes :: ID translation table -- PIS node IDs to experimental IDs
   * NGS_DE
      * comparison 1 :: file containing experimental IDs, differential expression values and adjusted p-values
      * comparison 2 :: file containing experimental IDs, differential expression values and adjusted p-values
      * ...
      * comparison n :: file containing experimental IDs, differential expression values and adjusted p-values
* scripts
* output
