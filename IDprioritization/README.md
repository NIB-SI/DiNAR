Run RMarkdown script (e.g. myScript.Rmd) via the command line

Rscript -e "rmarkdown::render('myScript.Rmd')"

Run RMarkdown script (e.g. myScript.Rmd) with arguments via the command line

Rscript -e "rmarkdown::render('myScript.Rmd', params=list(args = myarg))"
