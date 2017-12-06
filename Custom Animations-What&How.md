* To create ndtvd3 .html animation from cluster with larger number of edges/nodes change

  # maximal number of edges in cluster
  eThreshold = 2^11
  # maximal number of nodes in cluster
  nThreshold = 2^10
  
in ndtvd3Threshold.R script



* To create .pdf animation, in animatedPlotAB.R uncomment

  # subDir <- "./plots"
  # dir.create(file.path(subDir), showWarnings = FALSE)
  # myfilename = paste0("SampleGraph", length(list.files(subDir))+1, '.pdf')
  # myfilepath = file.path(subDir)

and

  # dev.copy2pdf(file = paste0(myfilepath, '/', myfilename), width=24, height=16, out.type="pdf")
  
 then in CreatePDFanimation.tex adapt line

  \animategraphics[width=0.9\linewidth, controls,loop]{1500}{SampleGraph}{1}{131} % change last generated file number
