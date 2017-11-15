howPatient <- function() {
  
  # optional changes in:
  
  # maximal number of edges in cluster
  eThreshold = 2^11
  # maximal number of nodes in cluster
  nThreshold = 2^10
  
  
  tmplist = list(eThreshold, nThreshold)
  names(tmplist) = c('eThreshold', 'nThreshold')
  return(tmplist)
}
