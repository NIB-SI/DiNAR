howPatient <- function() {
  
  # optional changes in:
  
  # maximal number of edges in cluster
  eThreshold = 2^13
  # maximal number of nodes in cluster
  nThreshold = 2^13
  
  
  tmplist = list(eThreshold, nThreshold)
  names(tmplist) = c('eThreshold', 'nThreshold')
  return(tmplist)
}
