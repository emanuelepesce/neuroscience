source("./../pruningEdges.R", chdir = T)
library(igraph)

if(interactive()){
  t = 0.05
  
  g <- i_adjacencyFromFile("./../../../data/toyData/extract/bordaMatrixControls.txt")
  c <- minFlowPruning(g, threshold = t)
  
  g <- i_adjacencyFromFile("./../../../data/toyData/extract/bordaMatrixPatients.txt")
  p <- minFlowPruning(g, threshold = t)
  
  write.graph(c$g_cut, "./../../../data/toyData/extract/pruning07_controls.gml", format = "gml")
  write.graph(p$g_cut, "./../../../data/toyData/extract/pruning07_patients.gml", format="gml")
}

