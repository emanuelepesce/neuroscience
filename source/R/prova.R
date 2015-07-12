cutOfEdge <- function(graph, edgesToRemove) {

  lstEdges <- get.edgelist(graph)
  for (e in edgesToRemove) {
    if (lstEdges[i,][1] == e[1] & lstEdges[i,][2] == e[2]) {
      removeEdge(graph, lstEdges[i,][1], lstEdges[i,][2])
    }
  }

}



cuttingOfEdge <- function(readFrom, typeIn = "gml", writeTo, typeOut = "gml",  mask, verbose = FALSE) {

  lstFile <- list.files(path = readFrom) # lista dei file
  for (i in 1:restToCut) {
    g <- read.graph(paste(directory, files[i], sep=""), format = type) # Recupero il grafo nel file corrente
    cutOfEdge(g, c("V2","V2"))
  } 

}


if (interactive()) {
  cuttingOfEdge("./../../../data/toyData/controls/withNoise/", "gml")
}
