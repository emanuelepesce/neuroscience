#' centrality
#' 
#' centralities
#' 
#' computes cenrality measure of a graph
#' 
#' Author: Emanuele Pesce
library(igraph)

#' Computes several centralities measures.
#' It uses igraph functions.
#' Centralities(default on vertices): degree, closeness, betweenness(vertices and eges), eigenvector, ragerank
#' 
#' @param graph
#' @return A list of centralities
#' @examples
#' cts <- centralities(g)
#' print(cts$pr.v) # print the pagerank values of each node
centralities <- function(graph){
  
  #eigenvector
  suppressWarnings(ei.v <- evcent(graph, directed = TRUE))
  
  #indegree
  in.v <- degree(graph, mode = "in") 
  
  #closeness
  cl.v <- closeness(graph)
  
  #PageRank
  pr.v <- page.rank(graph, directed = TRUE)
  
  #betweenness
  betw.v <- betweenness(graph, v=V(graph), directed = TRUE);
  betw.e <- edge.betweenness(graph, e=E(graph), directed = TRUE);
  
  #return
  cts <- list("betw.v" = betw.v, "betw.e" = betw.e, "ei.v" = ei.v, "in.v" = in.v, "cl.v" = cl.v, "pr.v" = pr.v);
  return(cts);
}

if(interactive()){
  #path of the dataset
  path <- "./../../data/sampleNetworks/CTRL_amore.txt";
  
  #read graph
  dat <- read.csv(path, header = FALSE, sep = " ");
  m <- as.matrix(dat);
  g <- graph.adjacency(m, mode = "directed", weighted = TRUE);
  
  cts <- centralities(g);
}