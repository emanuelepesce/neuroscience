#' smallword
#' 
#' functions for checking if a graph is a smallworld
#' 
#' Author: Emanuele Pesce
library(igraph)

#' Calculates properties which a smallworld should possess
#' It uses igraph functions.
#' Properties: maxComponent, averagePath(currently not weighted)
#' 
#' @param graph
#' @return A list of properties
#' @examples
#' p <- smallWorldProperties(g)
#' p$maxComponent$csize
smallWorldProperties <- function(graph){
  
  #max component
  maxComponent <- clusters(g, mode = "strong") # attributes: membership, csize, no 
  
  #average path length (this function doesn't consider weighted edges)
  averagePath <- average.path.length(g, directed=TRUE, unconnected=TRUE)
  
  #return
  cts <- list("maxComponent" = maxComponent, "averagePath" = averagePath);
  return(cts);
}

if(interactive()){
  #path of the dataset
  path <- "./../../data/sampleNetworks/CTRL_amore.txt";
  
  #read graph
  dat <- read.csv(path, header = FALSE, sep = " ");
  m <- as.matrix(dat);
  g <- graph.adjacency(m, mode = "directed", weighted = TRUE);
  
  sp <- smallWorldProperties(g)
}