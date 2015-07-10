#' smallworld
#' 
#' functions for checking if a graph is a smallworld
#' 
#' Author: Emanuele Pesce
library(igraph)

#' Calculates properties which a smallworld should possess
#' It uses igraph functions.
#' Properties: maxComponent, averagePath(currently not weighted), clustering coefficient
#' 
#' @param graph
#' @return A list of properties
#' @examples
#' p <- smallWorldProperties(g)
#' p$maxComponent$csize
smallWorldProperties <- function(graph){
  
  # max component
  maxComponent <- clusters(g, mode = "strong") # attributes: membership, csize, no 
  
  # average path length (this function doesn't consider weighted edges)
  averagePath <- average.path.length(g, directed=TRUE, unconnected=TRUE)
  
  # clustering coefficient
  clust_coeff <- transitivity(graph = g)
  
  
  # return
  sp <- list("maxComponent" = maxComponent, "averagePath" = averagePath,
             "clust_coeff" = clust_coeff);
  return(sp);  
}

#' Check if a graph is a small world
#' It uses igraph functions.
#' Currently it take into account only the average path lenght
#' 
#' @param graph
#' @return 1 if graph is a small world
#' @examples
#' isSmallWorld(g)
isSmallWorld <- function(graph){
  
  sw <- 1 # if graph is a smallworld
  
  sp <- smallWorldProperties(graph)
  
  # if the average of path lenght is approximately log(N), where N is the number of vertices
  if (sp$averagePath <= 1.5*(log10(vcount(graph)))){
    sw <- sw * 1
  }
  else{
    sw <- 0
  }
  # if the max component of graph includes at least N/2 vertices
  if (sp$maxComponent$csize > (vcount(graph)/2)){
    sw <- sw * 1
  }
  else{
    sw <- 0
  }
  
  
  # return
  return(sw)
}

if(interactive()){
  # path of the dataset
  path <- "./../../data/toyData/controls/CTRL_amore.txt";
  
  # read graph
  dat <- read.csv(path, header = FALSE, sep = " ");
  m <- as.matrix(dat);
  g <- graph.adjacency(m, mode = "directed", weighted = TRUE);
  
  sp <- smallWorldProperties(g)
  
  isSmallWorld(g)
  
  path <- "./../../data/toyData/cutted_controls/CTRL_amore.gml"
  g <- read.graph(path, format = "gml")
  sp <- smallWorldProperties(g)
  isSmallWorld(g)
}