#' graphUtils
#' 
#' functions for working with graph
#' Function which names start with 'i' are wrappers of igraph library
#' 
#' Author: Emanuele Pesce
library(igraph)

#' Get the adjacency matrix from a file
#' @param filename name of the file containing the graph
#' @return m adjacency matrix of graph in filename
#' @examples
#' A  <-   graph <- getMatrixFromFile("./file.txt")
getMatrixFromFile <- function(filename){
  dat <- read.csv(filename, header = FALSE, sep = " ")
  m <- as.matrix(dat);
  
  #return
  return(m)
}

#' Get the edges specified in a list
#' @param graph graph
#' @param listNames list of the names of the edges to retrieve. Edges are
#' specified in the following format: row_col. row and col are both integers.
#' @return m list of edges specified in listNames
#' @examples
#' edges <- getEdgesAsVector(graph, c("1_2",3_4") for retriving edges [1,2] and 
#' [3,4]
getEdgesAsVector <- function(graph, listNames){
  
  listEdges <- list()
  for(i in 1:length(listNames)){
    s <- listNames[i]
    ss <- strsplit(s, "_")
    row <- strtoi(ss[[1]][1])
    col <- strtoi(ss[[1]][2])
    listEdges[i] <- graph[row, col]
  }
  return(listEdges)
}

#' Get the adjancency matrix of a file
#' @param filename name of the file containing the graph (csv with separator = " ")
#' @return g igraph adjacency matrix
#' @examples
#' g <- i_adjacencyFromFile("graph.csv")
i_adjacencyFromFile <- function(filename){
  dat <- read.csv(filename, header = FALSE, sep = " ");
  m <- as.matrix(dat);
  g <- graph.adjacency(m, mode = "directed", weighted = TRUE);
  
  # return
  return(g)
}


if(interactive()){
  graph <- getMatrixFromFile("./../../data/toyData/controls/CTRL_amore.txt")
  
  listNames <- c("76_74", "75_73", "36_68", "80_82", "41_37", "79_81", "35_67", "80_18", "17_29", "36_35")
  edges <- getEdgesAsVector(graph, listNames)
  
  g <- i_adjacencyFromFile("./../../data/toyData/controls/CTRL_amore.txt")
}


