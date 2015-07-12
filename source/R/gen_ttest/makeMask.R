#' makeMask 
#' 
#' generate and apply the mask obtained with t-test
#' 
#' Author: Alessandro Merola

rm(list=ls())
library(igraph)
library(TopKLists)
library(igraph)
library(miscTools)

source("./../graphUtils.R", chdir = T)

applyMask <- function(graph, mask) {

  # inizialize
  g <- graph
  e_list <- get.edgelist(g)
  n_edges <- dim(e_list)[1]
  m_edges <- dim(mask)[1]

  # remove all edges from g
  for(i in 1:n_edges){
    v1 <- e_list[i,1]
    v2 <- e_list[i,2]
    g[v1, v2] <- FALSE
  }

  # add weights of edges in the mask
  for(i in 1:m_edges) {
    v1 <- mask[i,1]
    v2 <- mask[i,2]
    w <- graph[v1, v2]
    if (is.finite(w)){
      if (w <= 0) {
        g[v1, v2, attr = "weight"] <- 1e-05
      }
      else{
        g[v1, v2, attr="weight"] <- w
      }
    }
    g[v1, v2, attr="inverse"] <- 1 - w
  }

  return(g)
}

applyMaskDirectory <- function(pathIn, pathOut, pathMask = "./../../data/toyData/results/1_maskUnion/edgesMask.csv"){
  # get the mask
  mask <- read.csv(file = pathMask)
  mask <- as.matrix(mask)

  files <- list.files(path = pathIn) #take all files in pathIn
  for(i in 1:length(files)){ #for each file
    # take path + name and apply the mask
    cfile <- paste(pathIn, files[i], sep="")
    if(grepl(cfile, pattern = "*.txt")){
      print(cfile)
      g <- i_adjacencyFromFile(cfile)
#       g <- addNoise(g) # add noise to the adges with weight = 0
      gm <- applyMask(g,mask)
      # write the output
      outfile <- paste(pathOut, files[i], sep="")
      outfile <- gsub(".txt", ".gml", outfile)
      write.graph(gm, outfile, format="gml")
    }
  }
}

