#' extractMask
#' 
#' generate mask to apply to graphs
#' 
#' Author: Emanuele Pesce
library(igraph)
library(kernlab)
source("./graphUtils.R", chdir = T)


#' Apply a mask to a graph
#' @param graph graph 
#' @param mask containing which edges save in the graph. All other will be removed
#' @return g a graph where there are only edges of the mask    
#' @examples
#'  mask <- read.csv(file = "./../../data/toyData/results/1_maskUnion/edgesMask.csv")
#   mask <- as.matrix(mask)
#   g <- i_adjacencyFromFile("./../../data/toyData/controls/CTRL_amore.txt")
#   gm <- applyMask(g,mask)
applyMask <- function(graph, mask){
  
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
  for(i in 1:m_edges){
    v1 <- mask[i,1]
    v2 <- mask[i,2]
    w <- graph[v1, v2]
    if (is.finite(w)){
      if (w <= 0){
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

#' Add noise at edges with weight = 0
#' @param g graph
#' @return g a graph  
addNoise <- function(g){
  for(i in 1:length(V(g))){
    v <- V(g)[i]$name
    nrb <- neighbors(g, v)
    if(length(nrb) > 0){
      for (j in 1:length(nrb)){
        vt <- get.vertex.attribute(g, "name", nrb[j])
        w <- g[v, vt, attr = "weight"]
        if (is.finite(w)){
          if (w <= 0){
            g[v, vt, attr = "weight"] <- 1e-05
          }
        }
      }
    } 
    else{ # v has no neighbor
      for (j in 1:length(V(g))){
        vt <- get.vertex.attribute(g, "name", V(g)[j])
        g[v, vt, attr = "weight"] <- 1e-05
      }
    }
  }
  return(g)
}

#' Apply a mask to all graph in a directory and save the outputs in another 
#' directory
#' @param pathIn path where there are graphs which needs tobe applied a mask 
#' @param pathOut path where graph after mask application will be saved
#' @param path path of the mask
#' @examples
#'  applyMaskDirectory("./../../data/toyData/controls/", "./../../data/toyData/cutted_controls/")
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

if(interactive()){
  ptm <- proc.time()
  
#   mask <- read.csv(file = "./../../data/toyData/results/1_maskUnion/edgesMask.csv")
#   mask <- as.matrix(mask)
#    
#   g <- i_adjacencyFromFile("./../../data/toyData/controls/CTRL_amore.txt")
#   gm <- applyMask(g,mask)
#   applyMaskDirectory("./../../data/toyData/controls/a/", "./../../data/toyData/cutted_controls/")

  applyMaskDirectory("./../../data/toyData/controls/", "./../../data/toyData/cutted_controls/")
  applyMaskDirectory("./../../data/toyData/patients/", "./../../data/toyData/cutted_patients/")  
  
  time = proc.time() -ptm
  print (time)
}