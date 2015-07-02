#' extractMask
#' 
#' generate mask to apply to graphs
#' 
#' Author: Emanuele Pesce
library(igraph)
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
  
  # add weights in the mask
  for(i in 1:m_edges){
    v1 <- mask[i,1]
    v2 <- mask[i,2]
    g[v1, v2, attr="weight"] <- graph[v1, v2]
  }
  
  return(g)
}


#' Apply a mask to all graph in a directory and save the outputs in another 
#' directory
#' @param pathIn path where there are graphs which needs tobe applied a mask 
#' @param pathOut path where graph after mask application will be saved
#' @param path path of the mask
#' @return g a graph where there are only edges of the mask    
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
    g <- i_adjacencyFromFile(cfile)
    gm <- applyMask(g,mask)
    # write the output
    outfile <- paste(pathOut, files[i], sep="")
    outfile <- gsub(".txt", ".gml", outfile)
    write.graph(gm, outfile, format="gml")
  }
}

if(interactive()){
  ptm <- proc.time()
  
#   mask <- read.csv(file = "./../../data/toyData/results/1_maskUnion/edgesMask.csv")
#   mask <- as.matrix(mask)
#   
#   g <- i_adjacencyFromFile("./../../data/toyData/controls/CTRL_amore.txt")
#   gm <- applyMask(g,mask)
  
#   applyMaskDirectory("./../../data/toyData/controls/", "./../../data/toyData/cutted_controls/")
  applyMaskDirectory("./../../data/toyData/patients/", "./../../data/toyData/cutted_patients/")  

  time = proc.time() -ptm
  print (time)
}