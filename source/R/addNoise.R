rm(list=ls())
library(igraph)
source("./graphUtils.R", chdir = T)

addNoise <- function(graph){
  for(i in 1:ecount(graph)){
    if(E(graph)$weight[i] <= 0){
      E(graph)$weight[i] <- 1e-05
    }
  }
  return(graph)
}

applyNoiseDirectory <- function(pathIn, pathOut){

  files <- list.files(path = pathIn) #take all files in pathIn
  for(i in 1:length(files)){ #for each file
    # take path + name and apply the mask
    cfile <- paste(pathIn, files[i], sep="")
    if(grepl(cfile, pattern = "*.txt")){
      print(cfile)
      g <- i_adjacencyFromFile(cfile)
      g <- addNoise(g)
      # write the output
      outfile <- paste(pathOut, files[i], sep="")
      outfile <- gsub(".txt", ".gml", outfile)
      write.graph(g, outfile, format="gml")
    }
  }
}

if(interactive()){
  path1 <- "./../../data/toyData/controls/CTRL_amore.txt"
  g <- i_adjacencyFromFile(filename = path1)
  applyNoiseDirectory("./../../data/toyData/controls/","./../../data/toyData/controls/withNoise/")
  applyNoiseDirectory("./../../data/toyData/patients/","./../../data/toyData/patients/withNoise/")
  
}