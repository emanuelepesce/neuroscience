#' exp_centralities
#' 
#' computes centraliyt of graphs
#' 
#' Author: Emanuele Pesce
library(igraph)
source("./../centrality.R", chdir = T)

applyCentralities <- function(pathIn, pathOut){
  
  colnames <- c("betweenneess", "pagerank", "indegree", "closeness" )
  files <- list.files(path = pathIn, pattern = "*.gml") #take all files in pathIn
  for(i in 1:length(files)){ #for each file
    # take path + name and apply the mask
    cfile <- paste(pathIn, files[i], sep="")
    g <- read.graph(cfile, format = "gml")
    cts <- centralities(g);
    df <- data.frame(cts$betw.v, cts$pr.v$vector, cts$in.v, cts$cl.v)
    colnames(df) <- colnames
    # write the output
    outfile <- paste(pathOut, files[i], sep="")
    outfile <- gsub(".txt", ".csv", outfile)
    write.csv(df, file = "outfile")
  }
}

if(interactive()){
  pathIn <- "./../../../data/toyData/cutted_controls/"
  pathOut <- "./../../../data/toyData/cutted_controls/centralities/"
  applyCentralities(pathIn, pathOut)  
}