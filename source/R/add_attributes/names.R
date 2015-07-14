rm(list=ls())
library(igraph)


addLabelsArea <- function(graph, labels){
  graph <- set.graph.attribute(graph, "area", "null")
  for(i in 1:vcount(graph)){
    graph <- set.vertex.attribute(graph, "area", index=V(graph)[i], labels[i])
  }
  return(graph)
}

applyLabelArea <- function(pathIn, pathOut, labels){
    files <- list.files(path = pathIn) #take all files in pathIn
  for(i in 1:length(files)){ #for each file
    # take path + name and apply the mask
    cfile <- paste(pathIn, files[i], sep="")
    if(grepl(cfile, pattern = "*.gml")){
      print(cfile)
      g <- read.graph(cfile, format="gml")
      gm <- addLabelsArea(g, labels)
      # write the output
      outfile <- paste(pathOut, files[i], sep="")
      write.graph(gm, outfile, format="gml")
    }
  }
}


if(interactive()){
  path_names <- "./../../../data/regioni_coordinate/aalLABELS.txt"
  m <- read.table(path_names, header=FALSE)
  labels <- as.matrix(m)
  
#   g <- read.graph("./../../../data/toyData/cutted_controls/CTRL_amore.gml", format="gml")
#   gl <- addLabelsArea(g, labels)
  
  applyLabelArea("./../../../data/toyData/cutted_controls/", "./../../../data/toyData/cutted_controls/", labels)
  applyLabelArea("./../../../data/toyData/cutted_patients/", "./../../../data/toyData/cutted_patients/", labels)
 
#test
  g <- read.graph("./../../../data/toyData/cutted_controls/CTRL_amore.gml", format="gml")
}


