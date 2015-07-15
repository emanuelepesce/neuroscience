rm(list=ls())
library(igraph)


addLabelsCoord <- function(graph, labels){
  graph <- set.graph.attribute(graph, "cx", "null")
  graph <- set.graph.attribute(graph, "cy", "null")
  graph <- set.graph.attribute(graph, "cz", "null")
  for(i in 1:vcount(graph)){
    graph <- set.vertex.attribute(graph, "cx", index=V(graph)[i], labels[i,1])
    graph <- set.vertex.attribute(graph, "cy", index=V(graph)[i], labels[i,2])
    graph <- set.vertex.attribute(graph, "cz", index=V(graph)[i], labels[i,3])
  }
  return(graph)
}

applyLabelCoord <- function(pathIn, pathOut, labels){
    files <- list.files(path = pathIn) #take all files in pathIn
  for(i in 1:length(files)){ #for each file
    # take path + name and apply the mask
    cfile <- paste(pathIn, files[i], sep="")
    if(grepl(cfile, pattern = "*.gml")){
      print(cfile)
      g <- read.graph(cfile, format="gml")
      gm <- addLabelsCoord(g, labels)
      # write the output
      outfile <- paste(pathOut, files[i], sep="")
      write.graph(gm, outfile, format="gml")
    }
  }
}


if(interactive()){
  path_names <- "./../../../data/regioni_coordinate/aalCOG.txt"
  m <- read.table(path_names, header=FALSE)
  labels <- as.matrix(m)
  
#   g <- read.graph("./../../../data/toyData/cutted_controls/CTRL_amore.gml", format="gml")
#   gl <- addLabelsCoord(g, labels)
  
#   
#   applyLabelCoord("./../../../data/toyData/cutted_controls/", "./../../../data/toyData/cutted_controls/", labels)
#   applyLabelCoord("./../../../data/toyData/cutted_patients/", "./../../../data/toyData/cutted_patients/", labels)
#

  ### t test
  pathIn <- "./../../../data/toyData/t_test_controls/"
  applyLabelCoord(pathIn, pathIn, labels)
  
  pathIn <- "./../../../data/toyData/t_test_patients/"
  applyLabelCoord(pathIn, pathIn, labels)
  
  ### t test MST
  pathIn <- "./../../../data/toyData/t_test_MST_controls/"
  applyLabelCoord(pathIn, pathIn, labels)
  
  pathIn <- "./../../../data/toyData/t_test_MST_patients/"
  applyLabelCoord(pathIn, pathIn, labels)


#test
#   g <- read.graph("./../../../data/toyData/t_test_MST_controls/CTRL_amore.gml", format="gml")
#   x <- V(g)$cx
#   y <- V(g)$cy
#   z <- V(g)$cz
#   coords <- cbind(x,y,z)
# #   V(g)$name <- V(g)$area
#   rglplot(g, layout=coords, vertex.label = V(g)$area, vertex.size = 10, vertex.color = "red", 
#           vertex.label.dist=0.5 )
}


