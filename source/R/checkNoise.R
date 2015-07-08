library(igraph)

applyNoise <- function(pathIn, pathOut = ""){

  files <- list.files(path = pathIn) #take all files in pathIn
  for(i in 1:length(files)){ #for each file
    # take path + name and apply the mask
    cfile <- paste(pathIn, files[i], sep="")
    if(grepl(cfile, pattern = "*.gml")){
      print(cfile)
      g <- read.graph(cfile, format="gml")
      g1 <- addNoise(g) # add noise to the adges with weight = 0
      for (i in 1:length(E(g1))){
        if (E(g1)[i]$weight <= 0 ){
          print(i)
        }
      }
      # write the output
#       outfile <- paste(pathOut, files[i], sep="")
#       write.graph(g, outfile, format="gml")
    }
  }
}

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

checkNoise <- function(pathIn, pathOut){
  
  files <- list.files(path = pathIn) #take all files in pathIn
  for(i in 1:length(files)){ #for each file
    # take path + name and apply the mask
    cfile <- paste(pathIn, files[i], sep="")
    if(grepl(cfile, pattern = "*.gml")){
      print(cfile)
      g <- read.graph(cfile, format="gml")
      for (i in 1:length(E(g))){
        if (E(g)[i]$weight <= 0 ){
          print(i)
        }
      }
    }
  }
}

checkNoise("./../../data/toyData/cutted_controls/")
# applyNoise("./../../data/toyData/cutted_controls/")