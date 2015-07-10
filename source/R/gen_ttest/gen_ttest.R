#' Performing a T-Test to remve noice
#' 
#' Author: Alessandro Merola

rm(list=ls())
library(TopKLists)
library(igraph)
library(miscTools)

source("./../graphUtils.R", chdir = T)

#' Aggregates matrices
#' Get all matrix in 'path' and merge them all in a matrix in which each 
#' row is a sujbect (file) and each column is a value
#'  
#' @param path, path of matrices to aggregate
#' @return A list of vectors
#' @examples
#' M <-  m <- genMatrix("./../../data/toyData/controls/")
testOfRelevance <- function(pathIn = "./../../../data/toyData/controls/"){
  
  k <- 0
  l <- list()
  
  
  # archipresi[7006] ogni elemento è si se l'arco è preso
  #  for 1:7006
  # listarchi = list()
  # k = 1
  # for each graph 
  #   list[k] = E(g)[i]
  #   k = k+1
  #   ttest
  #   SI o NO?
  #   SI? archiPresi[i] = 1
  #   NO? archiPresi[i] = 0
  # salva
  # write the output
  #       outfile <- paste(pathOut, files[i], sep="")
  #       outfile <- gsub(".txt", ".gml", outfile)
  #       write.graph(gm, outfile, format="gml")
  
  
  files <- list.files(path = pathIn) # Prendo la lista dei nomi dei file nella directory
  
  archiPresi <- matrix(nrow = 0, ncol =2)
  kr <- 1
  # Recupero tutti gli archi, accedendo alle coppie di vertici
  for(i in 1:vcount(g)) {
    vi <- V(g)$name[i]
    l_values <- list()
    inx <- 1
    for (j in 1:vcount(g)) {
      vj <- V(g)$name[j]
      print("Testing for egde")
      print(V(g)$name[i])
      print(V(g)$name[j])
      if (vi != vj) {
        # Scorro la directory
        for(k in 1:length(files)) { # Per ogni file
          cfile <- paste(pathIn, files[k], sep="") # Recupero il nome completo, compreso di percorso
          if(grepl(cfile, pattern = "*.gml")) {
            g <- read.graph(cfile,format="gml")
            if(g[vi, vj] >= 0) {
              l_values[inx] <- g[vi, vj]
              inx <- inx +1
            }
          }
        }
        tt <- t.test(unlist(l_values),mu=0)
        if(tt$p.value<=0.05) {
          archiPresi <- insertRow(archiPresi, kr, c(vi,vj))
          kr <- kr+1
        }
      }
    }
  }

  return(archiPresi)
}

# return
# l[k] <- g[vi,vj]
if(interactive()){
  pathInC <- "./../../../data/toyData/controls/withNoise/"
  pathInP <- "./../../../data/toyData/patients/withNoise/"
  pathOutC <- "./../../../data/toyData/cutted_control_ttest/"
  pathOutP <- "./../../../data/toyData/cutted_patents_ttest/"
  lista <- testOfRelevance(pathInC)
}