#' Performing a T-Test to remve noice
#' 
#' Author: Alessandro Merola

rm(list=ls())
library(TopKLists)
library(igraph)
library(miscTools)
source("./../graphUtils.R", chdir = T)


# Restituisce una lista di archi da rimuovere secondo il criterio del t-test
testOfRelevance <- function(pathIn = "./../../../data/toyData/controls/withNoise/", pathOut, verbose = TRUE) {
  
  k <- 1 # Conta gli archi selezionati
  l <- array()
  files <- list.files(path = pathIn) # Prendo la lista dei nomi dei file nella directory
  archiPresi <- matrix(nrow = 0, ncol = 2)
  kr <- 1
  
  ################### Recupero tutti gli archi, accedendo alle coppie di vertici #######################
  for(i in 1:vcount(g)) {
    vi <- V(g)$name[i]
    l_values <- array()
    inx <- 1
    for (j in 1:vcount(g)) {
      vj <- V(g)$name[j]
      
      # Check for verbose
      if (verbose) {
        print("Testing for egde")
        print(V(g)$name[i])
        print(V(g)$name[j])
      }
      
      
      if (vi != vj) {
        
        # Scorro la directory
        for(k in 1:length(files)) { # Per ogni file
          cfile <- paste(pathIn, files[k], sep="") # Recupero il nome completo, compreso di percorso
          if(grepl(cfile, pattern = "*.gml")) {
            g <- read.graph(cfile, format = "gml") # Recupero il grafo di questo file
            if(g[vi, vj] >= 0) {
              l_values[inx] <- g[vi, vj]
              inx <- inx + 1
            }
          }
        }
        
        # Performing the t-test on every single edge
        tt <- t.test(unlist(l_values), mu = 0)
        if(tt$p.value <= 0.05) {
          archiPresi <- insertRow(archiPresi, kr, c(vi,vj))
          
          write.table(archiPresi, file = pathOut, sep=",", col.names = F, row.names = F)
          kr <- kr + 1
        }
      }
    }
  }
  
  return(archiPresi)
}



# Restituisce una lista di archi da rimuovere secondo il criterio del t-test
testOfRelevancePlus <- function(pathIn = "./../../../data/toyData/controls/withNoise/", pathOut, verbose = TRUE) {
  
  k <- 1 # Conta gli archi selezionati
  l <- array()
  files <- list.files(path = pathIn) # Prendo la lista dei nomi dei file nella directory
  archiPresi <- matrix(nrow = 0, ncol = 2)
  kr <- 1
  
  #creo una lista di oggetti grafo
  listGraphs <- list()
  k <- 1
  for(i in 1:length(files)) { # Per ogni file
    cfile <- paste(pathIn, files[i], sep="") # Recupero il nome completo, compreso di percorso
    if(grepl(cfile, pattern = "*.gml")) {
      g <- read.graph(cfile, format = "gml") # Recupero il grafo di questo file
      listGraphs[[k]] <- g
      k <- k +1
    }
  }

  ################### Recupero tutti gli archi, accedendo alle coppie di vertici #######################
  for(i in 1:vcount(g)) {
    vi <- V(g)$name[i]
    l_values <- array()
    inx <- 1
    for (j in 1:vcount(g)) {
      vj <- V(g)$name[j]
      
      # Check for verbose
      if (verbose) {
        print("Testing for egde")
        print(V(g)$name[i])
        print(V(g)$name[j])
      }
      
      
      if (vi != vj) {
        
        # Scorro la directory
        for(k in 1:length(listGraphs)) { # Per ogni file
          g <- listGraphs[[k]]
          if(g[vi, vj] >= 0) {
            l_values[inx] <- g[vi, vj]
            inx <- inx + 1
          }          
        }
        
        # Performing the t-test on every single edge
        tt <- t.test(unlist(l_values), mu = 0)
        if(tt$p.value <= 0.01) {
          print(tt$p.value)
          archiPresi <- insertRow(archiPresi, kr, c(vi,vj))         
          kr <- kr + 1
        }
      }
    }
  }
  write.table(archiPresi, file = pathOut, sep=",", col.names = F, row.names = F)
  return(archiPresi)
}


# findEdges <- function(edge, listOfEdge, dimens) {
#   
#   for (i in 1:dimens) {
#     if(edge[1] == listOfEdge[i,][1] && edge[2] == listOfEdge[i,][2]) {
#       return (TRUE)
#     }
#   }
#   return (FALSE)
#   
# }
# 
# Applica la maschera al grafo
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


# Applica la maschera a tutti i grafi nella directory
applyMaskDirectory <- function(pathIn, pathOut, pathMask = "./../../data/toyData/results/1_maskUnion/edgesMask.csv"){
  
  # get the mask
  mask <- read.csv(file = pathMask)
  mask <- as.matrix(mask)
  
  files <- list.files(path = pathIn) # take all files in pathIn
  for(i in 1:length(files)){ # for each file
    # take path + name and apply the mask
    cfile <- paste(pathIn, files[i], sep="")
    if(grepl(cfile, pattern = "*.gml")){
      print(cfile)
      g <- read.graph(cfile, format="gml")
      gm <- applyMask(g,mask)
      # write the output
      outfile <- paste(pathOut, files[i], sep="")
#       outfile <- gsub(".txt", ".gml", outfile)
      write.graph(gm, outfile, format="gml")
    }
  }
  
}
# 
# 
# cutOfEdge <- function(graph, edgesToRemove) {
#   
#   lstEdges <- get.edgelist(graph)
#   numRow <- dim(edgesToRemove)[1]
#   for (i in 1:numRow) {
#     if(findEdges(lstEdges[i,], edgesToRemove, numRow) == TRUE) {
#       graph[lstEdges[i,][1], lstEdges[i,][2]] <- FALSE
#     }
#   }
#   return (graph)
#   
# }
# 
# 
# cuttingOfEdge <- function(readFrom, typeIn = "gml", writeTo, typeOut = "gml",  mask, verbose = FALSE) {
#   
#   archiPresi <- matrix(nrow = 0, ncol = 2)
#   archiPresi <- insertRow(archiPresi, 1, c("V1","V2"))
#   archiPresi <- insertRow(archiPresi, 2, c("V3","V4"))
#   lstFile <- list.files(path = readFrom) # lista dei file
#   for (i in 1:length(lstFile)) {
#     g <- read.graph(paste(readFrom, lstFile[i], sep=""), format = typeIn) # Recupero il grafo nel file corrente
#     cuttedGraph <- cutOfEdge(g, archiPresi)
#     write.graph(cuttedGraph,paste(writeTo, lstFile[i], sep=""), format = typeOut)
#   }
#   
# }


if(interactive()) {
  ptm <- proc.time()
  
  pathInC <- "./../../../data/toyData/controls/withNoise/"
  pathInP <- "./../../../data/toyData/patients/withNoise/"
  pathOutC <- "./../../../data/toyData/cutted_control_ttest/"
  pathOutP <- "./../../../data/toyData/cutted_patents_ttest/"
#   testOfRelevance (pathInC)
#   #test
#   testOfRelevance (pathInC, "./../../../data/toyData/controls/withNoise/t_test_mask/t_test_mask_controls111.csv")
  #controls
  testOfRelevancePlus(pathInC,
                   "./../../../data/toyData/controls/withNoise/t_test_mask/t_test_mask_controls.csv")
#   #patients
  testOfRelevancePlus(pathInP,
                   "./../../../data/toyData/controls/withNoise/t_test_mask/t_test_mask_patients.csv")
#   applyMaskDirectory(pathInC, pathOutC, pathMask = "./../../../data/toyData/controls/withNoise/t_test_mask/t_test_mask.csv")
  time = proc.time() - ptm
  print (time)
  
}