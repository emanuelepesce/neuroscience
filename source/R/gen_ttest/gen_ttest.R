#' Performing a T-Test to remve noice
#' 
#' Author: Alessandro Merola

rm(list=ls())
library(TopKLists)
library(igraph)
library(miscTools)
source("./../graphUtils.R", chdir = T)


# Restituisce una lista di archi da rimuovere secondo il criterio del t-test
testOfRelevancePlus <- function(pathIn = "./../../../data/toyData/controls/withNoise/", pathOut, verbose = TRUE) {
  
  l_pvalues <- list() # p values list
  kp <- 1 # counter for l_pvalues
  k <- 1 # counter for edges
  files <- list.files(path = pathIn) # Prendo la lista dei nomi dei file nella directory
  archiPresi <- matrix(nrow = 0, ncol = 2)
  kr <- 1
  
  #creo una lista di oggetti grafo
  listGraphs <- list()
  for(i in 1:length(files)) { # Per ogni file
    cfile <- paste(pathIn, files[i], sep="") # Recupero il nome completo, compreso di percorso
    if(grepl(cfile, pattern = "*.gml")) {
      g <- read.graph(cfile, format = "gml") # Recupero il grafo di questo file
      listGraphs[[k]] <- g
      k <- k +1
    }
  }
  
  ################### generates all pairs of vertices (edges) #######################
  for(i in 1:vcount(g)) {
    vi <- V(g)$name[i]
    for (j in 1:vcount(g)) {
      vj <- V(g)$name[j]
      l_values <- list() #inizialize list values of the edge vi-vj
      
      # Check for verbose
      if (verbose) {
        print("Testing for egde")
        print(V(g)$name[i])
        print(V(g)$name[j])
      }

      if (vi != vj) { # if the pair is valid
        
        # Go through the graphs
        inx <- 1
        for(k in 1:length(listGraphs)) { # For each graph take the edge value (vi-vj)
          g <- listGraphs[[k]]
          if(g[vi, vj] >= 0) {
            l_values[inx] <- g[vi, vj]
            inx <- inx + 1
          }          
        }
        
        # Performing the t-test on every single edge
        tt <- t.test(unlist(l_values), mu = 0)
        l_pvalues[kp] <- tt$p.value
        archiPresi <- insertRow(archiPresi, kp, c(vi,vj))
        kp <- kp + 1
      }
    }
  }
  # bonferroni correction
  adj <- p.adjust(unlist(l_pvalues), "bonferroni")
  
  toSelect <- which(adj <= 0.05, arr.ind = T)
  selected <- archiPresi[toSelect,]
  write.table(selected, file = pathOut, sep=",", col.names = F, row.names = F)
  
  res <- list("adj" = adj, "p_values" = l_pvalues, "edgesList" = archiPresi);
  return(res)
}

# Compute the mask's union
makeUnion <- function(ctrl, pznt) {
  
  unionList <- matrix(nrow = 0, ncol = 2)
  k <- 1
  for (i in 1:dim(ctrl)[1]) {
    vs <- ctrl[i,1]
    vt <- ctrl[i,2]
    unionList <- insertRow(unionList, k, c(vs, vt))
    k <- k + 1
  }
  
  for (i in 1:dim(pznt)[1]) {
    vs_p <- pznt[i,1]
    vt_p <- pznt[i,2]
    catched <- 0
    for (i in 1:dim(ctrl)[1]) {
      vs_c <- ctrl[i,1]
      vt_c <- ctrl[i,2]
      if((vs_p==vs_c) && (vt_p==vt_c)){
        catched <- 1
        break
      }
    }
    if(catched == 0){
      unionList <- insertRow(unionList, k, c(vs, vt))
      k <- k + 1
    }
  }
  return (unionList)
  
}

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
      write.graph(gm, outfile, format="gml")
    }
  }
  
}

if(interactive()) {
  ptm <- proc.time()
  
  pathInC <- "./../../../data/toyData/controls/withNoise/"
  pathInP <- "./../../../data/toyData/patients/withNoise/"
  pathOutC <- "./../../../data/toyData/cutted_control_ttest/"
  pathOutP <- "./../../../data/toyData/cutted_patents_ttest/"
  #   testOfRelevance (pathInC)
  #   #test
#   r <-  testOfRelevancePlus(pathInC, "./../../../data/toyData/results/2_t_test_mask/t_test_mask_controls_prova.csv")
  #controls
#   res <- testOfRelevancePlus(pathInC, "./../../../data/toyData/results/2_t_test_mask/t_test_mask_controls.csv")
  #   #patients
#   res <- applyMaskDirectory(pathInC, pathOutC, pathMask = "./../../../data/toyData/controls/withNoise/t_test_mask/t_test_mask.csv")
  #   r <-  testOfRelevancePlus(pathInC, "./../../../data/toyData/results/2_t_test_mask/t_test_mask_controls_prova.csv")
  #controls
  #   res <- testOfRelevancePlus(pathInC, "./../../../data/toyData/results/2_t_test_mask/t_test_mask_controls.csv")
  #   #patients
  #res <- testOfRelevancePlus(pathInP, "./../../../data/toyData/results/2_t_test_mask/t_test_mask_patients.csv")
  
  control_mask = read.csv(file = "./../../../data/toyData/results/2_t_test_mask/t_test_mask_controls.csv", sep = ",", header = TRUE)
  patients_mask = read.csv(file = "./../../../data/toyData/results/2_t_test_mask/t_test_mask_patients.csv", sep = ",")
  unione <- makeUnion(as.matrix(control_mask), as.matrix(patients_mask))
  write.table(unione, file = "./../../../data/toyData/results/2_t_test_mask/union_t_test.csv", sep=",", col.names = F, row.names = F)
  #write.csv(unione, file = "./../../../data/toyData/results/2_t_test_mask/union_t_test.csv")
  #   res <- applyMaskDirectory(pathInC, pathOutC, pathMask = "./../../../data/toyData/controls/withNoise/t_test_mask/t_test_mask.csv")
  time = proc.time() - ptm
  print (time)
}