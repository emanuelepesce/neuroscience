rm(list=ls())
library(igraph)
source("./../pruningEdges.R", chdir = T)

maskMean <- function(pathInC, pathInP, pathOut, pathMask, verbose=1){
  
  # load mask
  mask <- read.table(file = pathMask, header=T, sep=",")
  mask <- as.matrix(mask)
  
  # load  graphs controls
  listGraphs <- list()
  k <- 0
  files <- list.files(path = pathInC)
  for(i in 1:length(files)) { # Per ogni file
    cfile <- paste(pathInC, files[i], sep="") 
    if(grepl(cfile, pattern = "*.gml")) {
      g <- read.graph(cfile, format = "gml")
      k <- k + 1
      listGraphs[[k]] <- g
    }
  }
  # load graphs patients
  files <- list.files(path = pathInP)
  for(i in 1:length(files)) { # Per ogni file
    cfile <- paste(pathInP, files[i], sep="") 
    if(grepl(cfile, pattern = "*.gml")) {
      g <- read.graph(cfile, format = "gml")
      k <- k + 1
      listGraphs[[k]] <- g
    }
  }
   
  # calculates weights of mask as the mean of the weights of the mask
  edges_mask <- NULL
  k <- 1
  for(i in 1:dim(mask)[1]){
    vs <- mask[i,1]
    vt <- mask[i,2]
    if(verbose > 0){
      print("edges")
      print(vs)
      print(vt)
    }
    edge_values <- NULL
    for(j in 1:length(listGraphs)){
      g <- listGraphs[[j]]
      if(g[vs, vt] > 0){
        edge_values <- c(edge_values, g[vs, vt])
      }
    }  
    if(length(edge_values) > 0 ){
      edges_mask[k] <- mean(edge_values)
      k <- k + 1
    }
  }
  res = list("mask" = mask, "edges_mean" = edges_mask)
  return(res)
}

createGraphMask <- function(g, mask_m){
  # inizialize
  e_list <- get.edgelist(g)
  g_edges <- ecount(g)
  m_edges <- dim(mask_m)[1]
  
  # remove all edges from g
  for(i in 1:g_edges){
    v1 <- e_list[i,1]
    v2 <- e_list[i,2]
    g[v1, v2] <- FALSE
  }
  
  # add weights of edges in the mask
  for(i in 1:m_edges) {
    v1 <- mask_m[i,1]
    v2 <- mask_m[i,2]
    w <- as.numeric(mask_m[i,3])
    if (is.finite(w)){
      if (w <= 0) {
        g[v1, v2, attr = "weight"] <- 1e-05
      }
      else{
        g[v1, v2, attr="weight"] <- w
      }
      g[v1, v2, attr="inverse"] <- 1 - w
    }
  }
  
  return(g)
}



if(interactive()){
  pathInC <- "./../../../data/toyData/controls/withNoise/"
  pathInP <- "./../../../data/toyData/patients/withNoise/"
  pathOutMask <- "./../../../data/toyData/results/2_t_test_mask/union_t_test_WEIGHTED.csv"
  pathOutMaskG <- "./../../../data/toyData/results/2_t_test_mask/union_t_test_WEIGHTED.gml"
  pathMaskU <- "./../../../data/toyData/results/2_t_test_mask/union_t_test.csv"
  
  # add average weights to the mask
  res <- maskMean(pathInC, pathInP, pathOutMask, pathMaskU)
  mask_m <- cbind(res$mask, res$edges_mean)
  write.table(mask_m, file = pathOutMask, col.names = F, row.names = F)
  
  
  # convert mask in graph object
  g_example <- read.graph("./../../../data/toyData/controls/withNoise/CTRL_amore.gml",
                          format="gml")
  gToCut <- createGraphMask(g_example, mask_m)
  write.graph(graph = gToCut, file = pathOutMaskG, format = "gml")
  
  # cut 0.05
  RC <- minFlowPruning(gToCut, threshold = 0.05, flow = 0)
  write.graph(graph = RC$residualGraph, file = "./../../../data/toyData/results/2_t_test_mask/mask_tt_test_cutted.gml"
                , format = "gml")
  write.csv(as.matrix(get.edgelist(RC$residualGraph)),
              file = "./../../../data/toyData/results/2_t_test_mask/mask_tt_test_cutted.csv" ,row.names = F)
  
  # cut only MST
  RM <- minFlowPruning(gToCut, threshold = 0, flow = 0)
  write.csv(graph = RC$residualGraph, file = "./../../../data/toyData/results/2_t_test_mask/mask_tt_test_MST.gml"
              , format = "gml")
  write.csv(as.matrix(get.edgelist(RM$residualGraph)),
                file = "./../../../data/toyData/results/2_t_test_mask/mask_tt_test_MST.csv" , row.names = F)
  
}
