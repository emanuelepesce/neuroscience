#' extractMask
#' 
#' generate mask to apply to graphs
#' 
#' Author: Emanuele Pesce
library(igraph)
source("./pruningEdges.R", chdir = T)

#' Generate the mask of the graph doing the union of two graph masks
#' The idea is: 
#' 1. Computes all shortest path among all vertices
#' 2. Put edges which are part of a shortest path in a set (util)
#' 3. For each edge not in util check if the fraction of its util neighbors
#'    edges is greater than threshold. If yes remove the edge (this means that 
#'    in this area there are a lot of util edges)  
#' 
#' @param gC graph of controls
#' @param gP graph of Patients
#' @return toReturn a matrix[m, 2], where m is the number of the edges after the
#'         union. So each row is an edge and columns stands for vertices of the 
#'         edges     
#' @examples
#' r = unionMask(residual_C, residual_P)
unionMask <- function(gC, gP){
  # get list of edges of controls and patients
  edgesC <- get.edgelist(gC)
  edgesP <- get.edgelist(gP)
  
  # cleaning: remove from each list edges with same nodes (i.e.: [V1, V1])
  indexToRmv <- c()
  for (i in 1:dim(edgesC)[1]){
    v1 = edgesC[i,1]
    v2 = edgesC[i,2]
    if (v1 == v2){
      indexToRmv <- c(indexToRmv, i)
    }
  }
  edgesC <- edgesC[-indexToRmv,]
  
  indexToRmv <- c()
  for (i in 1:dim(edgesP)[1]){
    v1 = edgesP[i,1]
    v2 = edgesP[i,2]
    if (v1 == v2){
      indexToRmv <- c(indexToRmv, i)
    }
  }
  edgesP <- edgesP[-indexToRmv,]
  
  # union: first find edges in patients which are not in controls and then merge
  #        in order to avoid repetitions
  indicesP = c()
  for (i in 1:dim(edgesP)[1]){
    v1 = edgesP[i,1]
    v2 = edgesP[i,2]
    find = 0
    for (j in 1:dim(edgesC)[1]){
      # if an edge of P is in C save it and after add it
      if((v1 == edgesC[j,1]) && (v2 == edgesC[j,2])){ 
        find = 1
      }
      if(find == 1) break
    }
    if(find == 0){ # element i in P is not in C
      indicesP <- c(indicesP, i)
    }
  }
  
  # take all edges in controls and indicesP
  # indicesP contains edges in patients which are not in controls
  n_row <- length(indicesP) + dim(edgesC)[1]
  m <- matrix(nrow = n_row, ncol = 2)
  for(i in 1:dim(edgesC)[1]){
    m[i, 1] <- edgesC[i, 1]
    m[i, 2] <- edgesC[i, 2]
  }
  j = 0
  for(i in (dim(edgesC)[1]+1):n_row){
    j = j + 1
    m[i, 1] <- edgesP[indicesP[j], 1]
    m[i, 2] <- edgesP[indicesP[j], 2]
  }
  
  return(m)
}

if(interactive()){
  # import graphs
  g_C <- i_adjacencyFromFile("./../../data/toyData/extract/bordaMatrixControls.txt")
  g_P <- i_adjacencyFromFile("./../../data/toyData/extract/bordaMatrixPatients.txt")
  
  # compute residuals
  RC <- minFlowPruning(g_C, threshold = 0.05, flow = 0)
  RP <- minFlowPruning(g_P, threshold = 0.05, flow = 0)
  residual_C <- RC$residualGraph
  residual_P <- RP$residualGraph
  
  r = unionMask(residual_C, residual_P)
  
  print("Edges in Controls")
  print(RC$n_residualEdges)
  print("Edges in Patients")
  print(RP$n_residualEdges)
  print("Edges in Union")
  print(dim(r)[1])
  print("Union - Controls")
  print(dim(r)[1] - RC$n_residualEdges)
  print("Union - Patients")
  print(dim(r)[1] - RP$n_residualEdges)
  
  out <- list("e_controls" = RC$n_residualEdges, "e_patients" = RP$n_residualEdges, 
              "e_union" =dim(r)[1], "e_union_m_controls" =  dim(r)[1] - RC$n_residualEdges,
              "e_union_m_patients" = dim(r)[1] - RP$n_residualEdges)
  write.csv(out, file = "info_maskUnion.csv")
  write.table(r,file="edgesMask.csv",sep="\t", col.names = F, row.names = F)
}