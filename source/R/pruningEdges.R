#' pruningEdges
#' 
#' algorithm for pruning edges from a graph with shortest paths method
#' 
#' Author: Emanuele Pesce
library(igraph)
source("./graphUtils.R", chdir = T)

#' Prunes edges from a graph using min flow method.
#' The idea is: 
#' 1. Computes all shortest path among all vertices
#' 2. Put edges which are part of a shortest path in a set (util)
#' 3. For each edge not in util check if the fraction of its util neighbors
#'    edges is greater than threshold. If yes remove the edge (this means that 
#'    in this area there are a lot of util edges)  
#' 
#' @param graph a graph in format igraph
#' @param threshold a threshold
#' @param invert, if TRUE normalize all weights and do 1-weights in order to 
#'        turn the max value in min and so work with shortest paths.
#' @param flow if is equal to 0 means that algorithm works ignoring the flow else
#'        it consider the flow when threshold are computed for deciding if a 
#'        not util edge should be cutted or don't.
#' @return toReturn a list of things:
#'         -v_util : set of util vertices (shortest paths)
#'         -n_util: number of edges in v_util
#'         -residualGraph: graph after cutting (igraph format)
#'         -n_residualEdges: number of residual edges
#'         -toRemove : set of vertices to remove
#'         -n_cuttedEdges: number of cutted edges      
#' @examples
#' R <- minFlowPruning(g, threshold = 0.1)
#' gc <- R$v_util
minFlowPruning <- function(graph, threshold=0.5, invert = FALSE, flow=0){
  
  ### normalization and invert the values in order to calculate max flow with
  ### shortest path
  ### it only works if invert is equal to FALSE
  if(invert==TRUE){
    e_weights <- E(graph)$weight 
    ne_weights <- (e_weights-min(e_weights))/(max(e_weights)-min(e_weights))
    E(graph)$weight  <- 1 - ne_weights
  }
    
  ### inizialize list of edges
  v_util <- list()
  for (i in 1:vcount(graph)){
    v_util[[i]] <- i
  }
  

  
  ### put in v_util all edges which are part of a shortest path
  for (v in V(graph)){ #for each vertex v
    # all shortest paths from v
    sp <- get.all.shortest.paths(graph, from = v, to = V(graph), mode = "out", weights = E(graph)$weight)
    # updates utils
    for(i in 1:length(sp$res)){ # for each target (path v-i) vertex i
      if( length(sp$res[[i]])>1){ # if the list has more than an element
        for (j in 1:(length(sp$res[[i]])-1)){ # for each vertex j in path v-i
          if(!is.na(sp$res[[i]][j+1])){ # if j + 1 exist
            # edge vj-vk is to add to the list
            vj <- sp$res[[i]][j]
            vk <- sp$res[[i]][j+1]
              if (!( vk %in% v_util[[vj]] )){ # check if vk is the list of vj
                v_util[[vj]] <- c(v_util[[vj]], vk)
              }
          }
        } # end j
      } 
    } #end i
  } # end v
  
  ### calculates edges to remove
  toRemove <- list()
  for (i in 1:vcount(graph)){
    toRemove[[i]] <- i 
  }
  
  ### Algorithm with no flow
  if(flow == 0){
    for (i in 1:vcount(graph)){
      for (j in 1:vcount(graph)){
        # check if ij is in v_util
        if (!(j %in% v_util[[i]])){ # if i-j is not in util
          # number of neighbors of i and j
          ni <- length(v_util[[i]]-1) 
          nj <- length(v_util[[j]]-1)
          f <- (ni+nj)/(2*vcount(graph)) # fraction of neighbors in util
          if (f >= threshold){ # add i-j to the list of edges to remove
            if (!( j %in% toRemove[[i]] )){  # add vertex j only once
              toRemove[[i]] <- c(toRemove[[i]],j)
            }
          }
        }
      } #j
    } #i
  }
  ### Algorithm with flow
  else{
    for (i in 1:vcount(graph)){
      for (j in 1:vcount(graph)){
        # check if ij is in v_util
        if (!(j %in% v_util[[i]])){ # if i-j is not in util
          # number of neighbors of i and j
          #         ni <- length(v_util[[i]]-1) 
          #         nj <- length(v_util[[j]]-1)
          ni <- length(v_util[[i]]) 
          nj <- length(v_util[[j]])
          #get weights
          sum_weights_util = 0
          for(k in 1:ni){
            ids <- get.edge.ids(g, c(i,v_util[[i]][k]))
            sum_weights_util <- sum_weights_util + get.edge.attribute(g, ids, name = "weight")
          }
          for(k in 1:nj){
            ids <- get.edge.ids(g, c(j,v_util[[j]][k]))
            sum_weights_util <- sum_weights_util + get.edge.attribute(g, ids, name = "weight")
          }
          sum_weights_tot = 0
          inc <- incident(g, i, mode = "all")
          for(k in 1:length(inc)){
            ids <- inc[k]
            sum_weights_tot <- sum_weights_tot + get.edge.attribute(g, ids, name = "weight")
          }
          inc <- incident(g, i, mode = "all")
          for(k in 1:length(inc)){
            ids <- inc[k]
            sum_weights_tot <- sum_weights_tot + get.edge.attribute(g, ids, name = "weight")
          }
          f <- sum_weights_util / sum_weights_tot
          #         f <- (ni+nj)/(2*vcount(graph)) # fraction of neighbors in util
          if (f >= threshold){ # add i-j to the list of edges to remove
            if (!( j %in% toRemove[[i]] )){  # add vertex j only once
              toRemove[[i]] <- c(toRemove[[i]],j)
            }
          }
        }
      } #j
    } #i
  }
 
  ### remove edges
  g_cut <- graph
  for(i in 1:length(toRemove)){ # for each vertex i
    if (length(toRemove[[i]]) > 1){ # check if i has at least an edge to remove
      for(j in 1:length(toRemove[[i]])){ # for each vertex j to remove
        if(toRemove[[i]][j] != i){ # check if j is not i
          g_cut <- removeEdge(g_cut, i, toRemove[[i]][j]) # remove
        }
      } # end j
    }
    
  } # end i
  
  residualGraph <- g_cut
  
  ### number of cutted edges
  n_cuttedEdges <- length(E(graph)) - length(E(residualGraph))
  
  ### number of edges in v_util
  n_util <- 0
  for (i in 1:length(v_util)){
    n_util <- n_util + length(v_util[[i]])
  }
  n_util <- n_util - 90
  
  ### number of residual edges
  n_residualEdges <- length(E(residualGraph))

  # return
  toReturn <- list("v_util" = v_util, "n_util" = n_util, 
                   "residualGraph" = residualGraph, "n_residualEdges" = n_residualEdges,
                   "toRemove" = toRemove,  "n_cuttedEdges" = n_cuttedEdges)
  return(toReturn)
}


if(interactive()){
  ptm <- proc.time()
  g <- i_adjacencyFromFile("./../../data/toyData/extract/bordaMatrixControls.txt")
  g <- i_adjacencyFromFile("./../../data/toyData/extract/bordaMatrixPatients.txt")
#   g <- i_adjacencyFromFile("./../../data/toyData/controls/CTRL_amore.txt")
#   g <- i_adjacencyFromFile("./../../data/toyData/extract/bordaMatrix.txt")
  R <- minFlowPruning(g, threshold = 0.05, flow = 0)
  
  print("Number of edges before cutting")
  print(length(E(g)))
  
  print("Number of edges after cutting:")
  print(R$n_residualEdges)
  
  print("Number of cutted edges:")
  print(R$n_cuttedEdges)
  
  print("Number of utils edges:") 
  print(R$n_util)

  time  <- proc.time() - ptm
  print(time)
}