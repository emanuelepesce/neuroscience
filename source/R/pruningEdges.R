
library(igraph)
source("./graphUtils.R")

minFlowPruning <- function(graph){
  
  # inizialize list of edges
  v_util <- list()
  for (i in 1:vcount(graph)){
    v_util[[i]] <- i
  }
  
  # put in v_util all edges which are part of a shortest path
  for (v in V(graph)){ #for each vertex v
    # all shortest paths from v
    sp <- get.all.shortest.paths(graph, from = v, to = V(graph), mode = "out")
    # updates utils
    for(i in 1:length(sp$res)){ # for each target (path v-i) vertex i
      for (j in 1:(length(sp$res[[i]])-1)){ # for each vertex j in path v-i
        if (j!=0){ # if j = 0 the list has just one element
          if(!is.na(sp$res[[i]][j+1])){ # if j + 1 exist
            # edge vj-vk is to add to the list
            vj <- sp$res[[i]][j]
            vk <- sp$res[[i]][j+1]
              if (!( vk %in% v_util[[vj]] )){ # check if vk is the list of vj
                v_util[[vj]] <- c(v_util[[vj]], vk)
              }
          }
        }
        } # end j
    } #end i
  } # end v
  
  # return
  return(v_util)
}


if(interactive()){
  
  g <- i_adjacencyFromFile("./../../data/toyData/controls/CTRL_amore.txt")
  R <- minFlowPruning(g)
}