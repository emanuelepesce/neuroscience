
library(igraph)
source("./graphUtils.R")

# massimizzare il flusso normalizzando
#rimuovere il nodo stesso dalle liste
minFlowPruning <- function(graph, threshold=0.5){
  
  ### normalization and invert the values in order to calculate max flow with
  ### shortest path
  e_weights <- E(graph)$weight 
  ne_weights <- (e_weights-min(e_weights))/(max(e_weights)-min(e_weights))
  ne_weights <- 1-ne_weights 
  
  
  ### inizialize list of edges
  v_util <- list()
  for (i in 1:vcount(graph)){
    v_util[[i]] <- i
  }
  
  ### put in v_util all edges which are part of a shortest path
  for (v in V(graph)){ #for each vertex v
    # all shortest paths from v
    sp <- get.all.shortest.paths(graph, from = v, to = V(graph), mode = "out")
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
  
  for (i in 1:(vcount(graph)-1)){
    for (j in 1:vcount(graph)){
      # check if ij is in v_util
      if (!(j %in% v_util[[i]])){ # if i-j is not in util
        # number of neighbors of i and j
        ni <- length(v_util[[i]]) 
        nj <- length(v_util[[j]])
        f <- (ni+nj)/(2*vcount(graph)) # fraction of neighbors in util
        if (f > threshold){ # add i-j to the list of edges to remove
          if (!( i %in% toRemove[[j]] )){ 
            toRemove[[i]] <- c(toRemove[[i]],j)
          }
        }
      }
    } #j
  } #i
    
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
  
  # return
  toReturn <- list("v_util" = v_util, "toRemove" = toRemove, "g_cut" = g_cut)
  return(toReturn)
}


if(interactive()){
  
  g <- i_adjacencyFromFile("./../../data/toyData/controls/CTRL_amore.txt")
  R <- minFlowPruning(g, threshold = 0)
  gc <- R$g_cut
  util <- R$v_util
  r <- R$toRemove
  
  print(E(gc))
  
  n_util <- 0
  for (i in 1:length(util)){
    n_util <- n_util + length(util[[i]])
  }
  print(n_util)
  
  nr <- 0
  for (i in 1:length(util)){
    nr <- nr + length(r[[i]])
  }
  print(nr)
}