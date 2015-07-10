rm(list=ls())
library(igraph)
library(miscTools)
source("./../graphUtils.R", chdir = TRUE)

g_mstlist <- function(graph){
  
  ### inizialize list of edges
  mst_edges <- list()
  for (i in 1:vcount(graph)){
    mst_edges[[i]] <- i
  }

  ### put in mst_edges all edges which are part of a shortest path
  for (v in V(graph)){ #for each vertex v
    # all shortest paths from v
    sp <- get.all.shortest.paths(graph, from = v, to = V(graph), mode = "out", weights = E(graph)$weight)
    # updates
    for(i in 1:length(sp$res)){ # for each target (path v-i) vertex i
      if( length(sp$res[[i]])>1){ # if the list has more than an element
        for (j in 1:(length(sp$res[[i]])-1)){ # for each vertex j in path v-i
          if(!is.na(sp$res[[i]][j+1])){ # if j + 1 exist
            # edge vj-vk is to add to the list
            vj <- sp$res[[i]][j]
            vk <- sp$res[[i]][j+1]
            if (!( vk %in% mst_edges[[vj]] )){ # check if vk is the list of vj
              mst_edges[[vj]] <- c(mst_edges[[vj]], vk)
            }
          }
        } # end j
      } 
    } #end i
  } # end v
  
  k <- 0
  me <- matrix(nrow = 0, ncol = 2)
  for (i in 1:length(mst_edges)){
    vs <- i
    if(length(mst_edges[[i]])>1){
      for(j in 2:length(mst_edges[[i]])){
        vt <- mst_edges[[i]][j]
        k = k + 1
        me <- insertRow(me, k, c(vs, vt))
      }
    }
  }
  return(me)
}

intersect_list <- function(l1, l2){
  
  k <- 0
  mu <- matrix(nrow = 0, ncol = 2)
  for(i in 1:dim(l1)[1]){
    vs1 <- l1[i,1]
    vt1 <- l1[i,2]
    for(j in 1:dim(l2)[1]){
      vs2 <- l2[j,1]
      vt2 <- l2[j,2]
      if((vs1 == vs2) && (vt1 ==vt2)){
        k = k + 1
        mu <- insertRow(mu, k, c(vs1, vt1))
      }
    }
  }
  return(mu)
}

frequency_list <- function(l1){
  
  k <- 0
  mu <- matrix(nrow = 90, ncol = 90, data = 0)
  for(i in 1:dim(l1)[1]){
    vs1 <- l1[i,1]
    vt1 <- l1[i,2]
    print
    mu[vs1, vt1] = mu[vs1, vt1] +1
  }
  return(mu)
}

applyIntersect <- function(pathIn){
  
  l_intersect <- matrix(nrow = 0, ncol = 0)
  first = TRUE
  files <- list.files(path = pathIn) #take all files in pathIn
  for(i in 1:length(files)){ #for each file
    # take path + name and apply the mask
    cfile <- paste(pathIn, files[i], sep="")
    if(grepl(cfile, pattern = "*.gml")){
      print(cfile)
      if(first){
        g <- read.graph(cfile, format="gml")
        mst <- g_mstlist(g)
        l_intersect <- intersect_list(mst, mst)
        first = FALSE
      }
      else{
        print(length(l_intersect))
        g <- read.graph(cfile, format="gml")
        mst <- g_mstlist(g)
        l_intersect <- intersect_list(l_intersect, mst)
      }
      # write the output
#       outfile <- paste(pathOut, files[i], sep="")
#       outfile <- gsub(".txt", ".gml", outfile)
#       write.graph(gm, outfile, format="gml")
    }
  }
  return(l_intersect)
}

applyFrequency <- function(pathIn){
  
  mf <- matrix(nrow = 90, ncol = 90, data = 0) 
  first = TRUE
  files <- list.files(path = pathIn) #take all files in pathIn
  for(i in 1:length(files)){ #for each file
    # take path + name and apply the mask
    cfile <- paste(pathIn, files[i], sep="")
    if(grepl(cfile, pattern = "*.gml")){
      print(cfile)
      g <- read.graph(cfile, format="gml")
      mst <- g_mstlist(g)
      tmp <- frequency_list(mst)
      mf <- mf + tmp
      # write the output
      #       outfile <- paste(pathOut, files[i], sep="")
      #       outfile <- gsub(".txt", ".gml", outfile)
      #       write.graph(gm, outfile, format="gml")
    }
  }
  return(mf)
}

findFirstK <- function(M, k){
  nM <- matrix(nrow = 90, ncol =90, data = 0)
  for(i in 1:k){
    m <- which(M >= max(M), arr.ind = TRUE)
    r <- m[1,1]
    c <- m[1,2]
    M[r,c] <- 0
    nM[r,c] <- 1
  }
  return(nM)
}


if(interactive()){
  pathg <- "./../../../data/toyData/cutted_controls/CTRL_barbatoa.gml"
  pathg2 <- "./../../../data/toyData/cutted_controls/CTRL_amore.gml"
  g <- read.graph(pathg, format="gml")
  e_list <- g_mstlist(g)
  m <- frequency_list(e_list)
#   
#   pathg2 <- "./../../../data/toyData/cutted_controls/CTRL_amore.gml"
#   g2 <- read.graph(pathg2, format="gml")
#   
#   e_list2 <- g_mstlist(g2)
#   u <- intersect_list(e_list, e_list2)
#   applyIntersect("./../../../data/toyData/controls/withNoise/")
  mc <- applyFrequency("./../../../data/toyData/controls/withNoise/")
  mp <- applyFrequency("./../../../data/toyData/patients/withNoise/")
  mt <- mc + mp

  kc <- findFirstK(mc, 100)
  kp <-findFirstK(mp, 100)
  
  edgesLeft <- kc - kp
  edgesRight <- kp - kc
  common <- kc + kp

  #plot

  gr <- graph.adjacency(edgesRight, mode = "directed", weighted = TRUE);
  gr <- set.vertex.attribute(gr, name = "id",value = V(g)$id)
  rmv <- list()
  k <- 1
  gnew <- gr
  for(i in 1:ecount(gr)){
    if(E(gr)$weight[i] <= 0){
      rmv[k] <- E(gr)[i]
      k = k+1
    }
  }
  gr <- delete.edges(gr, rmv)
  
  x <- V(g)$cx
  y <- V(g)$cy
  z <- V(g)$cz
  coords <- cbind(x,y,z)
  #   V(g)$name <- V(g)$area
  rglplot(gr, layout=coords, vertex.label = V(g)$area, vertex.size = 10, vertex.color = "red", 
          vertex.label.dist=0.5 )

}
