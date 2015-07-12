rm(list=ls())
library(igraph)
library(miscTools)
library(rgexf)
library(rgl)
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


saveResults <- function(kc, kp, g, path="./../../../data/toyData/controls/mst_first_100/"){
  edgesLeft <- kc - kp
  edgesRight <- kp - kc
  common <- kc + kp
  
  #left
  gl <- graph.adjacency(edgesLeft, mode = "directed", weighted = TRUE);
  gl <- set.vertex.attribute(gl, name = "area",value = V(g)$area)
  gl <- set.vertex.attribute(gl, name = "cx",value = V(g)$cx)
  gl <- set.vertex.attribute(gl, name = "cy",value = V(g)$cy)
  gl <- set.vertex.attribute(gl, name = "cz",value = V(g)$cz)
  gl <- set.vertex.attribute(gl, name = "id",value = V(g)$id)
  rmv <- list()
  k <- 1
  gnew <- gl
  for(i in 1:ecount(gl)){
    if(E(gl)$weight[i] <= 0){
      rmv[k] <- E(gl)[i]
      k = k+1
    }
  }
  gl <- delete.edges(gl, rmv)
  
  #right
  gr <- graph.adjacency(edgesLeft, mode = "directed", weighted = TRUE);
  gr <- set.vertex.attribute(gr, name = "area",value = V(g)$area)
  gr <- set.vertex.attribute(gr, name = "cx",value = V(g)$cx)
  gr <- set.vertex.attribute(gr, name = "cy",value = V(g)$cy)
  gr <- set.vertex.attribute(gr, name = "cz",value = V(g)$cz)
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
  
  #common
  gc <- graph.adjacency(edgesLeft, mode = "directed", weighted = TRUE);
  gc <- set.vertex.attribute(gc, name = "area",value = V(g)$area)
  gc <- set.vertex.attribute(gc, name = "cx",value = V(g)$cx)
  gc <- set.vertex.attribute(gc, name = "cy",value = V(g)$cy)
  gc <- set.vertex.attribute(gc, name = "cz",value = V(g)$cz)
  gc <- set.vertex.attribute(gc, name = "id",value = V(g)$id)
  rmv <- list()
  k <- 1
  gnew <- gc
  for(i in 1:ecount(gc)){
    if(E(gc)$weight[i] <= 0){
      rmv[k] <- E(gc)[i]
      k = k+1
    }
  }
  gc <- delete.edges(gc, rmv)
  
  el <- edgesLabels(gl)
  er <- edgesLabels(gr)
  ec <- edgesLabels(gc)
  
  write.csv(el, paste(path, "edge_controls.csv", sep=""))
  write.csv(er,paste(path, "edge_patients.csv", sep=""))
  write.csv(ec, paste(path, "edge_common.csv", sep=""))
  
  x <- V(g)$cx
  y <- V(g)$cy
  z <- V(g)$cz
  coords <- cbind(x,y,z)
  open3d()
  rglplot(gr, layout=coords, vertex.label = V(g)$area, vertex.size = 10, vertex.color = "green", 
          vertex.label.dist=0.5 )
  open3d()
  rglplot(gl, layout=coords, vertex.label = V(g)$area, vertex.size = 10, vertex.color = "red", 
          vertex.label.dist=0.5 )
  open3d()
  rglplot(gc, layout=coords, vertex.label = V(g)$area, vertex.size = 10, vertex.color = "yellow", 
          vertex.label.dist=0.5 )
  
}

edgesLabels <- function(g){
  m <- matrix(nrow = 0, ncol=2)
  k <- 1
  for(i in 1:ecount(g)){
    ed <-  get.edge(g, E(g)[i])
    vs <- get.vertex.attribute(graph = g, name = "area", index = ed[1])
    vt <- get.vertex.attribute(graph = g, name = "area", index = ed[2])
    m <- insertRow(m, k, v = c(vs, vt))
    k = k+1
  }
  return(m)
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
  
  saveResults(kc, kp, g)

  #plot
  
  gr <- graph.adjacency(edgesRight, mode = "directed", weighted = TRUE);
gr <- set.vertex.attribute(gr, name = "name",value = V(g)$name)
  gr <- set.vertex.attribute(gr, name = "area",value = V(g)$area)
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
  rgl.open()
  rgl.bg(sphere=TRUE, color = c("black", "white"),lit=FALSE, back="lines" )
  rglplot(gr, layout=coords, vertex.label = V(g)$area, vertex.size = 10, vertex.color = "red", 
          vertex.label.dist=0.5 )
}
