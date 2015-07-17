rm(list = ls())
library(igraph)
library(animation)

#Global variables
glob_var <- function(x,y){
  weak <<- x
  strong <<- y
}
up_var <- function(x,y){
  weak <<- weak + x
  strong <<- strong + y
}
p_var <- function(){
  print(weak)
  print(strong)
}

# function for updating the diffusers
update_diffusers = function(g, diffusers){
  nearest_neighbors = neighborhood(g, 1, diffusers, "out")
  nearest_neighbors = data.frame(table(unlist(nearest_neighbors)))
  nearest_neighbors = subset(nearest_neighbors, !(nearest_neighbors[,1]%in%diffusers))
  
  # infect vertices
  toss = function(v) {
    t_weak <- 0
    t_strong <- 0
    
    tossing = 0
    tot <- 0
    nrbs <- neighbors(g, v = v, mode = "in")
    for(i in 1:length(nrbs)){
      tot <- tot + g[nrbs[i], v]
    }
    pression <- 0
    for(i in 1:length(nrbs)){
      if(nrbs[i]%in%diffusers){
        pression <- pression + g[nrbs[i], v]
        if(g[nrbs[i], v, attr = "strong"]==1){
          t_strong <- t_strong + 1
        }
        else{
          t_weak <- t_weak + 1
        }
      }
    }
    tr_toInfect <- pression/tot
    if(runif(1) <= tr_toInfect){
      up_var(t_weak, t_strong)
      
      tossing = 1
    }
    return (tossing)
  }
  
  ve <- as.vector(nearest_neighbors[,1])
  ve <- strtoi(ve)
  keep = unlist(lapply(ve, toss))
  new_infected = as.numeric(as.character(nearest_neighbors[,1][keep >= 1]))
  diffusers = unique(c(diffusers, new_infected))
  return(diffusers)
}

# spreads epidemic on graph
epidemicsPression <- function(g, diffusers, pathOut = NULL){
  #inizialize
  infected =list()
  infected[[1]]= diffusers  
  glob_var(0,0)
  
  #spread
  i = 1
  while(length(infected[[i]]) < vcount(g)){ 
    infected[[i+1]] = sort(update_diffusers(g,infected[[i]]))
#     cat(length(infected[[i+1]]), "\n")
    i = i + 1
  }
  
  infected_round <- 0
  for(i in 1:length(infected)){
    infected_round[i+1] <- length(infected[[i]])
  }
  
  if(!is.null(pathOut)){
    syscall <- paste("rm ./", pathOut, "*png", sep="")
    system(syscall)
    
    # for rotating
    angle <- pi/2
    M <- matrix( c(cos(angle), -sin(angle), sin(angle), cos(angle)), 2, 2 )
    
    x <- V(g)$cx
    y <- V(g)$cy
    z <- V(g)$cz
    coords <- cbind(x,y,z)
    
    E(g)$color = "blueviolet"
    V(g)$color = "white"
    set.seed(2014); 
    V(g)$color[V(g)%in%diffusers] = "red"
#     plot.igraph(g, edge.arrow.mode = 0, layout=coords)
    
    m = 1
    while(m <= length(infected)){
      V(g)$color = "white"
      V(g)$color[V(g)%in%infected[[m]]] = "red"
      if(m<10){
        pathfile <- paste(pathOut, "00", m, ".png",sep="")
      }
      else if(m<100){
        pathfile <- paste(pathOut, "0", m, ".png",sep="")
      }
      else{
        pathfile <- paste(pathOut, m, ".png",sep="")
      }
      png(pathfile, width = 1000, height = 1000)
      par(mfrow=c(2,2))
      plot.igraph(g, edge.arrow.mode = 0, layout=coords[,-3])
      plot.igraph(g, edge.arrow.mode = 0, layout=coords[,-2])
      zy <- cbind(coords[,3],coords[,2])
      plot.igraph(g, edge.arrow.mode = 0, layout=zy%*%M, main = m)
      plot(infected_round[1:m], xlab = "Rounds", ylab="infected nodes",
           xlim = c(0,length(infected_round)), ylim =c(0,90), cex.axis=2, cex.lab = 2, cex = 2)
      mtext(paste("Round",m), side = 3, line = -2, outer=T,  cex=2)
      dev.off()
      m = m + 1

    }
    syscall <- paste("convert -delay 100 -loop 0 ", pathOut, "*.png ",pathOut,"animation.gif", sep="")
    system(syscall)   
  }
  

  res = list("infected" = infected, "infected_round" = infected_round, "strong"=strong,
             "weak" = weak) 
  return(res)
}


applyDiffusion <- function(pathIn, pathOut, diffusers="", times = 5){
  
  files <- list.files(path = pathIn) #take all files in pathIn
  toWrite <- NULL
  k <- 1
  for(i in 1:length(files)){ #for each file
    # take path + name and apply the mask
    cfile <- paste(pathIn, files[i], sep="")
    if(grepl(cfile, pattern = "*.gml")){
      print(cfile)
      # calculate average values
      l_time <- NULL
      l_strong <- NULL
      l_weak <- NULL
      g <- read.graph(cfile, format="gml")
      # get diffusers
      centers <- closeness(g, mode = "total")
      centers <- sort(centers, decreasing = T)
      diffusers <- c(match(centers[1], centers), match(centers[2], centers))
      for(j in 1:times){
        res <- epidemicsPression(g = g, diffusers = diffusers)
        l_time[j] <- length(res$infected_round) 
        l_strong[j] <- res$strong
        l_weak[j] <- res$weak
      }
      toWrite[[k]] <- c(files[i], mean(l_time), mean(l_strong), mean(l_weak))
      k <- k + 1
      # write the output
    }
  }
  toWrite <- unlist(toWrite)
  toWrite<- matrix(toWrite, ncol = 4, nrow = 2, byrow=TRUE)
  write.table(toWrite,file=pathOut,sep=",", col.names = F, row.names = F)
}



if(interactive()){
  sstime <- proc.time()
  
  ################################# EXAMPLE CONTROLS ####################################
#   g <- read.graph("./CTRL_amore.gml", format="gml")
#   
#   # get diffusers
#   centers <- betweenness(g)
#   centers <- closeness(g, mode = "total")
#   centers <- sort(centers, decreasing = T)
#   diffusers <- c(match(centers[1], centers), match(centers[2], centers))
#   
#   # epidemics
#   res <- epidemicsPression(g, diffusers, "./imgsC/")
#   print(res$infected_round)
#   print(length(res$infected_round))
#   print(res$strong)
#   print(res$weak)
# #   ################################# EXAMPLE PATIENTS ####################################
#   g <- read.graph("./SLA2_altezza.gml", format="gml")
#   
#   # get diffusers
#   centers <- betweenness(g)
#   centers <- closeness(g, mode = "total")
#   centers <- sort(centers, decreasing = T)
#   diffusers <- c(match(centers[1], centers), match(centers[2], centers))
#   
#   # epidemics
#   res <- epidemicsPression(g, diffusers, "./imgsP/")
#   print(res$infected_round)
#   print(length(res$infected_round))
#   print(res$strong)
#   print(res$weak)
  ################################# EXAMPLE ####################################
  

  ################################# EXAMPLE ####################################
  applyDiffusion(pathIn = "./", pathOut = "./result.csv")  

  ################################ Cutted ######################################
  pathIn <- "./../../../data/toyData/cutted_controls/"
  pathOut <- "./../../../data/toyData/results/5_Diffusion_closeness/cutted/diffusion_controls.csv"
  applyDiffusion(pathIn = pathIn, pathOut = pathOut) 
#   pathOut <- "./../../../data/toyData/results/5_Diffusion_closeness/cutted/diffusion_patients.csv"
#   applyDiffusion(pathIn = pathIn, pathOut = pathOut) 
  ################################ T test ######################################  
#   pathIn <- "./../../../data/toyData/t_test_controls/"  
#   pathOut <- "./../../../data/toyData/results/5_Diffusion_closeness/t_test_cutted/diffusion_controls.csv"
#   applyDiffusion(pathIn = pathIn, pathOut = pathOut) 
#   pathOut <- "./../../../data/toyData/results/5_Diffusion_closeness/t_test_cutted/diffusion_patients.csv/"
#   applyDiffusion(pathIn = pathIn, pathOut = pathOut) 
#   ################################ MST ######################################
#   pathIn <- "./../../../data/toyData/t_test_MST_controls/"    
#   pathOut <- "./../../../data/toyData/results/5_Diffusion_closeness/t_test_MST/diffusion_controls.csv/"
#   applyDiffusion(pathIn = pathIn, pathOut = pathOut) 
#   pathOut <- "./../../../data/toyData/results/5_Diffusion_closeness/t_test_MST/diffusion_patients.csv/"
#   applyDiffusion(pathIn = pathIn, pathOut = pathOut) 
  
  ttime <- sstime - proc.time()
  print(ttime)
}








