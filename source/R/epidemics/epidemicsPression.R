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
update_diffusers = function(diffusers){
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
    infected[[i+1]] = sort(update_diffusers(infected[[i]]))
    cat(length(infected[[i+1]]), "\n")
    i = i + 1
  }
  
  infected_round <- 0
  for(i in 1:length(infected)){
    infected_round[i+1] <- length(infected[[i]])
  }
  
  if(!is.null(pathOut)){
    syscall <- paste("rm ./", pathOut, "*png", sep="")
    system(syscall)
    
    x <- V(g)$cx
    y <- V(g)$cy
    z <- V(g)$cz
    coords <- cbind(x,y,z)
    
    E(g)$color = "blueviolet"
    V(g)$color = "white"
    set.seed(2014); 
    V(g)$color[V(g)%in%diffusers] = "red"
    plot.igraph(g, edge.arrow.mode = 0, layout=coords)
    
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
       plot.igraph(g, edge.arrow.mode = 0, layout=zy, main = m)
      plot(infected_round[1:m], xlab = "days", ylab="infected nodes",
           xlim = c(0,length(infected_round)), ylim =c(0,90), cex.axis=2, cex.lab = 2, cex = 2)
      mtext(paste("Day",m), side = 3, line = -2, outer=T,  cex=2)
#       dev.copy(png,pathfile)
      dev.off()
#       plot.igraph(g, edge.arrow.mode = 0, layout=coords[,-3], main = m)
#       dev.copy(png,filename=paste(pathfile,"xy", sep=""));
#       dev.off()
#       plot.igraph(g, edge.arrow.mode = 0, layout=coords[,-2], main = m)
#       dev.copy(png,filename=paste(pathfile,"xz", sep=""));
#                dev.off()
#       zy <- cbind(coords[,3],coords[,2])
#        plot.igraph(g, edge.arrow.mode = 0, layout=zy, main = m)
#       dev.copy(png,filename=paste(pathfile,"yz", sep=""));
#                 dev.off()
      m = m + 1

    }
    syscall <- paste("convert -delay 100 -loop 0 ", pathOut, "*.png ",pathOut,"animation.gif", sep="")
    system(syscall)   
#     syscall <- paste("convert -delay 100 -loop 0 ", pathOut, "*.pngxz ",pathOut,"xz_animation.gif", sep="")
#     system(syscall)   
#     syscall <- paste("convert -delay 100 -loop 0 ", pathOut, "*.pngyz ",pathOut,"yz_animation.gif", sep="")
#     system(syscall)   
         
    
#     plot(infected_round, xaxt='n', yaxt="n",xlab = "days", ylab="infected nodes")
#     axis(1, at = seq(1, length(infected_round), by = 1))
#     axis(2, at = seq(10, vcount(g), by = 10))
#     dev.copy(png,paste(pathOut, "grow.png", sep=""))
#     dev.off()
  }
  

  res = list("infected" = infected, "infected_round" = infected_round, "strong"=strong,
             "weak" = weak) 
  return(res)
}

if(interactive()){
  sstime <- proc.time()
  
  g <- read.graph("./CTRL_amore.gml", format="gml")
  
  # get diffusers
  centers <- betweenness(g)
  centers <- sort(centers, decreasing = T)
  diffusers <- c(match(centers[1], centers), match(centers[2], centers))
  
  # epidemics
  res <- epidemicsPression(g, diffusers, "imgs/")
  
  ttime <- sstime - proc.time()
  print(ttime)
}








