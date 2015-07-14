require(igraph)

# get graph
g <- read.graph("./SLA2_altezza.gml", format="gml")
size = vcount(g)
x <- V(g)$cx
y <- V(g)$cy
z <- V(g)$cz
coords <- cbind(x,y,z)

# get diffusers
centers <- betweenness(g)
diffusers <- which(centers == max(centers))
seeds_num = 1
infected =list()
infected[[1]]= diffusers

# for example, set percolation probability 
p = 0.128
coins = c(rep(1, p*1000), rep(0,(1-p)*1000))
n = length(coins)
sample(coins, 1, replace=TRUE, prob=rep(1/n, n))

# function for updating the diffusers
update_diffusers = function(diffusers){
  nearest_neighbors = neighborhood(g, 1, diffusers, "out")
  nearest_neighbors = data.frame(table(unlist(nearest_neighbors)))
  nearest_neighbors = subset(nearest_neighbors, !(nearest_neighbors[,1]%in%diffusers))
  # toss the coins
  toss = function(freq) {
    tossing = NULL
    for (i in 1:freq ) 
      tossing[i] = sample(coins, 1, replace=TRUE, prob=rep(1/n, times=n))
    tossing = sum(tossing)
    return (tossing)
  }
  keep = unlist(lapply(nearest_neighbors[,2], toss))
  new_infected = as.numeric(as.character(nearest_neighbors[,1][keep >= 1]))
  diffusers = unique(c(diffusers, new_infected))
  return(diffusers)
}

i = 1
while(length(infected[[i]]) < size){ 
  infected[[i+1]] = sort(update_diffusers(infected[[i]]))
  cat(length(infected[[i+1]]), "\n")
  i = i + 1
}

# "growth_curve"
num_cum = unlist(lapply(1:i, function(x) length(infected[x]) ))
p_cum = num_cum/max(num_cum)
time = 1:i

png(file = "./temporal_growth_curve.png", 
    width=5, height=5, 
    units="in", res=300)
plot(p_cum~time, type = "b")
dev.off()

E(g)$color = "blueviolet"
V(g)$color = "white"
set.seed(2014); 
# layout.old = layout.fruchterman.reingold(g)*100
V(g)$color[V(g)%in%diffusers] = "red"
plot.igraph(g, edge.arrow.mode = 0, layout=coords)

library(animation)

m = 1
while(m <= length(infected)){
  V(g)$color = "white"
  V(g)$color[V(g)%in%infected[[m]]] = "red"
  if(m<10){
    pathfile <- paste("./img2/00", m, ".png",sep="")
  }
  else if(m<100){
    pathfile <- paste("./img2/0", m, ".png",sep="")
  }
  else{
    pathfile <- paste("./img2/", m, ".png",sep="")
  }
#   png(file=pathfile)
  plot.igraph(g, edge.arrow.mode = 0, layout=coords, main = m)
#   plot(g, layout =layout.old, main= m)
  dev.copy(png,filename=pathfile);
  m = m + 1
  dev.off()
}
