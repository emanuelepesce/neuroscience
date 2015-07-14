require(igraph)
# generate a social graph
size = 50

# regular network
g = graph.tree(size, children = 2); plot(g)
g = graph.star(size); plot(g)
g = graph.full(size); plot(g)
g = graph.ring(size); plot(g)
g = connect.neighborhood(graph.ring(size), 2); plot(g) 

# random network
g = erdos.renyi.game(size, 0.1)

# small-world network
g = rewire.edges(erdos.renyi.game(size, 0.1), prob = 0.8 )
# scale-free network
g = barabasi.game(size) ; plot(g)

seeds_num = 1
set.seed(2014); diffusers = sample(V(g),seeds_num) ; diffusers
infected =list()
infected[[1]]= diffusers

# for example, set percolation probability 
p = 0.128
coins = c(rep(1, p*1000), rep(0,(1-p)*1000))
n = length(coins)
sample(coins, 1, replace=TRUE, prob=rep(1/n, n))

# function for updating the diffusers
update_diffusers = function(diffusers){
  nearest_neighbors = neighborhood(g, 1, diffusers)
  nearest_neighbors = data.frame(table(unlist(nearest_neighbors)))
  nearest_neighbors = subset(nearest_neighbors, !(nearest_neighbors[,1]%in%diffusers))
  # toss the coins
  toss = function(freq) {
    tossing = NULL
    for (i in 1:freq ) tossing[i] = sample(coins, 1, replace=TRUE, prob=rep(1/n, times=n))
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
set.seed(2014); layout.old = layout.fruchterman.reingold(g) 
V(g)$color[V(g)%in%diffusers] = "red"
plot(g, layout =layout.old)

library(animation)

m = 1
while(m <= length(infected)){
  V(g)$color = "white"
  V(g)$color[V(g)%in%infected[[m]]] = "red"
  if(m<10){
    pathfile <- paste("./imgs/00", m, ".png",sep="")
  }
  else if(m<100){
    pathfile <- paste("./imgs/0", m, ".png",sep="")
  }
  else{
    pathfile <- paste("./imgs/", m, ".png",sep="")
  }
#   png(file=pathfile)
  plot(g, layout =layout.old, main= m)
  dev.copy(png,filename=pathfile);
  m = m + 1
  dev.off()
}


# saveGIF({
#   ani.options(interval = 0.5, convert ='/opt/local/bin/convert')
#   # start the plot
#   m = 1
#   while(m <= length(infected)){
#     V(g)$color = "white"
#     V(g)$color[V(g)%in%infected[[m]]] = "red"
#     plot(g, layout =layout.old, main= m)
#     m = m + 1}
# })