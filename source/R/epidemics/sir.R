require(igraph)
library(animation)
# generate a social graph
node_number = 100
g = barabasi.game(node_number) ; plot(g)

seeds_num = 1
set.seed(2014); diffusers = sample(V(g),seeds_num) ; diffusers
infected =list()
infected[[1]]= diffusers

# for example, set percolation probability 
transmission_rate = 0.4
coins = c(1, 0) 
probabilities = c(transmission_rate, 1-transmission_rate )         
# sample(coins, 1, rep=TRUE, prob=probabilities) # Generate a sequence
# toss the coins
toss = function(freq) {
  tossing = NULL
  for (i in 1:freq ) tossing[i] = sample(coins, 1, rep=TRUE, prob=probabilities)
  tossing = sum(tossing)
  return (tossing)
}

update_diffusers = function(diffusers){
  nearest_neighbors = data.frame(table(unlist(neighborhood(g, 1, diffusers))))
  nearest_neighbors = subset(nearest_neighbors, !(nearest_neighbors[,1]%in%diffusers))
  keep = unlist(lapply(nearest_neighbors[,2], toss))
  new_infected = as.numeric(as.character(nearest_neighbors[,1][keep >= 1]))
  diffusers = unique(c(diffusers, new_infected))
  return(diffusers)
}

total_time = 1
while(length(infected[[total_time]]) < node_number){ 
  infected[[total_time+1]] = sort(update_diffusers(infected[[total_time]]))
  cat(length(infected[[total_time+1]]), "-->")
  total_time = total_time + 1
}


plot_time_series = function(infected, m){
  num_cum = unlist(lapply(1:m, 
                          function(x) length(infected[[x]]) ))
  p_cum = num_cum/node_number
  p = diff(c(0, p_cum))
  time = 1:m
  plot(p_cum~time, type = "b", 
       ylab = "CDF", xlab = "Time",
       xlim = c(0,total_time), ylim =c(0,1))
}

plot_time_series(infected, length(infected))

###animation
size=50
layout.old = layout.fruchterman.reingold(g) 

saveGIF({
  ani.options(interval = 0.5, convert = shQuote("C:/Program Files/ImageMagick-6.8.8-Q16/convert.exe"))
  # start the plot
  m = 1
  while(m <= length(infected)){
    # start the plot
    layout(matrix(c(1, 2, 1, 3), 2,2, byrow = TRUE), widths=c(3,1), heights=c(1, 1))
    V(g)$color <- "white"
    V(g)$color[V(g)%in%infected[[m]]] = "red"
    num_cum = unlist(lapply(1:m, function(x) length(infected[[x]]) ))
    p_cum = num_cum/size
    p = diff(c(0, p_cum))
    time = 1:m
    plot(g, layout =layout.old, edge.arrow.size=0.2)
    title(paste("Scale-free Network \n Day", m))
    plot(p_cum~time, type = "b", ylab = "CDF", xlab = "Time",
         xlim = c(0,i), ylim =c(0,1))
    plot(p~time, type = "h", ylab = "PDF", xlab = "Time",
         xlim = c(0,i), ylim =c(0,1), frame.plot = FALSE)
    m = m + 1}
}, ani.width = 800, ani.height = 500)