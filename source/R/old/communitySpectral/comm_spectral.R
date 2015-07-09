library(kernlab)
library(igraph)

gm <- read.graph("./CTRL_lavoro.gml", format="gml")
ugm <- as.undirected(gm, "collapse")
MA <- get.adjacency(ugm, attr="weight")
M <- as.matrix(MA)
# mds
D <- as.dist(exp(-M))
fit <- cmdscale(D, 2)
plot(fit[,1], fit[,2])

#communities
path = "./communities/"
spc = specc(M, 4, kernel = "rbfdot")
spec_cl = spc@.Data

tab_spec = table(spec_cl)

for(i in 1:length(tab_spec)){
  D_index = which(spec_cl==i)
  yellow_g = subgraph(ugm, D_index)
  lfr = layout.fruchterman.reingold(graph = yellow_g,niter=1000,
                                    area=100 * vcount(yellow_g)^2)
  pdf(paste(path,"cluster",i,".pdf",sep=""))
  plot(yellow_g, layout = lfr)
  dev.off()
}