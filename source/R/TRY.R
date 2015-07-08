library(igraph)
# pathIn <- "./../../data/toyData/cutted_controls/CTRL_barbatog.gml"
# g <- read.graph(pathIn, format="gml")
# 
# pr <- page.rank(g, directed = TRUE)
# ind <- degree(g, mode = "in) 
# cl <- closeness(g)
# 
# x = pr$vector
# h<-hist(x, breaks=10, col="red", xlab="PageRank", 
#         main="Histogram with Normal Curve")
# xfit<-seq(min(x),max(x),length=40) 
# yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
# yfit <- yfit*diff(h$mids[1:2])*length(x) 
# lines(xfit, yfit, col="blue", lwd=2)
# lines(density(pr$vector))

addNoise <- function(g){
  for(i in 1:length(V(g))){
    v <- V(g)[i]$name
    nrb <- neighbors(g, v)
    if(length(nrb) > 0){
      for (j in 1:length(nrb)){
        vt <- get.vertex.attribute(g, "name", nrb[j])
        w <- g[v, vt, attr = "weight"]
        if (is.finite(w)){
          if (w <= 0){
            g[v, vt, attr = "weight"] <- 1e-05
          }
        }
      }
    } 
    else{ # v has no neighbor
      for (j in 1:length(V(g))){
        vt <- get.vertex.attribute(g, "name", V(g)[j])
        g[v, vt, attr = "weight"] <- 1e-05
      }
    }
  }
  return(g)
}



path1 <- "./../../data/toyData/cutted_controls/CTRL_barbatog.gml"
path2 <- "./../../data/toyData/cutted_controls/CTRL_barbatoa.gml"

g1 <- read.graph(path1, format="gml")
g2 <- read.graph(path2, format="gml")

average.path.length(g2, directed = TRUE)

g <- addNoise(g1)
for (i in 1:length(E(g))){
  if (E(g)[i]$weight == 0 ){
    print(i)
  }
}

for (i in 1:length(E(g1))){
  if (E(g1)[i]$weight == 0 ){
    print(i)
  }
}