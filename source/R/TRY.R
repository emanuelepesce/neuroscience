library(igraph)
pathIn <- "./../../data/toyData/cutted_controls/CTRL_barbatog.gml"
g <- read.graph(pathIn, format="gml")

pr <- page.rank(g, directed = TRUE)
ind <- degree(g, mode = "in") 
cl <- closeness(g)

x = pr$vector
h<-hist(x, breaks=10, col="red", xlab="PageRank", 
        main="Histogram with Normal Curve")
# xfit<-seq(min(x),max(x),length=40) 
# yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
# yfit <- yfit*diff(h$mids[1:2])*length(x) 
# lines(xfit, yfit, col="blue", lwd=2)
# lines(density(pr$vector))
