#' esperiment for determinating the threshold of minFlowPruning
#' 
#' Author: Emanuele Pesce
source("./../pruningEdges.R", chdir = T)

ptm <- proc.time()

##############
g <- i_adjacencyFromFile("./../../../data/toyData/extract/bordaMatrix.txt")
R <- minFlowPruning(g, threshold = 0.01)
nc <- R$n_cut

s <- seq(0.15, 0.01, by=-0.01)
res <- list()
for(i in 1:length(s)){
  R <- minFlowPruning(g, threshold = s[i])
  res[i] <- R$n_cut
}

time  <- proc.time() - ptm
print(time)

plot(y = res, x = s, type='b', main = 'screeplotPruningThreshold', xlab = 'threshold', 
     ylab = 'cutted edges')
axis(1, at=seq(0.01,0.15,0.1), label=F, tick=T)
axis(2, at=seq(0, 7500, 1000), label=F, tick=T)
grid()


##############

s <- seq(0.11, 0.04, by=-0.001)
res <- list()
for(i in 1:length(s)){
  R <- minFlowPruning(g, threshold = s[i])
  res[i] <- R$n_cut
}

time  <- proc.time() - ptm
print(time)

plot(y = res, x = s, type='b', main = 'screeplotPruningThreshold', xlab = 'threshold', 
     ylab = 'cutted edges')
axis(1, at=seq(0.03,0.12,0.005), label=F, tick=T)
axis(2, at=seq(0, 7500, 1000), label=F, tick=T)
grid()
