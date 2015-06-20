#' esperiment for determinating the threshold of minFlowPruning
#' 
#' Author: Emanuele Pesce
source("./../pruningEdges.R", chdir = T)

ptm <- proc.time()
# ###############CONTROLS
# ##############
# g <- i_adjacencyFromFile("./../../../data/toyData/extract/bordaMatrixControls.txt")
# R <- minFlowPruning(g, threshold = 0.01)
# nc <- R$n_cut
# 
# s <- seq(0.15, 0.01, by=-0.01)
# res <- list()
# for(i in 1:length(s)){
#   R <- minFlowPruning(g, threshold = s[i])
#   res[i] <- R$n_cut
# }
# 
# 
# 
# plot(y = res, x = s, type='b', main = 'Controls screeplotPruningThreshold', xlab = 'threshold', 
#      ylab = 'cutted edges')
# axis(1, at=seq(0.01,0.15,0.1), label=F, tick=T)
# axis(2, at=seq(0, 7500, 1000), label=F, tick=T)
# grid()
# 
# 
# ##############
# 
# s <- seq(0.11, 0.04, by=-0.001)
# res <- list()
# for(i in 1:length(s)){
#   R <- minFlowPruning(g, threshold = s[i])
#   res[i] <- R$n_cut
# }
# 
# time  <- proc.time() - ptm
# print(time)
# 
# plot(y = res, x = s, type='b', main = 'Controls screeplotPruningThreshold', xlab = 'threshold', 
#      ylab = 'cutted edges')
# axis(1, at=seq(0.03,0.12,0.005), label=F, tick=T)
# axis(2, at=seq(0, 7500, 1000), label=F, tick=T)
# grid()
# 
# ###############Patients
# ##############
# g <- i_adjacencyFromFile("./../../../data/toyData/extract/bordaMatrixPatients.txt")
# R <- minFlowPruning(g, threshold = 0.01)
# nc <- R$n_cut
# 
# s <- seq(0.15, 0.01, by=-0.01)
# res <- list()
# for(i in 1:length(s)){
#   R <- minFlowPruning(g, threshold = s[i])
#   res[i] <- R$n_cut
# }
# 
# 
# 
# plot(y = res, x = s, type='b', main = 'Patients screeplotPruningThreshold', xlab = 'threshold', 
#      ylab = 'cutted edges')
# axis(1, at=seq(0.01,0.15,0.1), label=F, tick=T)
# axis(2, at=seq(0, 7500, 1000), label=F, tick=T)
# grid()
# 
# 
# ##############
# 
# s <- seq(0.11, 0.04, by=-0.001)
# res <- list()
# for(i in 1:length(s)){
#   R <- minFlowPruning(g, threshold = s[i])
#   res[i] <- R$n_cut
# }
# 
# time  <- proc.time() - ptm
# print(time)
# 
# plot(y = res, x = s, type='b', main = 'Patients screeplotPruningThreshold', xlab = 'threshold', 
#      ylab = 'cutted edges')
# axis(1, at=seq(0.03,0.12,0.005), label=F, tick=T)
# axis(2, at=seq(0, 7500, 1000), label=F, tick=T)
# grid()




############## new
g <- i_adjacencyFromFile("./../../../data/toyData/extract/bordaMatrixControls.txt")

s <- seq(0.025, 0.006, by=-0.001)
res <- list()
for(i in 1:length(s)){
  R <- minFlowPruning(g, threshold = s[i])
  res[i] <- R$n_cut
}

plot(y = res, x = s, type='b', main = 'Controls screeplotPruningThreshold', xlab = 'threshold', 
     ylab = 'cutted edges')
axis(1, at=seq(0.005,0.025,0.001), label=F, tick=T)
axis(2, at=seq(0, 7500, 1000), label=F, tick=T)
grid()


g1 <- i_adjacencyFromFile("./../../../data/toyData/extract/bordaMatrixPatients.txt")

s1 <- seq(0.025, 0.01, by=-0.001)
res1 <- list()
for(i in 1:length(s1)){
  R1 <- minFlowPruning(g1, threshold = s1[i])
  res1[i] <- R1$n_cut
}

plot(y = res1, x = s1, type='b', main = 'Patients screeplotPruningThreshold', xlab = 'threshold', 
     ylab = 'cutted edges')
axis(1, at=seq(0.01,0.025,0.001), label=F, tick=T)
axis(2, at=seq(0, 7500, 1000), label=F, tick=T)
grid()

time  <- proc.time() - ptm
print(time)