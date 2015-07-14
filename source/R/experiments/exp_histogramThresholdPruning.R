#' esperiments for visualization which edges are cutted
#' 
#' Author: Emanuele Pesce
source("./../pruningEdges.R", chdir = T)
library(igraph)
library(plotrix)

ptm <- proc.time()

############## Controls
gC <- i_adjacencyFromFile("./../../../data/toyData/extract/bordaMatrixControls.txt")

limInf <- 0.04
limSup <- 0.08
step <- 0.01

sC <- seq(limInf, limSup, by=step)
listC <- list()

for(i in 1:length(sC)){
  listC[[i]] <- (minFlowPruning(gC, threshold = sC[i]))
}

############## Patients
gP <- i_adjacencyFromFile("./../../../data/toyData/extract/bordaMatrixPatients.txt")

sP <- seq(limInf, limSup, by=step)
listP <- list()

for(i in 1:length(sP)){
  listP[[i]] <- (minFlowPruning(gP, threshold = sP[i]))
}

############# Plotting
  X11 ()
  par(mfrow=c(1, 2))
  hist(x = E(gC)$weight, xlab = "Borda Score", main = "Controls, Threshold = None", ylim = c(0,700), 
       col="steelblue")
  hist(x = E(gP)$weight, xlab = "Borda Score", main = "Patients, Threshold = None", ylim = c(0,700), 
       col="red")
  dev.copy(jpeg,filename="./hist_edgesBordaScore_0.jpeg", width = 1000, height = 500);
  dev.off ();


for(i in 1:length(sC)){
  X11 ()
  par(mfrow=c(1, 2))
  hist(x = E(listC[[i]]$residualGraph)$weight, xlab = "Borda Score", main = paste("Controls, Threshold = ",sC[i]),
       ylim = c(0,700), col="steelblue")
  hist(x = E(listP[[i]]$residualGraph)$weight, xlab = "Borda Score",  main = paste("Patients, Threshold =",sC[i]), 
       ylim = c(0,700), col="red") 
  dev.copy(jpeg,filename=paste("./hist_edgesBordaScore_",i, ".jpeg",sep=""), width = 1000, height = 500);
  dev.off ();
}

graphics.off()


########### Calculate useful values
C_residual <- list()
C_paths <- list()

P_residual <- list()
P_paths <- list()

C_notpaths<- list()
P_notpaths <- list()

for(i in 1:length(sC)){
  C_residual[i] <-  unlist(listC[[i]]$n_residualEdges)
  C_paths[i] <- listC[[i]]$n_util
  
  P_residual[i] <-  listP[[i]]$n_residualEdges
  P_paths[i] <- listP[[i]]$n_util
  
  C_notpaths[i] <-  C_residual[[i]] - C_paths[[i]]
  P_notpaths[i] <- P_residual[[i]] - P_paths[[i]]
}
C_residual <- unlist(C_residual)
C_paths <- unlist(C_paths)

P_residual <- unlist(P_residual)
P_paths <- unlist(P_paths)

dataC <- cbind(C_residual, C_paths, C_notpaths)
dataP <- cbind(P_residual, P_paths, P_notpaths)

write.csv(dataC, file = "infoControls.csv")
write.csv(dataP, file = "infoPatients.csv")


time = proc.time() -ptm
print (time)