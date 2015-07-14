#' esperiment for determinating the threshold of minFlowPruning
#' 
#' Author: Emanuele Pesce
source("./../pruningEdges.R", chdir = T)
require(graphics)

if(interactive()){
  ptm <- proc.time()
  ############## Controls
  gC <- i_adjacencyFromFile("./../../../data/toyData/extract/bordaMatrixControls.txt")
  
  limInf <- 0.02
  limSup <- 0.14
  step <- 0.01
  
  sC <- seq(limInf, limSup, by=step)
  resC <- list()
  for(i in 1:length(sC)){
    RC <- minFlowPruning(gC, threshold = sC[i])
    resC[i] <- RC$n_residualEdges
  }
  

  
  ############## Patients
  gP <- i_adjacencyFromFile("./../../../data/toyData/extract/bordaMatrixPatients.txt")
    
  sP <- seq(limInf, limSup, by=step)
  resP <- list()
  for(i in 1:length(sP)){
    RP <- minFlowPruning(gP, threshold = sP[i])
    resP[i] <- RP$n_residualEdges
  }
  
  ############## Plotting
  X11()
  
  plot(y = resC, x = sC, type='b', main = 'Screeplot: threshold for cutting edges', xlab = 'Threshold', 
       ylab = 'Number of residual edges', col = "blue", pch=19)
  lines(y = resP, x = sP, type='b',  col = "red", pch = 19)
  axis(1, at=seq(limInf,limSup,step), label=T, tick=T)
  axis(2, at=seq(0, 7500, 1000), label=T, tick=T)
  abline(h=seq(0, 7500, 1000), v=seq(limInf,limSup,step), lty=3, col="lightgrey")
  legend('bottomright',  c("Controls","Patients"), pch = 19,  col=c("steelblue","red"))  
  
  dev.copy(jpeg,filename="./screeplotCuttingEdges.jpeg", width = 700, height = 500);
  dev.off ();
  
}

