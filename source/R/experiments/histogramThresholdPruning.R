#' esperiments for visualization which edges are cutted
#' 
#' Author: Emanuele Pesce
source("./../pruningEdges.R", chdir = T)
library(igraph)

ptm <- proc.time()

############## Controls
g <- i_adjacencyFromFile("./../../../data/toyData/extract/bordaMatrixControls.txt")
R05 <- minFlowPruning(g, threshold = 0.05)
R06 <- minFlowPruning(g, threshold = 0.06)
R07 <- minFlowPruning(g, threshold = 0.07)
R08 <- minFlowPruning(g, threshold = 0.08)

print("Cutted edges")
print(R05$n_cut)
print(R06$n_cut)
print(R07$n_cut)
print(R08$n_cut)

hist(x = E(g)$weight, xlab = "bordaScore", main = "Control Threshold = None", ylim = c(0,700))
hist(x = E(R05$g_cut)$weight, xlab = "bordaScore", main = "Control Threshold = 0.05", ylim = c(0,700))
hist(x = E(R06$g_cut)$weight, xlab = "bordaScore", main = "Control Threshold = 0.06", ylim = c(0,700))
hist(x = E(R07$g_cut)$weight, xlab = "bordaScore", main = "Control Threshold = 0.07", ylim = c(0,700))
hist(x = E(R08$g_cut)$weight, xlab = "bordaScore", main = "Control Threshold = 0.08", ylim = c(0,700))



############## Patients
g <- i_adjacencyFromFile("./../../../data/toyData/extract/bordaMatrixPatients.txt")
R05 <- minFlowPruning(g, threshold = 0.05)
R06 <- minFlowPruning(g, threshold = 0.06)
R07 <- minFlowPruning(g, threshold = 0.07)
R08 <- minFlowPruning(g, threshold = 0.08)

print("Cutted edges")
print(R05$n_cut)
print(R06$n_cut)
print(R07$n_cut)
print(R08$n_cut)

hist(x = E(g)$weight, xlab = "bordaScore", main = "Patients Threshold = None", ylim = c(0,700))
hist(x = E(R05$g_cut)$weight, xlab = "bordaScore", main = "Patients Threshold = 0.05", ylim = c(0,700))
hist(x = E(R06$g_cut)$weight, xlab = "bordaScore", main = "Patients Threshold = 0.06", ylim = c(0,700))
hist(x = E(R07$g_cut)$weight, xlab = "bordaScore", main = "Patients Threshold = 0.07", ylim = c(0,700))
hist(x = E(R08$g_cut)$weight, xlab = "bordaScore", main = "Patients Threshold = 0.08", ylim = c(0,700))

proc.time() -ptm