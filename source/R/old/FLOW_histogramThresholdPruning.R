#' esperiments for visualization which edges are cutted
#' 
#' Author: Emanuele Pesce
source("./../pruningEdges.R", chdir = T)
library(igraph)

ptm <- proc.time()

############## Controls
g <- i_adjacencyFromFile("./../../../data/toyData/extract/bordaMatrixControls.txt")
CR05 <- minFlowPruning(g, threshold = 0.01, flow = 1)
CR06 <- minFlowPruning(g, threshold = 0.011, flow = 1)
CR07 <- minFlowPruning(g, threshold = 0.012, flow = 1)
CR08 <- minFlowPruning(g, threshold = 0.013, flow = 1)

print("Cutted edges")
print(CR05$n_cut)
print(CR06$n_cut)
print(CR07$n_cut)
print(CR08$n_cut)

hist(x = E(g)$weight, xlab = "bordaScore", main = "Control Threshold = None", ylim = c(0,700))
hist(x = E(CR05$g_cut)$weight, xlab = "bordaScore", main = "Control Threshold = 0.1", ylim = c(0,700))
hist(x = E(CR06$g_cut)$weight, xlab = "bordaScore", main = "Control Threshold = 0.011", ylim = c(0,700))
hist(x = E(CR07$g_cut)$weight, xlab = "bordaScore", main = "Control Threshold = 0.012", ylim = c(0,700))
hist(x = E(CR08$g_cut)$weight, xlab = "bordaScore", main = "Control Threshold = 0.013", ylim = c(0,700))



############## Patients
g1 <- i_adjacencyFromFile("./../../../data/toyData/extract/bordaMatrixPatients.txt")
PR05 <- minFlowPruning(g, threshold = 0.011, flow = 1)
PR06 <- minFlowPruning(g, threshold = 0.012, flow = 1)
PR07 <- minFlowPruning(g, threshold = 0.013, flow = 1)
PR08 <- minFlowPruning(g, threshold = 0.014, flow = 1)

print("Cutted edges")
print(PR05$n_cut)
print(PR06$n_cut)
print(PR07$n_cut)
print(PR08$n_cut)

hist(x = E(g1)$weight, xlab = "bordaScore", main = "Patients Threshold = None", ylim = c(0,700))
hist(x = E(PR05$g_cut)$weight, xlab = "bordaScore", main = "Patients Threshold = 0.01", ylim = c(0,700))
hist(x = E(PR06$g_cut)$weight, xlab = "bordaScore", main = "Patients Threshold = 0.011", ylim = c(0,700))
hist(x = E(PR07$g_cut)$weight, xlab = "bordaScore", main = "Patients Threshold = 0.012", ylim = c(0,700))
hist(x = E(PR08$g_cut)$weight, xlab = "bordaScore", main = "Patients Threshold = 0.013", ylim = c(0,700))

proc.time() -ptm