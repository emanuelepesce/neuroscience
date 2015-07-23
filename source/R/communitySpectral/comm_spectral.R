#' Execution and testing of spectral clust method for community detection problem
#' The test is performed on
#' - Bonferroni corrected t-test-cutted graph
#' - Shortest path + Bonferroni corrected t-test-cutted graph
#' - Shortest path + MST cutted graph
#' To evaluate the result is used a hitmap
#'
#' Author: Alessandro Merola

rm(list=ls())

library(infotheo)
library(kernlab)
library(igraph)
library(gplots)
library(ade4)

trySpec <- function(M) {
  out <- tryCatch({ 
    spc <- specc(M, 10, kernel = "rbfdot")
  },
  error=function(cond){
    return(NULL)
  },
  warning=function(cond) {
  },
  finally={    
  }
  )
  return(out)
}


# Performs the specified community detection method saving result on a specified file
# @param pathIn is the input path
# @param pathOut is the output path
# @algorithm specifies the community detection algorithm
# @verbose specifies if verbose execution is desired
performingCommunityDetection <- function(pathIn = "./../../../data/toyData/cutted_controls/", pathOut = "./../../../../data/toyData/cutted_controls/spectral", algorithm = "specc", verbose = FALSE) {
  
  pathForCommunity = pathOut
  listNames <- "names"
  k <- 0
  files <- list.files(path = pathIn) # take all files in pathIn
  if(algorithm == "specc") {
    for(i in 1:length(files)) {
      cfile <- paste(pathIn, files[i], sep="")
      if(grepl(cfile, pattern = "*.gml")) {
        print(cfile)
        print(k)
        k <- k + 1
        
        if (verbose == TRUE) {
          print ("Processing")
          print (cfile)
        }
        
        # get the graph from the file
        g <- read.graph(cfile, format="gml")
        ugm <- as.undirected(g, "collapse")
        MA <- get.adjacency(ugm, attr="weight")
        M <- as.matrix(MA)
        listNames <- c(listNames, files[i])
        
        # execute spectral clustering method
        repeat{
          spc <- trySpec(M)
          if(!is.null(spc)){
            break
          } 
        }
        spec_cl = spc@.Data
        names <- V(g)$name
        if(k <= 1) {
          outp <- cbind(names, spec_cl)
        }
        else {
          outp <- cbind(outp, spec_cl)
        }
      }
    }
  }
  colnames(outp) <- listNames
  write.csv(outp[,-1], pathForCommunity, row.names = outp[,1])
  
}


# Counts how many times each region is in the same community for each subject
# @param pathIn is the input path
# @return a sets of elements (a matrix) A(i,j) to indicate how many time the regions i and j are in the same community
coOccurrence <- function(pathIn = "./../../../data/toyData/cutted_patients/spectral/membership_patients.csv") {
  data <- read.table(pathIn, sep=",", header=TRUE)
  data <- data[,-1]
  
  m <- as.matrix(data) # a matrix to return
  numRow <- dim(m)[1]
  k <- 0
  occurrenceMatrix <- matrix(nrow = 90, ncol = 90, data = 0)
  
  for (i in 1:numRow) {
    for (j in 1:numRow) {
      r1 <- m[i,]
      r2 <- m[j,]
      for (z in 1:length(m[i,])) { # check for each subject
        if(m[i,][z] == m[j,][z]) {
          occurrenceMatrix[i,j] <- occurrenceMatrix[i,j] + 1
        }
      }
    }
  }
  
  maximum <- occurrenceMatrix[1,][1]
  normalizedMatrix <- occurrenceMatrix/maximum
  return(normalizedMatrix)
}


# Compute the heatmap for m1 and m2 cooccurrence matrix
# @pathOut is the output file to save
makeHeatmap <- function(m1, m2, pathOut) {
  
  # heatmap
  
  
  jpeg(filename=paste(pathOut, "heat_controls.jpeg", sep=""), width = 1000, height = 1000)
  h1 <- heatmap.2(t(m1), tracecol = F, main = "Community Controls")
  dev.off()
  
  
  jpeg(filename=paste(pathOut, "heat_patients.jpeg", sep=""), width = 1000, height = 1000)
  h2 <- heatmap.2(t(m1), tracecol = F, main = "Community patients")
  dev.off()
  
  
  sum_occ <- m1 + m2
  jpeg(filename=paste(pathOut, "heat_sum.jpeg", sep=""), width = 1000, height = 1000)
  sumH <- heatmap.2(t(sum_occ), tracecol = F, main = "Community Sum")
  dev.off()
  
  res <- list("h1" = h1, "h2" = h2, "sum" = sumH)
  return(res)
  
}


plotF <- function(m, k = 10, pathIn, pathOut, verbose = FALSE) {
  
  # get coordinates
  files <- list.files(path = pathIn) # take all files in pathIn
  valid <- grepl(files, pattern = "*.gml")
  valid <- which(valid == TRUE)
  cfile <- files[valid[1]]
  g <- read.graph(paste(pathIn, cfile, sep=""), format = "gml") # get the graph from the file
  x <- V(g)$cx
  y <- V(g)$cy
  z <- V(g)$cz
  coords <- cbind(x,y,z)
  # for rotating
  angle <- pi/2
  M <- matrix( c(cos(angle), -sin(angle), sin(angle), cos(angle)), 2, 2 )
  
  # clustering
  dm <- dist(m)
  hc <- hclust(dm)
  hcc <- cutree(hc, k = 10)
  hcc <- as.vector(hcc)
  
  jpeg(filename = pathOut, width = 1000, height = 1000)
  plot(g, vertex.color = hcc, vertex.size = 10, edge.arrow.mode = 0, layout = coords[,-3])
  dev.off()
  
  pathOut <- sub("*graphControls*", "graphControls3d", pathOut)
  pathOut <- sub("*graphPatients*", "graphPatients3d", pathOut)
  jpeg(pathOut, width = 1000, height = 1000)
  par(mfrow=c(2,2))
  plot.igraph(g, vertex.color = hcc, edge.arrow.mode = 0, layout=coords[,-3])
  plot.igraph(g, vertex.color = hcc, edge.arrow.mode = 0, layout=coords[,-2])
  zy <- cbind(coords[,3],coords[,2])
  plot.igraph(g, vertex.color = hcc, edge.arrow.mode = 0, layout=zy%*%M)
  dev.off()
  
  return(hcc)
}


# Perform the mantel test on m1 and m2 with bootstrap 9000 as default
mantel_test <- function(m1, m2, b = 9000) {
  
  d1 <- as.dist(m1)
  d2 <- as.dist(m2)
  m <- mantel.rtest(d1, d2, b)
  return(m)
  
}


if(interactive()) {
  
  stime <- proc.time()
  l_m <- NULL
  ################################################# cutted ############################################################
  pathInC = "./../../../data/toyData/cutted_controls/"
  pathInP = "./../../../data/toyData/cutted_patients/"
  pathOutC = "./../../../data/toyData/results/3_community_spectral/cutted/membership_controls.csv"
  pathOutP = "./../../../data/toyData/results/3_community_spectral/cutted/membership_patients.csv"
  pathOutResults = "./../../../data/toyData/results/3_community_spectral/cutted/"
  
  controls1 <- performingCommunityDetection(pathInC, pathOutC)
  patient1 <- performingCommunityDetection(pathInP, pathOutP)
  
  coOc_Ctrl1 <- coOccurrence(pathOutC)
  coOc_Ptnt1 <- coOccurrence(pathOutP)
  
  l_m[1] <- mantel_test(coOc_Ctrl1, coOc_Ptnt1, 9000)$pvalue
  H1 <- makeHeatmap(coOc_Ctrl1, coOc_Ptnt1, pathOutResults)
  dvc1 <- plotF(coOc_Ctrl1, 10, pathInC, paste(pathOutResults, "graphControls.jpg", sep = ""), FALSE)
  dvp1 <- plotF(coOc_Ptnt1, 10, pathInC, paste(pathOutResults, "graphPatients.jpg", sep = ""), FALSE)
  plotF(coOc_Ptnt1+coOc_Ctrl1, 10, pathInC, paste(pathOutResults, "graphBoth.jpg", sep = ""), FALSE)
  
  MU1 <- mutinformation(dvp1,dvc1)
  Ec1 <- entropy(dvc1)
  Ep1 <- entropy(dvp1)
  MUN1 <- 2* (MU1 / (Ec1 + Ep1))
  ###################################################### t test ##########################################################
  pathInC = "./../../../data/toyData/t_test_controls/"
  pathInP = "./../../../data/toyData/t_test_patients/"
  pathOutC = "./../../../data/toyData/results/3_community_spectral/t_test_cutted/membership_controls.csv"
  pathOutP = "./../../../data/toyData/results/3_community_spectral/t_test_cutted/membership_patients.csv"
  pathOutResults = "./../../../data/toyData/results/3_community_spectral/t_test_cutted/"
  controls2 <- performingCommunityDetection(pathInC, pathOutC)
  patient2 <- performingCommunityDetection(pathInP, pathOutP)
  
  coOc_Ctrl2 <- coOccurrence(pathOutC)
  coOc_Ptnt2 <- coOccurrence(pathOutP)
  
  l_m[2] <- mantel_test(coOc_Ctrl2, coOc_Ptnt2, 9000)$pvalue
  H2 <- makeHeatmap(coOc_Ctrl2, coOc_Ptnt2, pathOutResults)
  dvc2 <- plotF(coOc_Ctrl2, 10, pathInC, paste(pathOutResults, "graphControls.jpg", sep = ""), FALSE)
  dvp2 <- plotF(coOc_Ptnt2, 10, pathInC, paste(pathOutResults, "graphPatients.jpg", sep = ""), FALSE)
  plotF(coOc_Ptnt2+coOc_Ctrl2, 10, pathInC, paste(pathOutResults, "graphBoth.jpg", sep = ""), FALSE)
  
  MU2 <- mutinformation(dvp2,dvc2)
  Ec2 <- entropy(dvc2)
  Ep2 <- entropy(dvp2)
  MUN2 <- 2* (MU2 / (Ec2 + Ep2))
  ###################################################### t test MST ########################################################
  pathInC = "./../../../data/toyData/t_test_MST_controls/"
  pathInP = "./../../../data/toyData/t_test_MST_patients/"
  pathOutC = "./../../../data/toyData/results/3_community_spectral/t_test_MST/membership_controls.csv"
  pathOutP = "./../../../data/toyData/results/3_community_spectral/t_test_MST/membership_patients.csv"
  pathOutResults = "./../../../data/toyData/results/3_community_spectral/t_test_MST/"
  
  controls3 <- performingCommunityDetection(pathInC, pathOutC)
  patient3 <- performingCommunityDetection(pathInP, pathOutP)
  
  coOc_Ctrl3 <- coOccurrence(pathOutC)
  coOc_Ptnt3 <- coOccurrence(pathOutP)
  l_m[3] <- mantel_test(coOc_Ctrl3, coOc_Ptnt3, 9000)$pvalue
  
  H3 <- makeHeatmap(coOc_Ctrl3, coOc_Ptnt3, pathOutResults)
  dvc3 <- plotF(coOc_Ctrl3, 10, pathInC, paste(pathOutResults, "graphControls.jpg", sep = ""), FALSE)
  dvp3 <- plotF(coOc_Ptnt3, 10, pathInC, paste(pathOutResults, "graphPatients.jpg", sep = ""), FALSE)
  plotF(coOc_Ptnt3+coOc_Ctrl3, 10, pathInC, paste(pathOutResults, "graphBoth.jpg", sep = ""), FALSE)
  
  MU3 <- mutinformation(dvp3,dvc3)
  Ec3 <- entropy(dvc3)
  Ep3 <- entropy(dvp3)
  MUN3 <- 2* (MU3 / (Ec3 + Ep3))
  #########################################################################################################################
  write.csv(l_m, "./../../../data/toyData/results/3_community_spectral/mantel.csv")
  etime <- stime - proc.time()
}




#plot(igraphDat,vertex.color = spc_data, vertex.size = 10, edge.arrow.mode=0, layout = coords[,-3])

# gm <- read.graph("./CTRL_lavoro.gml", format="gml") # Leggo il grafo
# ugm <- as.undirected(gm, "collapse") # Trasformo il grafo diretto in un grafo non diretto
# MA <- get.adjacency(ugm, attr="weight") # Ricavo la sua matrice di adiacenza
# M <- as.matrix(MA) # Trasformo l'oggetto ottenuto con get.adjacency in una matrice
# 
# mds
# D <- as.dist(exp(-M))
# fit <- cmdscale(D, 2)
# plot(fit[,1], fit[,2])

#communities
# path = "./communities/"
# spc = specc(M, 4, kernel = "rbfdot")
# spec_cl = spc@.Data
# # print("Matrice dei centri")
# b <- withinss(spc)
# c <- centers(spc)
# cc <- as.matrix(c)
# print(cc)
# # write.table(cc, "/home/alessandro/Scrivania/prova.csv", sep = ",")
# print("Centers number of every cluster is")
# # print(c)
# barplot(b, xlab = "Withinss intra cluster")
# centers(spc)
# size(spc)
# tab_spec = table(spec_cl)
# print(tab_spec)
# 
# for(i in 1:length(tab_spec)) {
#   D_index = which(spec_cl==i) # Indici dei cluster
#   write.csv(spec_cl, file = "")
# }
# 
# for(i in 1:length(tab_spec)) {
#   D_index = which(spec_cl==i)
#   yellow_g = subgraph(ugm, D_index)
#   lfr = layout.fruchterman.reingold(graph = yellow_g,niter=1000, area=100 * vcount(yellow_g)^2)
#   pdf(paste(path,"cluster",i,".pdf",sep=""))
#   plot(yellow_g, layout = lfr)
#   dev.off()
# }