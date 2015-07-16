#' Execution and testing of spectral clust method for community detection problem
#' The test is performed on
#' - Bonferroni corrected t-test-cutted graph
#' - Shortest path + Bonferroni corrected t-test-cutted graph
#' - Shortest path + MST cutted graph
#' To evaluate the result is used a hitmap
#'
#' Author: Alessandro Merola

rm(list=ls())

library(kernlab)
library(igraph)
library(gplots)
library(ade4)

trySpec <- function(M){
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
<<<<<<< HEAD
        
        
=======
      
          
>>>>>>> 01f6a00e28da44682ad9bfa054c03dd1524ca332
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
<<<<<<< HEAD
  colnames(outp) <- listNames
  write.csv(outp[,-1], pathForCommunity, row.names = outp[,1])
=======
    colnames(outp) <- listNames
    write.csv(outp[,-1], pathForCommunity, row.names = outp[,1])
>>>>>>> 01f6a00e28da44682ad9bfa054c03dd1524ca332
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
  return(occurrenceMatrix)
}


# Compute the heatmap for m1 and m2 cooccurrence matrix
# @pathOut is the output file to save
makeHeatmap <- function(m1, m2, pathOut) {
  
  # heatmap
<<<<<<< HEAD
  #   jpeg(filename=paste(pathOut, "heat_controls.jpeg", sep=""), 
  #        width = 1000, height = 1000)
  h1 <- heatmap.2(t(m1), tracecol = F, main = "Community Controls")
  #   dev.off()
  #   jpeg(filename=paste(pathOut, "heat_patients.jpeg", sep=""), 
  #        width = 1000, height = 1000)
  #   h2 <- heatmap.2(t(m2), tracecol = F, main = "Community Patients")
  #   dev.off()
  #   
  #   sum_occ <- m1 + m2
  #   jpeg(filename=paste(pathOut, "heat_sum.jpeg", sep=""), 
  #        width = 1000, height = 1000)
  #   sumH <- heatmap.2(t(sum_occ), tracecol = F, main = "Community Sum")
  #   dev.off()
  
  #   res <- list("h1"=h1, "h2"=h2, "sum"=sumH)
  #   return(res)
=======
#   jpeg(filename=paste(pathOut, "heat_controls.jpeg", sep=""), 
#        width = 1000, height = 1000)
  h1 <- heatmap.2(t(m1), tracecol = F, main = "Community Controls")
#   dev.off()
#   jpeg(filename=paste(pathOut, "heat_patients.jpeg", sep=""), 
#        width = 1000, height = 1000)
#   h2 <- heatmap.2(t(m2), tracecol = F, main = "Community Patients")
#   dev.off()
#   
#   sum_occ <- m1 + m2
#   jpeg(filename=paste(pathOut, "heat_sum.jpeg", sep=""), 
#        width = 1000, height = 1000)
#   sumH <- heatmap.2(t(sum_occ), tracecol = F, main = "Community Sum")
#   dev.off()
  
#   res <- list("h1"=h1, "h2"=h2, "sum"=sumH)
#   return(res)
>>>>>>> 01f6a00e28da44682ad9bfa054c03dd1524ca332
}


if(interactive()) {
<<<<<<< HEAD
  #   stime <- proc.time()
  #   ################################################ cutted #########################################################
  #   
  #   pathInC = "./../../../data/toyData/cutted_controls/"
  #   pathInP = "./../../../data/toyData/cutted_patients/"
  #   pathOutC = "./../../../data/toyData/results/3_community_spectral/cutted/membership_controls.csv"
  #   pathOutP = "./../../../data/toyData/results/3_community_spectral/cutted/membership_patients.csv"
  #   pathOutResults = "./../../../data/toyData/results/3_community_spectral/cutted/"
  #   
  #   controls <- performingCommunityDetection(pathInC, pathOutC)
  #   patient <- performingCommunityDetection(pathInP, pathOutP)
  #   
  #   coOc_Ctrl <- coOccurrence(pathOutC)
  #   coOc_Ptnt <- coOccurrence(pathOutP)
  #   
  #   
  #   H <- makeHeatmap(coOc_Ctrl, coOc_Ptnt, pathOutResults)
  #   
  #   ################################################# t test #######################################################
      pathInC = "./../../../data/toyData/t_test_controls/"
      pathInP = "./../../../data/toyData/t_test_patients/"
      pathOutC = "./../../../data/toyData/results/3_community_spectral/t_test_cutted/membership_controls.csv"
      pathOutP = "./../../../data/toyData/results/3_community_spectral/t_test_cutted/membership_patients.csv"
      pathOutResults = "./../../../data/toyData/results/3_community_spectral/t_test_cutted/"
      
      controls <- performingCommunityDetection(pathInC, pathOutC)
      patient <- performingCommunityDetection(pathInP, pathOutP)
      
      coOc_Ctrl <- coOccurrence(pathOutC)
      coOc_Ptnt <- coOccurrence(pathOutP)
  
    
    
     H <- makeHeatmap(coOc_Ctrl, coOc_Ptnt, pathOutResults)
    
  #   ################################################# t test MST ####################################################
  #     pathInC = "./../../../data/toyData/t_test_MST_controls/"
  #     pathInP = "./../../../data/toyData/t_test_MST_patients/"
  #     pathOutC = "./../../../data/toyData/results/3_community_spectral/t_test_MST/membership_controls.csv"
  #     pathOutP = "./../../../data/toyData/results/3_community_spectral/t_test_MST/membership_patients.csv"
  #     pathOutResults = "./../../../data/toyData/results/3_community_spectral/t_test_MST/"
  #     
  #     controls <- performingCommunityDetection(pathInC, pathOutC)
  #     patient <- performingCommunityDetection(pathInP, pathOutP)
  #     
  #     coOc_Ctrl <- coOccurrence(pathOutC)
  #     coOc_Ptnt <- coOccurrence(pathOutP)
  #   
  #     
  #     H <- makeHeatmap(coOc_Ctrl, coOc_Ptnt, pathOutResults)  
  #     etime <- stime - proc.time()
  ######################################################################################################################
=======
  stime <- proc.time()
  ### cutted
    
    pathInC = "./../../../data/toyData/cutted_controls/"
    pathInP = "./../../../data/toyData/cutted_patients/"
    pathOutC = "./../../../data/toyData/results/3_community_spectral/cutted/membership_controls.csv"
    pathOutP = "./../../../data/toyData/results/3_community_spectral/cutted/membership_patients.csv"
    pathOutResults = "./../../../data/toyData/results/3_community_spectral/cutted/"
    
    controls <- performingCommunityDetection(pathInC, pathOutC)
    patient <- performingCommunityDetection(pathInP, pathOutP)
    
    coOc_Ctrl <- coOccurrence(pathOutC)
    coOc_Ptnt <- coOccurrence(pathOutP)

    
    H <- makeHeatmap(coOc_Ctrl, coOc_Ptnt, pathOutResults)
  
#   ### t test
#     pathInC = "./../../../data/toyData/t_test_controls/"
#     pathInP = "./../../../data/toyData/t_test_patients/"
#     pathOutC = "./../../../data/toyData/results/3_community_spectral/t_test_cutted/membership_controls.csv"
#     pathOutP = "./../../../data/toyData/results/3_community_spectral/t_test_cutted/membership_patients.csv"
#     pathOutResults = "./../../../data/toyData/results/3_community_spectral/t_test_cutted/"
#     
#     controls <- performingCommunityDetection(pathInC, pathOutC)
#     patient <- performingCommunityDetection(pathInP, pathOutP)
#     
#     coOc_Ctrl <- coOccurrence(pathOutC)
#     coOc_Ptnt <- coOccurrence(pathOutP)
# 
#   
#   
#    H <- makeHeatmap(coOc_Ctrl, coOc_Ptnt, pathOutResults)
#   
#   ### t test MST
#     pathInC = "./../../../data/toyData/t_test_MST_controls/"
#     pathInP = "./../../../data/toyData/t_test_MST_patients/"
#     pathOutC = "./../../../data/toyData/results/3_community_spectral/t_test_MST/membership_controls.csv"
#     pathOutP = "./../../../data/toyData/results/3_community_spectral/t_test_MST/membership_patients.csv"
#     pathOutResults = "./../../../data/toyData/results/3_community_spectral/t_test_MST/"
#     
#     controls <- performingCommunityDetection(pathInC, pathOutC)
#     patient <- performingCommunityDetection(pathInP, pathOutP)
#     
#     coOc_Ctrl <- coOccurrence(pathOutC)
#     coOc_Ptnt <- coOccurrence(pathOutP)
#   
#     
#     H <- makeHeatmap(coOc_Ctrl, coOc_Ptnt, pathOutResults)  
#     
#     etime <- stime - proc.time()
  
>>>>>>> 01f6a00e28da44682ad9bfa054c03dd1524ca332
}


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