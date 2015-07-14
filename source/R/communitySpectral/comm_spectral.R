library(kernlab)
library(igraph)


# Performs the specified community detection method
performingCommunityDetection <- function(pathIn = "./../../../data/toyData/cutted_controls/", pathOut = "./../../../../data/toyData/cutted_controls/spectral", algorithm = "specc", verbose = FALSE) {
  
  pathForCommunity = pathOut
  listNames <- "names"
  k <- 0
  files <- list.files(path = pathIn) # take all files in pathIn
  if(algorithm == "specc") {
    for(i in 1:length(files)) {
      cfile <- paste(pathIn, files[i], sep="")
      if(grepl(cfile, pattern = "*.gml")) {
        k <- k +1
        
        if (verbose == TRUE) {
          print ("Processing")
          print (cfile)
        }
        
        # get the graph from the file
        g <- read.graph(cfile, format="gml")
        ugm <- as.undirected(gm, "collapse")
        MA <- get.adjacency(ugm, attr="weight")
        M <- as.matrix(MA)
        listNames <- c(listNames, files[i])
        
        # execute spectral clustering method
        spc = specc(M, 10, kernel = "rbfdot")
        w <- withinss(spc)
        c <- centers(spc)
        d <- size(spc)
        
        spec_cl = spc@.Data
        names <- V(g)$name
        if(k <= 1) {
          outp <- cbind(name, spec_cl)
        }
        else {
          outp <- cbind(outp, spec_cl)
        }
      }
    }
    #outp <- cbind(outp, spec_cl)
  }
  colnames(outp) <- listNames
  write.csv(outp[,-1], pathForCommunity, row.names = outp[,1] )
}

if(interactive()) {
  pathInC = "./../../../data/toyData/cutted_controls/"
  pathInP = "./../../../data/toyData/cutted_patients/"
  pathOutC = "./../../../data/toyData/cutted_controls/spectral/membership_controls.csv"
  pathOutP = "./../../../data/toyData/cutted_patients/spectral/membership_patients.csv"
  performingCommunityDetection(pathInC, pathOutC)
  performingCommunityDetection(pathInP, pathOutP)
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