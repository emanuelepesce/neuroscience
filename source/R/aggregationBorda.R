#' aggregationBorda
#' 
#' functions for pruning and aggregating edges of graphs
#' 
#' Author: Emanuele Pesce
library(TopKLists)

source("./graphUtils.R")

#' Aggregates matrices
#' Get all matrix in 'path' and merge them all in a matrix in which each 
#' row is a sujbect (file) and each column is a value
#'  
#' @param path, path of matrices to aggregate
#' @return A list of vectors
#' @examples
#' M <-  m <- genMatrix("./../../data/toyData/controls/")
genMatrix <- function(path = "./../../data/toyData/controls/"){
  # get all filenames
  fileNames  <- list.files(path)
  nRoi = 90
  
  # build names, creates a vector where each element is a string and rapresent 
  # row-column notations
  v_names <- list()
  for (i in 1:nRoi){
    si <- toString(i)
    for (j in 1:nRoi){
      sj <- toString(j)
      v_names <- c(v_names, paste(si, sj, sep = "_"))
    }
  }
  
  M <- list()
  #for each file, read it as matrix, then transform it in a vector and append to M
  for (i in 1:length(fileNames)){
    fname <- fileNames[i]
    dat <- read.csv(paste(path, fname, sep = ""), header = FALSE, sep = " ")
    m <- as.matrix(dat);
    v <- as.vector(t(m));
    # associates names
    names(v) <- v_names
    v <- sort(v, decreasing = TRUE)
    # saves names after sorting elements
    M[[i]] <- names(v)
  }
  
  # return
  return(M)
}

writeBordaMatrix <- function(filename, bordaTop, bordaScores, n=90){
  vec <- bordaScores
  names(vec) <- bordaTop
  
  M <- matrix(nrow = 90, ncol = 90)
  for(i in 1:n){
    for(j in 1:n){
      idx <- paste(toString(i), toString(j), sep = "_")
      M[i,j] <- vec[idx]
    }
  }
  
  write.table(M, file = filename, row.names = FALSE, col.names=FALSE )
}


if(interactive()){
  mControls <- genMatrix("./../../data/toyData/controls/")
  mPatients <- genMatrix("./../../data/toyData/patients/")
  
  kv <- 8000
  c_outBorda=Borda(mControls,space = mControls)
  p_outBorda=Borda(mPatients,space = mPatients)
  
  writeBordaMatrix(filename = "./../../data/toyData/extract/bordaMatrixControls.txt",
                   c_outBorda$TopK$mean, c_outBorda$Scores$mean)
  
  writeBordaMatrix(filename = "./../../data/toyData/extract/bordaMatrixPatients.txt",
                   p_outBorda$TopK$mean, p_outBorda$Scores$mean)
  
  # screeplot
#   v <- outBorda$Scores$mean[1:kv]
#   e <- outBorda$TopK$mean
#   
#   nv <- (v-min(v))/(max(v)-min(v))
#   nv <- 1-nv 
#   
#   #plot(rey = v[1:10], x = outBorda$TopK , type='b', main = 'screeplot', xlab = 'edges', ylab = 'mean')
#   plot(y = nv, x = seq(1:kv), type='b', main = 'screeplot', xlab = 'edges', ylab = 'mean')
#   
#   nv <- outBorda$Scores$mean[1:8000]
#   nv <- (nv-min(nv))/(max(nv)-min(nv))
#   nv <- 1-nv
#   plot(y = nv, x = seq(1:8000), type='b', main = 'screeplot', xlab = 'edges', ylab = 'mean')
#   
#   # edges
#   graph <- getMatrixFromFile("./../../data/toyData/controls/CTRL_amore.txt")
#   edges <- getEdgesAsVector(graph, e)
#   plot(y = as.numeric(sort(unlist(edges)[1:300], decreasing = TRUE)), x = seq(1:300), type='b', main = 'screeplot', xlab = 'edges', ylab = 'mean')

  
}