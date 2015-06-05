#' aggregation
#' 
#' Author: Emanuele Pesce
library(TopKLists)

#' Aggregates matrices
#' Get all matrix in 'path' and merge them all in a matrix in which each 
#' row is a sujbect (file) and each column is a value
#'  
#' @param path, path of matrices to aggregate
#' @return A list of vectors
#' @examples
#' M <-  m <- genMatrix("./../../data/toyData/controls/")
genMatrix <- function(path = "./../../data/toyData/controls/"){
  #get all filenames
  fileNames  <- list.files(path)
  
  M <- list()
  #for each file, read it as matrix, then transform it in a vector and append to M
  for (i in 1:length(fileNames)){
    fname <- fileNames[i]
    dat <- read.csv(paste(path, fname, sep = ""), header = FALSE, sep = " ");
    m <- as.matrix(dat);
    v <- as.vector(t(m));
    M[[i]] <- v
  }
  
  #return
  return(M)
}


if(interactive()){
  mControls <- genMatrix("./../../data/toyData/controls/")
  mPatients <- genMatrix("./../../data/toyData/patients/")
#   #path of the dataset
#   path <- "./../../data/toyData/controls/CTRL_amore.txt";
#   
#   #read graph
#   dat <- read.csv(path, header = FALSE, sep = " ");
#   m <- as.matrix(dat);
#   v <- as.vector(t(m));
#   
#   l <- list()
#   l[[1]] <- v
#   l[[2]] <- v

}