rm(list=ls())

counting <- function(pathIn = "./../../../data/toyData/cutted_patients/spectral/membership_patients.csv") {
  data <- read.table(pathIn, sep=",", header=TRUE)
  data <- data[,-1]
  
  m <- as.matrix(data)
  numRow <- dim(m)[1]
  k <- 0
  a <- matrix(nrow = 90, ncol = 90, data = 0)
  
  for (i in 1:numRow) {
    for (j in 1:numRow) {
      r1 <- m[i,]
      r2 <- m[j,]
      for (z in 1:length(m[i,])) {
        if(m[i,][z] == m[j,][z]) {
          a[i,j] <- a[i,j] + 1
        }
      }
    }
  }
  return(a)
}


if(interactive()) {
  mm <- counting()
  heatmap.2(t(mm), tracecol = F)
}