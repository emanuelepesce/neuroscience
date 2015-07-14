#' makeMask 
#' 
#' generate and apply the mask obtained with t-test
#' 
#' Author: Alessandro Merola

rm(list=ls())
library(igraph)
library(TopKLists)
library(igraph)
library(miscTools)

source("./../graphUtils.R", chdir = T)


# Compute the mask's union
makeUnion <- function(ctrl, pznt) {

  i <- 1
  unionList <- matrix(nrow = 0, ncol = 2)
  for (x in ctrl) {
    unionList <- insertRow(unionList, i, x)
    i <- i + 1
  }

  for (x in ctrl) {
    for (y in pznt) {
      if (x[i,1]!=y) {
        unionList <- insertRow(unionList, i, x)
        i <- i + 1
      }
    }
  }
  return (unionList)

}


if(interactive()) {
  
  contr <- read.csv("./../../../data/toyData/controls/withNoise/t_test_mask/t_test_mask_controls.csv")
  pazie <- read.csv("./../../../data/toyData/controls/withNoise/t_test_mask/t_test_mask_patients.csv")
  unione <- makeUnion(contr, pazie)
  #print (unione)

}