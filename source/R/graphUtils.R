getMatrixFromFile <- function(filename){
  dat <- read.csv(filename, header = FALSE, sep = " ")
  m <- as.matrix(dat);
  
  return(m)
}

getEdgesAsVector <- function(graph, listNames){
  
  listEdges <- list()
  for(i in 1:length(listNames)){
    s <- listNames[i]
    ss <- strsplit(s, "_")
    row <- strtoi(ss[[1]][1])
    col <- strtoi(ss[[1]][2])
    listEdges[i] <- graph[row, col]
  }
  return(listEdges)
}


if(interactive()){
  graph <- getMatrixFromFile("./../../data/toyData/controls/CTRL_amore.txt")
  
  listNames <- c("76_74", "75_73", "36_68", "80_82", "41_37", "79_81", "35_67", "80_18", "17_29", "36_35")
  edges <- getEdgesAsVector(graph, listNames)
  
}