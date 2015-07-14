library(igraph)


function <- addNoise(g){
  for(i in 1:length(V(g))){
    v <- V(g)[i]$name
    nrb <- neighbors(g, v)
    for (j in 1:length(nrb)){
      vt <- get.vertex.attribute(g, "name", nrb[j])
      if(g[v, vt, attr = "weight"] <= 0){
        g[v, vt, attr = "weight"] = 1e-05
      }
    }
  }
}


if(interactive()){
  pathIn <- "./../../data/toyData/cutted_controls/CTRL_longo.gml"
  g <- read.graph(pathIn, format="gml")
  e_list <- get.edgelist(g)
  d <- degree(g, v=V(g), mode = "out") 
  

}
