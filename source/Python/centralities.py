#----------------------------------------------------------------------
# centralities
#
# Computes centralities of graphs
# 
# Author: Emanuele Pesce
#----------------------------------------------------------------------
import networkx as nx
import csv
import fnmatch
import os

def applyCentralities(pathIn, pathOut):
    """ apply centralities to all file in pathIn directory and save them all in 
        pathOut directory
            
        @type pathIn:  string
        @param pathIn: directory where the file .gml are 
        @type pathOut: string
        @param graph: directory where to save all files
    """
    n = [("Node","Betweenness","Indegree","Closeness","PageRank")]    
    
    for f in os.listdir(pathIn):
        if fnmatch.fnmatch(f, '*.gml'):
            print f
            p_in = pathIn + f
            g=nx.read_gml(p_in)
            C = centralities(g)
            p_out = pathOut + f
            p_out = p_out.replace(".gml", ".csv")
            with open(p_out, 'w') as fp:
                fp = csv.writer(fp, delimiter=',')
                fp.writerows(n)
                fp.writerows(C)
    

def centralities(g):
    """ Computes graph centrality (betweenneess, indegree, closeness, pageRank)
            
        @type graph: networkx graph
        @param graph: graph
        
        @return: a matrix where each columns is a centrality and each row is a 
                node
    """
    bet = nx.betweenness_centrality(g, weight="weight")
    ind = nx.in_degree_centrality(g)
    cls = nx.closeness_centrality(g)
    prk = nx.pagerank(g)

    
    M = zip(bet, bet.values(), ind.values(), cls.values(), prk.values())
    
    return M
    
    
    
if __name__ ==  "__main__":
#    path = "./../../data/toyData/cutted_controls/CTRL_amore.gml"
#    g=nx.read_gml(path)
#    w = nx.get_edge_attributes(g, "weight")
#    bet = nx.betweenness_centrality(g)
#    ind = nx.in_degree_centrality(g)
#    cls = nx.closeness_centrality(g)
#    prk = nx.pagerank(g, max_iter=1000)
    
#    n = [("Node","Betweenness","Indegree","Closeness","PageRank")]
#    C = centralities(g)
    
#    with open('test.csv', 'w') as fp:
#        f = csv.writer(fp, delimiter=',')
#        f.writerows(n)
#        f.writerows(C)
    
    pathIn = "./../../data/toyData/cutted_controls/"
    pathOut = "./../../data/toyData/cutted_controls/centralities/"
    applyCentralities(pathIn, pathOut)
    
    pathIn = "./../../data/toyData/cutted_patients/"
    pathOut = "./../../data/toyData/cutted_patients/centralities/"
    applyCentralities(pathIn, pathOut) 