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


def applyAnalysis(pathIn, filename = "analysis.csv"):
    """ apply centralities to all file in pathIn directory and save them all in 
        pathOut directory
            
        @type pathIn:  string
        @param pathIn: directory where the file .gml are 
        @type pathOut: string
        @param graph: directory where to save all files
    """
    n = [("ID","average_path","weighted_average_path","average_clust","largest_component")]    
    with open(filename, 'w') as fp:
        f = csv.writer(fp, delimiter=',')
        f.writerows(n)
                
        for fl in os.listdir(pathIn):
            if fnmatch.fnmatch(fl, '*.gml'):
                p_in = pathIn + fl
                print p_in
                g=nx.read_gml(p_in)
                C = g_analysis(g, fl.replace(".gml",""))                
                
                f.writerow(C)
                
    
def g_analysis(graph, name):
    """ Computes graph centrality (betweenneess, indegree, closeness, pageRank)
            
        @type graph: networkx graph
        @param graph: graph
        
        @return: a matrix where each columns is a centrality and each row is a 
                node
    """
    avg_len = nx.average_shortest_path_length(graph)
    avg_len_w = nx.average_shortest_path_length(g, weight = "weight")
    avg_clust = nx.transitivity(graph)
    largest_component = len(nx.strongly_connected_components(graph)[0])
    
    M = [name,avg_len, avg_len_w, avg_clust, largest_component]
    
    return M

if __name__ ==  "__main__":
    path = "./../../data/toyData/cutted_controls/CTRL_amore.gml"
    path2 =  "./../../data/toyData/cutted_patients/SLA2_altezza.gml"

    g=nx.read_gml(path)
    l = nx.average_shortest_path_length(g)
    clust_coeff = nx.transitivity(g)
    largest_component = len(nx.strongly_connected_components(g)[0])
        
#    a = g_analysis(g, "a")
    
    pathIn = "./../../data/toyData/cutted_controls/"
    pathOut = "./../../data/toyData/cutted_controls/centralities/"
    applyAnalysis(pathIn)
    
#    pathIn = "./../../data/toyData/cutted_patients/"
#    pathOut = "./../../data/toyData/cutted_patients/centralities/"
#    applyCentralities(pathIn, pathOut)