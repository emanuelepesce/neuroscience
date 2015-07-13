import csv

#load data
pathfile = "./../../../data/regioni_coordinate/aalCOG.txt"
coords = []
with open(pathfile, 'rb') as csvfile:
     spamreader = csv.reader(csvfile, delimiter=' ', quotechar='|')
     for row in spamreader:
         coords.append(row)

add_sphere = bpy.ops.mesh.primitive_ico_sphere_add
for index in range(0, len(coords)): 
    add_sphere(location=(float(coords[i][1]), float(coords[i][1]), float(coords[i][1])))