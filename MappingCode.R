library(raster)
map<-getData('GADM',country='MEX',level=2)
# gsimplify (map, tot=.01) # The smaller the number, the simpler the map

map$addedlayer<-c(1,2,3,4,5,6) # How to add layers to the map

library(tmap)
tmshape(map)+tm_polygons("Nombre1","Nombre2",palette=c('blue','red'))

# Adding layers
# g3<-g1 + g2
# print(g3)

save_tmap(objectname,'name.svg')
