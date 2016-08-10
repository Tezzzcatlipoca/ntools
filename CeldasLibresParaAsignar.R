
#################################################################################
#          Para encontrar segmentos de celda disponibles para asignar           #
#                                                                               #
#         IMPORTANTE: Antes de usar cualquier segmento, es importante           #
#          también buscarlo en el gerador en cuestión (este programa sólo       #
#          busca los segmentos disponibles en SMS, pero algunos ya están        #
#          asignados en la muestra maestra, aunque nunca han sido usados).      #
#################################################################################


segmentoslibres<-function(indice,aarea,view.c=FALSE) {
if (aarea>9) { stop ('Área es incorrecta')}
if (is.na(aarea)) {aarea<-1:9}

library(RODBC)
smsh<-odbcConnect("SMSH",uid="nretail",pwd="nretail")
busqueda<-paste("SELECT cell_id, cell_name, period_id, index_id, status_id FROM index_period_cell WHERE index_id = ",indice)
extracto<-sqlQuery(smsh,query=busqueda)
close(smsh)

# Convertir celdas en texto
cell.txt<-paste('000000',as.character(extracto$cell_id),sep="")
cell.txt<-substr(cell.txt,nchar(cell.txt)-4,nchar(cell.txt))

# extraer segmento
seg<-substr(cell.txt,1,4)

# extraer área
area<-substr(cell.txt,2,2)

# crear tabla
tabla<-data.frame(segmento=as.integer(seg),area=as.integer(area))

# dejar sólo valores únicos
tabla.unicos<-unique(tabla)

# ordenar por área
tabla.unicos<-tabla.unicos[order(tabla.unicos$segmento),]
minimo<-min(tabla.unicos$segmento)
maximo<-max(tabla.unicos$segmento)

# Identificar todos los segmentos posibles
segs.posibles<-seq(minimo,maximo)

# Identificar las áreas de cada segmento posible
insumo.areas<-paste('000000',as.character(segs.posibles),sep="")
insumo.areas<-substr(insumo.areas,nchar(insumo.areas)-3,nchar(insumo.areas))
area.pos<-substr(insumo.areas,2,2)

# Crear tabla de posibles
tabla.posibles<-data.frame(segmentos=segs.posibles,area=area.pos)

# Filtrar solo segmentos disponibles
tabla.disponibles<-tabla.posibles[!(tabla.posibles$segmentos %in% tabla.unicos$segmento), ]

# arrojar tabla de áreas y celdas disponibles
if (view.c==TRUE) { View(tabla.disponibles) }

OUTP<-tabla.disponibles[tabla.disponibles$area==aarea,]
print(OUTP,row.names = FALSE)

} # Fin de la función


