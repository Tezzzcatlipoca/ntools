
#
# Programa para encontrar segmentos utilizados pero que hace 2 años están inactivos (para reciclar)
#

if (aarea>9) { stop ('Área es incorrecta')}
if (is.na(aarea)) {aarea<-1:9}

library(RODBC)
smsh<-odbcConnect("SMSH",uid="nretail",pwd="nretail")
busqueda<-paste("SELECT cell_id, cell_name, period_id, index_id, status_id FROM index_period_cell WHERE index_id = ",indice," AND ((cell_id >= 5000 AND cell_id <= 5999) OR (cell_id >= 15000 AND cell_id <= 15999) OR (cell_id >= 25000 AND cell_id <= 25999)) AND period_id > 2015000 ")
extracto<-sqlQuery(smsh,query=busqueda)
close(smsh)

cell.txt<-paste('000000',as.character(extracto$cell_id),sep="")
cell.txt<-substr(cell.txt,nchar(cell.txt)-4,nchar(cell.txt))

# extraer segmento
seg<-substr(cell.txt,1,4)

# extraer área
area<-substr(cell.txt,2,2) # En este caso todas las áreas deben ser 5

# Hacer tabla de status para cada segmento
extracto$usable<-FALSE
segs<-unique(extracto$segmento)
for (www in 1:length(segs)) {
     sprawdzic<-sum(extracto$status_id[extracto$segmento==segs[www]])==sum(extracto$segmento==segs[www])
     extracto$usable[extracto$segmento==segs[www]]<-sprawdzic
}

usar<-unique(extracto$segmento[extracto$usable==TRUE])
