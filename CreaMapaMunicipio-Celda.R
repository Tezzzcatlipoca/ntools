
quebras<-read.table("C:/Users/franro04/Documents/Proyectos/NewVIAB/QuebrasTodas.txt", header=TRUE, sep="\t", quote = "")

# Extraer todas las tiendas
tiendas<-sm('source_id, ciudad, estibge')

# Extraer celdas para cada tienda
corresp20<-ips(20,'source_id,cell_id')
corresp27<-ips(27,'source_id,cell_id')
corresp51<-ips(51,'source_id,cell_id')
corresp60<-ips(60,'source_id,cell_id')

# Juntar anteriores
juntos20<-merge(tiendas,corresp20,by="source_id",all.y = TRUE)
juntos27<-merge(tiendas,corresp27,by="source_id",all.y = TRUE)
juntos51<-merge(tiendas,corresp51,by="source_id",all.y = TRUE)
juntos60<-merge(tiendas,corresp60,by="source_id",all.y = TRUE)

# Encontrar las tienda-celdas con estado-municipio repetido y eliminarlas
juntos20$key<-paste0(juntos20$estibge,"-",juntos20$ciudad)
juntos27$key<-paste0(juntos27$estibge,"-",juntos27$ciudad)
juntos51$key<-paste0(juntos51$estibge,"-",juntos51$ciudad)
juntos60$key<-paste0(juntos60$estibge,"-",juntos60$ciudad)

repetidos20<-duplicated(juntos20$key)
repetidos27<-duplicated(juntos27$key)
repetidos51<-duplicated(juntos51$key)
repetidos60<-duplicated(juntos60$key)

unicos20<-juntos20[!repetidos20,]
unicos27<-juntos27[!repetidos27,]
unicos51<-juntos51[!repetidos51,]
unicos60<-juntos60[!repetidos60,]

# Extraer nombres de celda
celdas20<-ipc(20,'cell_id,cell_name')
celdas27<-ipc(27,'cell_id,cell_name')
celdas51<-ipc(51,'cell_id,cell_name')
celdas60<-ipc(60,'cell_id,cell_name')

# Juntar celdas con sus nombres
celtiend20<-merge(unicos20,celdas20,by="cell_id",all.x = TRUE)
celtiend27<-merge(unicos27,celdas27,by="cell_id",all.x = TRUE)
celtiend51<-merge(unicos51,celdas51,by="cell_id",all.x = TRUE)
celtiend60<-merge(unicos60,celdas60,by="cell_id",all.x = TRUE)

# Hacer mapa para cada índice
queb20<-quebras[,c('key','ESTADO','MUNIC','QUEBRA_20_21')]
queb27<-quebras[,c('key','ESTADO','MUNIC','Quebra_27')]
queb51<-quebras[,c('key','ESTADO','MUNIC','QUEBRA_51')]
queb60<-quebras[,c('key','ESTADO','MUNIC','nome_split_ENH')]

mapa20<-merge(celtiend20,queb20,by="key",all.x=TRUE)
mapa27<-merge(celtiend27,queb27,by="key",all.x=TRUE)
mapa51<-merge(celtiend51,queb51,by="key",all.x=TRUE)
mapa60<-merge(celtiend60,queb60,by="key",all.x=TRUE)

# Graba mapas
write.csv(mapa20,"mapa20.csv")
write.csv(mapa27,"mapa27.csv")
write.csv(mapa51,"mapa51.csv")
write.csv(mapa60,"mapa60.csv")

