
### Procesamiento de cargas de DA

library(xlsx)
# Quizás sea excel
archivo<-read.xlsx("H:/FIELDAM/Censo2016/Censo16_recebidos - Onda 2.xlsx",sheetName = "Data 0")

# Cambiaremos a numéricas las siguientes variables: 
archivo$cdUF<-as.integer(as.character(archivo$cdUF))
archivo$cdMunicipio<-as.integer(as.character(archivo$cdMunicipio))
archivo$cdDistrito<-as.integer(as.character(archivo$cdDistrito))
archivo$CodSubDistrito<-as.integer(as.character(archivo$CodSubDistrito))
archivo$CdSetorIBGE<-as.integer(as.character(archivo$CdSetorIBGE))

# Cambiarle el nombre a otras variables
ruta.cambios<-"J:/CENSOOBS/DATA/1CICLO/CENSO2016/Estimativa Universos/Data Quality/CUESTIONARIO_MM_2016/compraciones_cuestionarios_2015_2016.xlsx"
cambios<-read.xlsx(ruta.cambios,sheetName = "renombra_var")
tabla.cambios<-cambios[nchar(cambios$renombra)>0 & !is.na(cambios$renombra),c(1,3)]
for (i in 1:dim(tabla.cambios)[1]) {
     # Aquí buscar en los nombres de variable de "archivo" las observaciones 
     # de la tabla de cambios y modificarlas una por una
     nombres<-names(archivo)
     donde<-which(grepl(tabla.cambios[i,1],nombres))
     do<-donde[1]
     nombres[do]<-as.character(tabla.cambios[i,2])
}

names(archivo)<-nombres

# Crear directorio nuevo utilizando la fecha actual
actual<-getwd()
donde<-"J:/CENSOOBS/DATA/1CICLO/CENSO2016/Estimativa Universos/Data Quality/CARGAS_2016"
setwd(donde)
nombre<-paste0("CARGA",format(Sys.time(),"%d%m%Y"))
dir.create(nombre)
setwd(nombre)

# Dividir la base en dos partes (187 variables max- actualmente 187 vs 67)
# y grabarlos en la ruta destino
# Donde está el idLoja
nombres<-names(archivo)
donde<-which(grepl("idLoja",nombres))
prim_part<-(1:186)[!1:186 %in% donde] # Escoger todas las variables menos idLoja
prim_part<-c(donde,prim_part)

sec_part<-(187:length(nombres))[!187:length(nombres) %in% donde] # Escoger todas las variables menos idLoja
sec_part<-c(donde,sec_part)

# Se divide el archivo en dos partes
parte1<-archivo[,prim_part]
parte2<-archivo[,sec_part]

# Se define el nombre de la ruta y del archivo
ruta1<-paste0(donde,"/",nombre,"/carga",format(Sys.time(),"%d%m%Y"),"parte1.xlsx")
ruta2<-paste0(donde,"/",nombre,"/carga",format(Sys.time(),"%d%m%Y"),"parte2.xlsx")

# Se graba las dos partes
write.xlsx(parte1,ruta1,sheetName = "Sheet1",row.names = FALSE)
write.xlsx(parte2,ruta2,sheetName = "Sheet1",row.names = FALSE)

# Encontrar idlojas duplicados
cuales_duplicados<-archivo$idloja[duplicated(archivo$idloja)]
if(length(cuales_duplicados)>0){
     print(paste0("Los siguientes idLoja están duplicados",cuales_duplicados,collapse = ","))
}


# Regresa al directorio original
setwd(actual)

