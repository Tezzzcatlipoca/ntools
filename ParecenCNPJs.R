
#
# Busca un CNPJ dentro de otra cadena con CNPJs, devuelve coincidencias
#

archivo2<-read.csv("C:/Users/franro04/Documents/Work Log/z91. Oct 2016/TiendasAVerif.csv")
claves<-read.csv("C:/Users/franro04/Documents/Work Log/z9. Sep 2016/CADEIAs.csv")
# Cambiar de formato numérico a otro (para evitar perder últimos dígitos)
archivo2$cnpj2<-as.character(archivo2$Cnpj)
# Quitar signos de puntuación
claves$cnpj2<-str_replace_all(claves$cnpj,"[:punct:]","")
# Identificar todos los CNPJ posibles
repetidos<-duplicated(claves$cnpj2)
claves.cnpj<-claves[!repetidos,]
# Renombrar las variables para poderlas identificar después del Merger
names(claves.cnpj)<-paste0("cnpj-",names(claves.cnpj))

encontradas<-merge(archivo2,claves.cnpj,by.x="cnpj2",by.y = "cnpj-cnpj2",all.x = TRUE)

write.csv(archivo2,file = "C:/Users/franro04/Documents/Work Log/z91. Oct 2016/outCNPJ.csv")
