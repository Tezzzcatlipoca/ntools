
#
#         Este programa compara dos strings y ve si son similares
#

# La función de abajo no esta vectorizada
parecen<-function(uno,dos){        # Busca variaciones de palabra uno en palabra dos
     uno<-gsub("\\(","",uno)
     uno<-gsub("\\)","",uno)
     uno<-gsub("[:punct:]"," ",uno)
     uno<-tolower(uno)
     dos<-gsub("\\(","",dos)
     dos<-gsub("\\)","",dos)
     dos<-gsub("[:punct:]"," ",dos)
     dos<-tolower(dos)
     if (length(uno)>1 || length(dos)>1) { stop('Error. Variable a comparar debe tener sólo 1 observación.') }
     
     # Abre diccionario de palabras comunes
     palabras.comunes<-read.csv("C:/Users/franro04/Documents/VBA/PalsSolo.csv",header = FALSE)
     cons.pals.com<-paste(palabras.comunes$V1,collapse = ";")
     
     palabras<-strsplit(uno," ")[[1]]
     marcador<-0
     pals.comunes<-0
     for (pal in 1:length(palabras)) {
          slowo<-palabras[pal]
          if (grepl(dos,pattern = slowo)) {
               marcador<-nchar(slowo)
               # Aquí poner la prueba de las palabras comunes
               if (grepl(cons.pals.com,pattern = slowo)) {pals.comunes<-nchar(slowo)}
          } else {
               rozmiar<-nchar(slowo)
               for (let in 1:rozmiar) {
                    patron<-slowo
                    substr(patron,let,let)<-"~"
                    patron<-gsub("~","(.?)",patron)
                    if (grepl(dos,pattern = patron) & rozmiar>2) {
                         marcador<-marcador+rozmiar
                         # Aquí poner la segunda prueba de las palabras comunes
                         if (grepl(cons.pals.com,pattern = patron)) {pals.comunes<-rozmiar}
                         break
                    }
               }
          }
     }
     
     espacios<-sum(grepl(uno,pattern = " "))
     tamano.uno<-marcador # espacios: en promedio 
     espacios<-sum(grepl(dos,pattern = " "))
     tamano.dos<-nchar(dos) #-espacios
     
     ratios<-tamano.uno/tamano.dos
     if (is.na(ratios)) {ratios<-0}
     if (ratios>.85 & tamano.uno>pals.comunes) {
          TRUE
     } else {
          FALSE
     }
}


# La función de abajo utiliza a la anterior como base y SÍ está vectorizada
archivo<-read.csv("C:/Users/franro04/Documents/Work Log/z9. Sep 2016/univ050916v2.csv")
claves<-read.csv("C:/Users/franro04/Documents/Work Log/z9. Sep 2016/CADEIAs.csv")

iteraciones<-dim(archivo)[1]
num.claves<-dim(claves)[1]

var1<-archivo[,'nome']
var.clave<-claves[,'nome']
which.cadeia<-rep(0,iteraciones)
which1<-rep(0,iteraciones)
which2<-rep(0,iteraciones)

for (i in 1:iteraciones) {
     for (nc in 1:num.claves) {
          #if(parecen(as.character(var.clave[nc]),as.character(var1[i]))) {
          if(parecen(as.character(var1[i]),as.character(var.clave[nc]))) {
               which.cadeia[i]<-nc
               which1[i]<-as.character(var1[i])
               which2[i]<-as.character(var.clave[nc])
               break
          }
     }
}

which.cadeia[which.cadeia==0]<-NA
cadeias<-claves[which.cadeia,c('cadeia','nome')]
archivo2<-cbind(archivo,cadeias,which1,which2)

write.csv(archivo2,file = "C:/Users/franro04/Documents/Work Log/z9. Sep 2016/out2.csv")
