
#
#         Este programa compara dos strings y ve si son similares
#

# La función de abajo no esta vectorizada
parecen<-function(uno,dos){        # Busca variaciones de palabra uno en palabra dos
     library(stringdist)
     library(stringr)
     uno<-str_replace_all(uno,"[:punct:]","")
     dos<-str_replace_all(dos,"[:punct:]","")
     uno<-tolower(uno)
     dos<-tolower(dos)
     if (length(uno)>1 || length(dos)>1) { stop('Error. Variable a comparar debe tener sólo 1 observación.') }
     
     # Abre diccionario de palabras comunes
     palabras.comunes<-read.csv("C:/Users/franro04/Documents/VBA/PalsSolo.csv",header = FALSE)
     palabras.comunes$V1<-tolower(palabras.comunes$V1)
     cons.pals.com<-paste(palabras.comunes$V1,collapse = ";")

     palabras<-strsplit(uno," ")[[1]]
     marcador<-0
     pals.comunes<-0
     
     if (nchar(uno)<8 & nchar(dos)<8) {
          sensib<-1
     } else {
          sensib<-3
     }
     
     if (t(amatch(dos,uno, maxDist = sensib))>0) {     # Si coincide todo el string
          found<-1
     } else {
          found<-0
     }
     
     for (pal in 1:length(palabras)) {     # Dividir el nombre en palabras
          if (found==1) break # Si ya encontramos coincidencia, evita los ciclos
          slowo<-palabras[pal]      
          if (t(amatch(dos,slowo, maxDist = 1))>0) {
               marcador<-nchar(slowo)+marcador
               rozmiar<-nchar(slowo)
               # Si la palabra encontrada era palabra común, identificarla
               for (comun in 1:length(palabras.comunes$V1)) {
                    if (t(amatch(palabras.comunes$V1[comun],slowo,maxDist = 1))>0) {
                         pals.comunes<-nchar(slowo)
                         break
                    }
               }
               
          }
     }
     
     
     tamano.uno<-marcador # espacios: en promedio 
     tamano.dos<-nchar(dos) #-espacios
     
     ratios<-tamano.uno/tamano.dos
     
     if (is.na(ratios)) {ratios<-0}
     if (found==1) {
          ratios<-1
          tamano.uno<-1
          pals.comunes<-0
     }
     if (ratios>.85 & tamano.uno>pals.comunes) {
          TRUE
     } else {
          FALSE
     }
}

t<-function(innnput){
     if(is.na(innnput)){
          out<-0
     } else {
          out<-innnput
     }
     out
}

# Función vectorizada (sin abrir archivo)
parecen.vec<-function(one,two){
     veces<-length(one)
     veces2<-length(two)
     if(veces!=veces2){stop('Ambas variables de entrada deben tener misma longitud.')}
     outvec<-rep(NA,veces)
     for(i in 1:veces) {
          outvec[i]<-parecen(one[i],two[i])
     }
     outvec
}


# La función de abajo utiliza a la anterior como base y SÍ está vectorizada
archivo<-read.csv("C:/Users/franro04/Documents/Work Log/z91. Oct 2016/TiendasAVerif.csv")
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

write.csv(archivo2,file = "C:/Users/franro04/Documents/Work Log/z91. Oct 2016/out1.csv")

