
#
# Código para Clasificar Celdas por sus Canales utilizando Expresiones Regulares
# guardadas en un archivo "diccionario"
#


clasifica<-function(archivo){ 
     # Abre diccionarios
     dicc<-read.table("dicc-canales.txt",header = TRUE,sep = "\t",stringsAsFactors = FALSE)
     sot<-read.table("dicc-sot.txt",header = TRUE,sep = "\t",stringsAsFactors = FALSE)
     # Abre archivo a clasificar
     tabla<-read.csv(archivo)
     tabla$canal<-''

     regs<-dim(dicc)[1]
     for(ind.regs in 1:regs) {
               reg.exp<-dicc$Criterio[ind.regs]
               SType<-dicc$ShopType[ind.regs]
               donde<-which(grepl(reg.exp,tabla$cell_name))
               tabla$canal[donde]<-as.character(SType)
     }
     
     tabla$cadenas<-'No'
     regs2<-dim(sot)[1]
     for(ind.regs in 1:regs2) {
          reg.exp<-sot$Criterio[ind.regs]
          SType<-sot$Tipo[ind.regs]
          donde<-which(grepl(reg.exp,tabla$cell_name))
          tabla$cadenas[donde]<-as.character(SType)
     }
     
     modif<-gsub(".csv","-Clasificado.csv",archivo)
     write.csv(tabla,modif)
}


