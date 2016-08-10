# Para extraer celdas en TOTs y SMS en TODOS los periodos anteriores

# Cómo llamar a la función
CeldaTodosPeriodos(32,949,'noenh')


# *****************************************************************************************************
# Datos a proporcionar
index_id<-21
version<-'noenh'  #Puede ser 'enh' o 'noenh' solamente
buscada<-4053  # Debe ser una celda a la vez -1284
# *****************************************************************************************************


# FUNCIÓN
CeldaTodosPeriodos<- function(index_id,buscada,version) {

  # Leer tot
donde<-getwd()
direccion<-"H:/ESTATIST/PROCESSOS E CLIENTES/Migração Universos entre IVPO-OE E MSci/2010/historicos/"
setwd(direccion)

if (index_id>51) {
  cuenta<-2
} else {
  cuenta<-4
}

TOText<-data.frame(cell=0,fator=0,mktr=0,loja1=0,condicao=0,year=0)
TOText<-TOText[-1,]

final<-2011+(cuenta-1)
for (i in 2011:final) {
      nombretot<-paste("tot",index_id,"_",i,"_",version,"_mar.csv",sep="")
      tot<-read.csv(nombretot)
      # Extracción
      if (sum(names(tot)=='CELL')>0) {
        tot$cell<-tot$CELL
      }
      if (sum(names(tot)=='MKTR')>0) {
        tot$mktr<-tot$MKTR
      }
      
      if (sum(names(tot)=='fat21')>0) {
        tot$fator<-tot$fat21
      }
      
      if (sum(names(tot)=='fat27')>0) {
           tot$fator<-tot$fat27
      }
      
      if (sum(names(tot)=='FATOR')>0) {
        tot$fator<-tot$FATOR
      }
      
      if (sum(names(tot)=='CONDICAO')>0) {
        tot$condicao<-tot$CONDICAO
      }
      
      if (sum(names(tot)=='LOJA1')>0) {
        tot$loja1<-tot$LOJA1
      }
      
      tot$year<-i
      
      # Agregando cada año al total del TOT
      extracto<-tot[tot$cell==buscada,c('cell','fator','mktr','loja1','condicao','year')]
      if (dim(extracto)[1]>0) {
          TOText<-rbind(TOText,extracto)
      }
      
}

setwd(donde) #Vuelve al directorio origen

# Leer SMS
library(RODBC)
smsh<-odbcConnect('SMSH', uid='nretail', pwd = 'nretail')
quer<-paste("SELECT ","cell_id, index_id, period_id, cell_name, universe_source, ideal_source, status_id, sample_source"," FROM index_period_cell WHERE index_id = ", index_id, " AND cell_id = ", buscada, sep="")
SMSext<-sqlQuery(smsh,query=quer)
odbcClose(smsh)

# Encontrando últimos registros para celdas activas
activas<-c(1,2)
SMSactivas<-SMSext[SMSext$status_id==2,]
TOTactivas<-TOText[TOText$condicao %in% activas,]

# Imprimir datos
if (dim(TOText)[1]>0) {print('Datos encontrados en TOTs: TOText')}
if (dim(TOTactivas)[1]>0) {
    TOT_N<-aggregate(fator~year,TOTactivas,FUN = sum)
    print('Activas encontradas')
    print(TOT_N)
    View(TOTactivas)
} else {
    print ('Activas NO encontradas')
    View(TOText)
}
if (dim(SMSext)[1]>0) {print('Datos encontrados en SMS: SMSext')}
if (dim(SMSactivas)[1]>0) {
  print('Activas encontradas')
  print(SMSactivas)
  View(SMSactivas)
} else {
  print('Activas NO encontradas')
  View(SMSext)
}


} # Fin de la función

