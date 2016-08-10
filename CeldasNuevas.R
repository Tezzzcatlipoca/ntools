### **********************************************************************************************************
### IMPORTANTE: Antes de correr este programa, es necesario correr el programa de 'ExportaDeSASaCSV-R.SAS'
### **********************************************************************************************************


### ARREGLAR: Agregar posibilidad de checar en univ 2.0!!!!!!!!!!!!!!!
### ARREGLAR: Hacer directorio de Tots y Universos por periodo para acceso con menos errores
### ARREGLAR: Guardar universo en caché

#Cómo llamar a la función
universos(2016016,27,'mar','Marzo','noenh',3115,1,0)


#*****************************************************************************************
period_id<-2016016
index_id<-27
month<-'mar' #Versión breve (usualmente 3 letras: Jan, Mar...)
mes<-'Marzo' #Versión larga (Enero / Marzo)
version<-'noenh'  #Puede ser 'enh' o 'noenh' solamente
buscadas<-3115  # Debe ser una celda a la vez -1284
tiendas_que_entran_en_celda<-1  # Obtener esta información de IV
grabar_archivo_cezinhos<-0 # Para grabar el archivo, debe ser igual a 1
#*****************************************************************************************

#Función
universos<-function (period_id, index_id, month, mes, version, buscadas,tiendas_que_entran_en_celda, grabar_archivo_cezinhos) {

library(RODBC)
year<-as.integer(substr(period_id,1,4))
donde<-getwd()
direccion<-paste("H:/ESTATIST/PROCESSOS E CLIENTES/Migração Universos entre IVPO-OE E MSci/",year-1,"/Ind ",index_id,"/",mes,substr(year,3,6),sep="")
setwd(direccion)
if (index_id==51) {
     tot<-"refri"
} else {
     tot<-"tot"
}
if (version=='noenh') {
    nombretot<-paste(tot,year-1,"_",version,"_",month,".csv",sep="")
} else {
    nombretot<-paste(tot,year-1,"_",month,".csv",sep="")
}
print('Leyendo archivo TOT...')
tot<-read.csv(nombretot)
#print('Archivo leído con éxito.')
direc2<-paste("J:/CENSO/DATA/CENSO",year-1,sep="")
setwd(direc2)
if (version=='noenh') {
    nombreuni<-paste("uni",year-1,"_",version,"_",month,".csv",sep="")
} else {
    nombreuni<-paste("uni",year-1,"_",month,".csv",sep="")
}
if (!exists('uni')) {  # Carga el universo sólo si no ha sido cargado
print('Leyendo archivo de Universo...')
uni<-read.csv(nombreuni)
#print('Archivo leído con éxito.')
}

# Leer SMS
smsh<-odbcConnect('SMSH', uid='nretail', pwd = 'nretail')
quer<-paste("SELECT ","cell_id, index_id, period_id, cell_name, universe_source, ideal_source, status_id, sample_source, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16"," FROM index_period_cell WHERE period_id = ",period_id," AND index_id = ", index_id, sep="")
cells<-sqlQuery(smsh,query=quer)
odbcClose(smsh)

if (dim(cells)[1]==0) {
    stop('El índice no contiene datos para este periodo.')
}

#C1
try({
extr1<-cells[cells$c1>0 & cells$status_id==2,]
extr1$Consolidada<-extr1$c1
extr1$Cezinho<-'c1'
extr1<-extr1[,c('cell_id','cell_name','Consolidada','Cezinho')]
},silent = TRUE)

#C2
try({
extr2<-cells[cells$c2>0 & cells$status_id==2,]
extr2$Consolidada<-extr2$c2
extr2$Cezinho<-'c2'
extr2<-extr2[,c('cell_id','cell_name','Consolidada','Cezinho')]
},silent = TRUE)

#C3
try({
extr3<-cells[cells$c3>0 & cells$status_id==2,]
extr3$Consolidada<-extr3$c3
extr3$Cezinho<-'c3'
extr3<-extr3[,c('cell_id','cell_name','Consolidada','Cezinho')]
},silent = TRUE)

#C4
try({
extr4<-cells[cells$c4>0 & cells$status_id==2,]
extr4$Consolidada<-extr4$c4
extr4$Cezinho<-'c4'
extr4<-extr4[,c('cell_id','cell_name','Consolidada','Cezinho')]
},silent = TRUE)

#C5
try({
extr5<-cells[cells$c5>0 & cells$status_id==2,]
extr5$Consolidada<-extr5$c5
extr5$Cezinho<-'c5'
extr5<-extr5[,c('cell_id','cell_name','Consolidada','Cezinho')]
},silent = TRUE)

#C6
try({
extr6<-cells[cells$c6>0 & cells$status_id==2,]
extr6$Consolidada<-extr6$c6
extr6$Cezinho<-'c6'
extr6<-extr6[,c('cell_id','cell_name','Consolidada','Cezinho')]
},silent = TRUE)

#C7
try({
extr7<-cells[cells$c7>0 & cells$status_id==2,]
extr7$Consolidada<-extr7$c7
extr7$Cezinho<-'c7'
extr7<-extr7[,c('cell_id','cell_name','Consolidada','Cezinho')]
},silent = TRUE)

#C8
try({
extr8<-cells[cells$c8>0 & cells$status_id==2,]
extr8$Consolidada<-extr8$c8
extr8$Cezinho<-'c8'
extr8<-extr8[,c('cell_id','cell_name','Consolidada','Cezinho')]
},silent = TRUE)

#C9
try({
extr9<-cells[cells$c9>0 & cells$status_id==2,]
extr9$Consolidada<-extr9$c9
extr9$Cezinho<-'c9'
extr9<-extr9[,c('cell_id','cell_name','Consolidada','Cezinho')]
},silent = TRUE)

#C10
try({
extr10<-cells[cells$c10>0 & cells$status_id==2,]
extr10$Consolidada<-extr10$c10
extr10$Cezinho<-'c10'
extr10<-extr10[,c('cell_id','cell_name','Consolidada','Cezinho')]
},silent = TRUE)

#C11
try({
extr11<-cells[cells$c11>0 & cells$status_id==2,]
extr11$Consolidada<-extr11$c11
extr11$Cezinho<-'c11'
extr11<-extr11[,c('cell_id','cell_name','Consolidada','Cezinho')]
},silent = TRUE)

#C12
try({
extr12<-cells[cells$c12>0 & cells$status_id==2,]
extr12$Consolidada<-extr12$c12
extr12$Cezinho<-'c12'
extr12<-extr12[,c('cell_id','cell_name','Consolidada','Cezinho')]
},silent = TRUE)

#C13
try({
extr13<-cells[cells$c13>0 & cells$status_id==2,]
extr13$Consolidada<-extr13$c13
extr13$Cezinho<-'c13'
extr13<-extr13[,c('cell_id','cell_name','Consolidada','Cezinho')]
},silent = TRUE)

#C14
try({
extr14<-cells[cells$c14>0 & cells$status_id==2,]
extr14$Consolidada<-extr14$c14
extr14$Cezinho<-'c14'
extr14<-extr14[,c('cell_id','cell_name','Consolidada','Cezinho')]
},silent = TRUE)

#C15
try({
extr15<-cells[cells$c15>0 & cells$status_id==2,]
extr15$Consolidada<-extr15$c15
extr15$Cezinho<-'c15'
extr15<-extr15[,c('cell_id','cell_name','Consolidada','Cezinho')]
},silent = TRUE)

#C16
try({
extr16<-cells[cells$c16>0 & cells$status_id==2,]
extr16$Consolidada<-extr16$c16
extr16$Cezinho<-'c16'
extr16<-extr16[,c('cell_id','cell_name','Consolidada','Cezinho')]
},silent = TRUE)

consolida<-rbind(extr1, extr2, extr3, extr4, extr5, extr6, extr7, extr8, extr9, extr10, extr11, extr12, extr13, extr14, extr15, extr16)

consolida<-consolida[order(consolida$Consolidada),]
consolida<-consolida[order(consolida$cell_id),]
consolida<-consolida[!is.na(consolida$cell_id),]

nombre<-paste("Celdas_",index_id,"_",period_id,".csv",sep="")

setwd(donde)
if (grabar_archivo_cezinhos==1) {
    write.csv(consolida, file=nombre, quote=FALSE, row.names = FALSE)
}
  
padre<-consolida$cell_id[consolida$Consolidada %in% buscadas]
relacion<-consolida[consolida$Consolidada %in% buscadas,]
hijas<-consolida$Consolidada[consolida$cell_id %in% padre]
relacion2<-consolida[consolida$cell_id %in% padre,]
parientes<-c(padre,hijas)

# Consolidada o no
if (length(padre)==0) { 
  stop('La celda no está consolidada') 
} 

#Algunos tots tienen los nombres de las variables en mayúscula: Esto causa problemas abajo
if (sum(names(tot)=='CELL')>0) {
  tot$cell<-tot$CELL
}

if (sum(names(tot)=='MKTR')>0) {
  tot$mktr<-tot$MKTR
}

if (sum(names(tot)=='FATOR')>0) {
  tot$fator<-tot$FATOR
}

# Obteniendo datos necesarios
activas<-c(1,2)
data1<-tot[tot$CONDICAO %in% activas & tot$cell %in% parientes,c('cell','mktr','fator','LOJA1')]
data2<-uni[,c('ACV','LOJA1')]
permisibles<-unique(data1$LOJA1)
data2.5<-data2[data2$LOJA1 %in% permisibles,]
data3<-merge(data1,data2.5,by='LOJA1',all=FALSE)

#Saber si es una celda SOT o no
totalregistros<-nrow(data3)
mktrs<-data3$mktr
no<-is.na(mktrs)
newmktr<-mktrs[!no]
totalsot<-length(newmktr)
if (totalregistros==totalsot) {
  sot<-1
} else {
  sot<-0
}
if (sot==1) {
  print('Celda SOT. Revisar si la tienda reportada por IV aparece abajo:')
  print(data3)
}

#Converting from factors to numbers to be able to count
data3$ACV<-as.double(as.character(data3$ACV))


# Tabla de ACV por celda
ACVs<-aggregate(ACV~cell,data3,FUN=sum)
Ns<-aggregate(fator~cell,data3,FUN=sum)
NewACV<-ACVs[ACVs$cell==buscadas,"ACV"]  # Propuesta de Nuevo ACV
sumN<-sum(Ns$fator)
ourN<-Ns[Ns$cell==buscadas,"fator"]
proporcion<-ourN/sumN
SMS_N<-cells[cells$cell_id==padre,"universe_source"] # N del padre
SMS_Name<-cells[cells$cell_id==buscadas,"cell_name"] #Nombre de célula
propuestaN<-round(SMS_N*proporcion,0) # Propuesta de N para célula nueva
if (propuestaN==0 & SMS_N>0) {propuestaN==1}
propPadre<-SMS_N-propuestaN # Propuesta de N para célula padre
ideal<-cells[cells$cell_id==padre,"ideal_source"]
fisicas<-cells[cells$cell_id==padre,"sample_source"]
# Ajustando el Nuevo ideal para célula padre
if (ideal==fisicas) {
    NuevoIdeal<-ideal
} else {
    NuevoIdeal<-ideal-propuestaN      
}
if (NuevoIdeal<fisicas) {NuevoIdeal<-fisicas}

if (sot==0) {
  tiendas_que_entran_en_celda<-round(ideal*proporcion,0)
}

RespuestaHija<-data.frame(Celda=buscadas, Name=SMS_Name, N=propuestaN,ACV=NewACV,Ideal=tiendas_que_entran_en_celda)
RespuestaPadre<-data.frame(Celda=padre,N=propPadre,Ideal=NuevoIdeal)
print('Datos de celda solicitada:')
print(RespuestaHija)
print('También modificar:')
print(RespuestaPadre)

} # Fin de la función


