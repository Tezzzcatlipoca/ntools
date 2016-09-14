### **********************************************************************************************************
### IMPORTANTE: Antes de correr este programa, es necesario correr el programa de 'ExportaDeSASaCSV-R.SAS'
### **********************************************************************************************************


### ARREGLAR: Agregar posibilidad de checar en univ 2.0!!!!!!!!!!!!!!!
### ARREGLAR: Guardar universo en caché

### ---- YA se abren los archivos automáticamente
### ---- Falta ajustar el cálculo de ACV, Ideal, etc para ENH y las variables de input


#Cómo llamar a la función
universos(2016016,27,'','','noenh',3115,1,0)


#*****************************************************************************************
index_id<-27
period_id<-2016016
buscadas<-3115  # Debe ser una celda a la vez -1284
tiendas_que_entran_en_celda<-1  # Obtener esta información de IV
grabar_archivo_cezinhos<-0 # Para grabar el archivo, debe ser igual a 1
#*****************************************************************************************

# Función
desconsolida.univ<-function (index_id, period_id, buscadas, tiendas_que_entran_en_celda=1, grabar_archivo_cezinhos=0) {

if (index_id=='' || period_id=='' || buscadas=='') {
     stop('Obligatorio introducir índice, periodo y celdas buscadas, en ese orden.')
}

library(RODBC)
year<-as.integer(substr(period_id,1,4))
donde<-getwd()
# Abrir directorios
DirTots<-read.table("J:/CENSO/DATA/CENSO2015/DirectorioTots.txt",header = TRUE,sep="\t")
direcUnis<-read.table("J:/CENSO/DATA/CENSO2015/DirectorioUnis.txt",header = TRUE,sep="\t")

#direccion<-paste("H:/ESTATIST/PROCESSOS E CLIENTES/Migração Universos entre IVPO-OE E MSci/",year-1,"/Ind ",index_id,"/",mes,substr(year,3,6),sep="")
#setwd(direccion)
#if (index_id==51) {
#     tot<-"refri"
#} else {
#     tot<-"tot"
#}
#if (version=='noenh') {
#    nombretot<-paste(tot,year-1,"_",version,"_",month,".csv",sep="")
#} else {
#    nombretot<-paste(tot,year-1,"_",month,".csv",sep="")
#}

#print('Archivo leído con éxito.')
#direc2<-paste("J:/CENSO/DATA/CENSO",year-1,sep="")
#setwd(direc2)
if ((index_id>59 && index_id) <65 || index_id==84) {
    ver<-"N"
   # nombreuni<-paste("uni",year-1,"_",version,"_",month,".csv",sep="")
} else {
    ver<-"Y"
    #nombreuni<-paste("uni",year-1,"_",month,".csv",sep="")
}
#if (!exists('uni')) {  # Carga el universo sólo si no ha sido cargado
nombreuni<-as.character(direcUnis[direcUnis$Enh==ver,7][1])
print('Leyendo archivo de Universo...')
uni<-read.csv(nombreuni)
#print('Archivo leído con éxito.')
#}
if (index_id<60 || index_id>64) {
     print('Leyendo archivo TOT...')
     nombretot<-as.character(DirTots[DirTots$Ind==index_id,7][1])
     tot<-read.csv(nombretot)
} else {
     tot<-uni[uni$CONDICAO %in% c(1,2) & !is.na(uni$CONDICAO),]
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

if (sum(tolower(names(tot))=='celsorv')>0) {
     donde<-which(tolower(names(tot))=='celsorv')
     tot$cell<-tot[,donde]
}

if (sum(tolower(names(tot))=='cellsorv')>0) {
     donde<-which(tolower(names(tot))=='cellsorv')
     tot$cell<-tot[,donde]
}

if (sum(tolower(names(tot))=='cell_sorv')>0) {
     donde<-which(tolower(names(tot))=='cell_sorv')
     tot$cell<-tot[,donde]
}

if (sum(tolower(names(tot))=='cel_sorv')>0) {
     donde<-which(tolower(names(tot))=='cel_sorv')
     tot$cell<-tot[,donde]
}

if (sum(names(tot)=='MKTR')>0) {
  tot$mktr<-tot$MKTR
}

if (sum(names(tot)=='FATOR')>0) {
  tot$fator<-tot$FATOR
}

if (sum(tolower(names(tot))=='fator_fim')>0) {
     donde<-which(tolower(names(tot))=='fator_fim')
     tot$fator<-tot[,donde]
}

if (sum(names(tot)=='acv')>0) {
     tot$ACV<-tot$acv
}

if (sum(names(tot)=='loja1')>0) {
     tot$LOJA1<-tot$loja1
}

if (sum(names(uni)=='acv')>0) {
     uni$ACV<-uni$acv
}

if (sum(names(uni)=='loja1')>0) {
     uni$LOJA1<-uni$loja1
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
mktrs<-as.integer(as.character(data3$mktr))
no<-is.na(mktrs)
newmktr<-mktrs[!no]
totalsot<-length(newmktr)
if (totalregistros==totalsot) {
  sot<-1
} else {
  sot<-0
}
if (sot==1) {
  print('Celda posiblemente SOT. Revisar si la tienda reportada por IV aparece abajo:')
  print(data3[data3$cell==buscadas & !is.na(data3$cell),])
}

#Converting from factors to numbers to be able to count
data3$ACV<-as.double(as.character(data3$ACV))
if(class(data3$ACV)=='factor'){data3$ACV<-as.integer(as.character(data3$ACV))}
if(class(data3$cell)=='factor'){data3$cell<-as.integer(as.character(data3$cell))}
if(class(data3$fator)=='factor'){data3$fator<-as.integer(as.character(data3$fator))}

# Tabla de ACV por celda
ACVs<-aggregate(ACV~cell,data3,FUN=sum)
Ns<-aggregate(fator~cell,data3,FUN=sum)
NewACV<-ACVs[ACVs$cell==buscadas,"ACV"]  # Propuesta de Nuevo ACV
if(length(NewACV)==0){NewACV<-0}
sumN<-sum(Ns$fator)
ourN<-Ns[Ns$cell==buscadas,"fator"]
if (length(ourN)==0) {ourN<-0.0001}
proporcion<-ourN/sumN
SMS_N<-cells[cells$cell_id==padre,"universe_source"] # N del padre
SMS_Name<-as.character(cells[cells$cell_id==buscadas,"cell_name"]) #Nombre de célula
propuestaN<-round(SMS_N*proporcion,0) # Propuesta de N para célula nueva
if (propuestaN==0 & SMS_N>0) {propuestaN<-1}
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


