# ExtraeBimestral('insumo_cvcalc_51.txt')

ExtraeBimestral<-function (nombreRaw) {

# Programa para extraer varios periodos de un archivo de insumo RAW para CVCalc y
# para grabarlos en archivos individuales para cada periodo.


a<-read.table(nombreRaw ,quote="",sep="\t",header=TRUE)
unique(a$Period)
length(unique(a$Period))

a$borrar<-TRUE
a$borrar[is.na(a$Sales)]<-FALSE
a<-a[a$borrar,1:5]


rw1513<-a[a$Period==2015003,]
rw1514<-a[a$Period==2015005,]
rw1515<-a[a$Period==2015007,]
rw1516<-a[a$Period==2015009,]
rw1517<-a[a$Period==2015011,]
rw1518<-a[a$Period==2016001,]

write.table(rw1513,file="RAW_data_PN2015003.txt",quote=FALSE,sep="\t",row.names=FALSE)
write.table(rw1514,file="RAW_data_PN2015005.txt",quote=FALSE,sep="\t",row.names=FALSE)
write.table(rw1515,file="RAW_data_PN2015007.txt",quote=FALSE,sep="\t",row.names=FALSE)
write.table(rw1516,file="RAW_data_PN2015009.txt",quote=FALSE,sep="\t",row.names=FALSE)
write.table(rw1517,file="RAW_data_PN2015011.txt",quote=FALSE,sep="\t",row.names=FALSE)
write.table(rw1518,file="RAW_data_PN2016001.txt",quote=FALSE,sep="\t",row.names=FALSE)

}