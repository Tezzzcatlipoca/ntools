
#BorrarNAS ('2015015',5)

BorrarNASPeriodo<-function (Periodo,Columna) {

nombre<-paste("RAW_data_PN",Periodo,".txt",sep="")

a<-read.table(nombre,quote="",sep="\t",header=TRUE)

a$borrar<-TRUE
a$borrar[is.na(a[,Columna])]<-FALSE
b<-a[a$borrar,1:5]

write.table(b,file=nombre,quote=FALSE,sep="\t",row.names=FALSE)

}


# Para el insumo completo ----------------------

BorrarNAS<-function (nombre,Columna) {

a<-read.table(nombre,quote="",sep="\t",header=TRUE)

a$borrar<-TRUE
a$borrar[is.na(a[,Columna])]<-FALSE
b<-a[a$borrar,1:5]

write.table(b,file=nombre,quote=FALSE,sep="\t",row.names=FALSE)

}
