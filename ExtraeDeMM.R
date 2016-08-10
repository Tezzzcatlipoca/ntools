
############################################################################
#              Extraer cadenas SOT de muestra maestra, con algunas de      #
#              sus características.                                        #
############################################################################

# Cómo llamar la función

extraeMM(noenh=1,vars = c('mktr','condicao','loja1'),nombres=0) # Para extraer info

extraeMM(noenh=1,nombres=1) # Para extraer nombres de variables

# Código

extraeMM<-function(noenh=1,vars=NA,nombres=0) {

lib<-"J:/CENSO/DATA/CENSO2015"
archivos<-dir(lib,pattern = ".csv")
localiz<-paste(lib,archivos,sep="/")
fechas<-file.info(localiz)$ctime
juntos<-data.frame(archivo=archivos,fecha=fechas)
juntos<-juntos[order(juntos$fecha),]
juntos<-juntos[grepl(pattern = "uni|Uni",juntos$archivo),]
if (noenh==1) {
     juntos<-juntos[grepl(pattern = "noenh",juntos$archivo),]
} else {
     juntos<-juntos[!grepl(pattern = "noenh",juntos$archivo),]
}
ultimo<-dim(juntos)[1]
abrir<-as.character(juntos[ultimo,1])
nombre.completo<-paste(lib,abrir,sep="/")
univer<-read.csv(nombre.completo)
if (is.na(vars) & nombres==0) {
     univer
} else if (nombres==0) {
     univer[,vars]
} else {
     sort(names(univer))
}

}


