
#
#    Acceso rápido a los archivos de Tots y Universos en una sola línea
#

ultimotot<-function(indice) {
DirTots<-read.table("C:/Users/franro04/Documents/R Code/DirectorioTots.txt",sep="\t",header = TRUE)
as.character(DirTots[DirTots$Ind==indice,'Completo'][1])
}

ultimouniv<-function(enh=TRUE) {
direcUnis<-read.table("C:/Users/franro04/Documents/R Code/DirectorioUnis.txt",sep="\t",header = TRUE)
if (enh==TRUE) { 
     cual<-'Y' 
} else {
     cual<-'N'
}
as.character(direcUnis[direcUnis$Enh==cual,'Complete'][1])
}

