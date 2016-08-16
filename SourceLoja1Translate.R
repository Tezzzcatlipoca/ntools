#
#         Para traducir source_id a loja1 (usando SMS)
#

source_loja<-function(source_id) {

library(RODBC)
sms<-odbcConnect('SMS',uid='nretail',pwd = 'nretail')
quero<-paste("SELECT source_id, loja_amostra_mestra, scan_source_id FROM source_master WHERE source_id = ",source_id,sep = "")
tiendas<-sqlQuery(sms,query = quero)
close(sms)

if (dim(tiendas)[1]==0) { stop('No existe el source_id') }

loja1<-tiendas$loja_amostra_mestra
scan_s_i<-tiendas$scan_source_id
if (loja1==0) {
     scan<-odbcConnect('scan',uid='scanning',pwd='scanning')
     quero2<-paste("SELECT source_id, cod_amostra_mestra FROM source_master WHERE source_id =",scan_s_i,sep = "")
     tiendas2<-sqlQuery(scan,query = quero2)
     close(scan)
     if (dim(tiendas2)[1]==0) { stop('No existe información de loja1 en retail o scan') }
     loja1<-tiendas2$cod_amostra_mestra
} 
     
print(loja1)

} # End of function

#
#         Traducir de mktr a loja1
#

mktr_loja<-function(mktr) {
     
     scan<-odbcConnect('scan',uid='scanning',pwd='scanning')
     quero2<-paste("SELECT source_id, cod_amostra_mestra FROM source_master WHERE source_id =",mktr,sep = "")
     tiendas2<-sqlQuery(scan,query = quero2)
     close(scan)
     if (dim(tiendas2)[1]==0) { stop('No existe información de loja1 en retail o scan') }
     loja1<-tiendas2$cod_amostra_mestra
     print(loja1)
     
} # End of function

#
#         Mktr to Loja1 (Usando el último archivo de universo)
#

mktr_loja2<-function(mktr,Enh="N") {
     ubi.uni<-read.table("C:/Users/franro04/Documents/R Code/DirectorioUnis.txt",sep = "\t",header = TRUE, stringsAsFactors = FALSE)
     year<-2015
     max.num<-max(ubi.uni[ubi.uni$Year==year & ubi.uni$Enh==Enh,"Num"])
     name.univ<-ubi.uni[ubi.uni$Year==year & ubi.uni$Enh==Enh & ubi.uni$Num==max.num,"Complete"]
     univ<-read.csv(name.univ)
     nombres<-names(univ)
     if (sum(grepl(pattern = "MKTR", nombres))>0) {univ$mktr<-univ$MKTR}  # Algunas variables están escritas diferente
     if (sum(grepl(pattern = "loja1", nombres))>0) {univ$LOJA1<-univ$loja1}
     extr<-univ[univ$mktr==mktr & !is.na(univ$mktr),c("LOJA1","mktr")]
     if (dim(extr)[1]==0) { stop("El universo no contiene este mktr") }
     loja1<-as.character(extr$LOJA1[1])
     print(loja1)
} # End of function

#
#         Loja1 to Mktr (Usando el último archivo de universo)
#

loja_mktr2<-function(loja1,Enh="N") {
     ubi.uni<-read.table("C:/Users/franro04/Documents/R Code/DirectorioUnis.txt",sep = "\t",header = TRUE, stringsAsFactors = FALSE)
     year<-2015
     max.num<-max(ubi.uni[ubi.uni$Year==year & ubi.uni$Enh==Enh,"Num"])
     name.univ<-ubi.uni[ubi.uni$Year==year & ubi.uni$Enh==Enh & ubi.uni$Num==max.num,"Complete"]
     univ<-read.csv(name.univ)
     nombres<-names(univ)
     if (sum(grepl(pattern = "MKTR", nombres))>0) {univ$mktr<-univ$MKTR}  # Algunas variables están escritas diferente
     if (sum(grepl(pattern = "loja1", nombres))>0) {univ$LOJA1<-univ$loja1}
     extr<-univ[univ$LOJA1==loja1 & !is.na(univ$LOJA1),c("LOJA1","mktr")]
     if (dim(extr)[1]==0) { stop("El universo no contiene este LOJA1") }
     mktr<-as.character(extr$mktr[1])
     print(mktr)
} # End of function



