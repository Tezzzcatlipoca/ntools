#
#              Programa para extraer todas las cadenas de MM 2.0, darles nombre de Cadena
#              (Bandeira) y agruparlas por Bandeira
#

# Abrir universo
DirecUnis<-read.table("DirectorioUnis.txt",header = TRUE,sep="\t",stringsAsFactors = FALSE)
nombreEnh<-DirecUnis[DirecUnis$Num==1 & DirecUnis$Enh=="Y","Complete"]
uniEnh<-read.csv(nombreEnh)

# Extraer cadenas
activas<-c(1,2)
cadenas<-uniEnh[uniEnh$mktr>0 & uniEnh$CONDICAO %in% activas & !is.na(uniEnh$mktr),c("LOJA1","mktr","CADEIA","ACV","CONDICAO")]
# cadenas$fator_fim<-as.integer(as.character(cadenas$fator_fim))
cadenas$ACV<-as.integer(as.character(cadenas$ACV))


# Abrir info de cadenas
library(xlsx)
cad.path<-"J:/ESTATOC/DATA/CADEIAS.xls"
CADEIA<-read.xlsx(cad.path,sheetName = "RELACAO CADEIAS")
BANDEIRAS<-CADEIA[,c("CADEIA","BANDEIRA")]
BANDEIRAS$CADEIA<-as.integer(BANDEIRAS$CADEIA)

# Juntar ambas
informe<-merge(cadenas,BANDEIRAS,by="CADEIA",all.x=TRUE,all.y=FALSE)

# Corrige error de cadena faltante
informe$BANDEIRA<-as.character(informe$BANDEIRA)
informe[informe$CADEIA==369,"BANDEIRA"]<-"Mateus Supermercados"
informe$BANDEIRA<-as.factor(informe$BANDEIRA)

# Crear informe
#library(dplyr)
library(mosaic)
#tabla<-sum(fator_fim~BANDEIRA,data=informe)
tabla<-sum(ACV~BANDEIRA,data=informe)
nombres<-names(tabla)
numeros<-as.integer(tabla)

#Razem<-data.frame(CADENA=nombres,UNIV=numeros)
Razem<-data.frame(CADENA=nombres,ACV=numeros)

# Exportar tabla
View(Razem)
write.csv(Razem,file = "CAD-ACV.csv",row.names = FALSE,quote = FALSE)

