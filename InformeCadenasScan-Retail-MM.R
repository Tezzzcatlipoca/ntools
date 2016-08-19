
# Extrae celdas de cadena de SMS-Scan [Periodo 1]
# Extrae celdas de cadena de MM [Generado por PED]
# Extrae celdas de cadena de SMS-Retail [Periodo 2]

####                CORREGIR:  En la tabla de BANDEIRAS para Universo, obtenerlas a través de SCAN, no de CADEIA


periodo_scan<-2016042
periodo_retail<-2016018
nombre.univ<-"C:/Users/franro04/Documents/Work Log/z8. Ago 2016/ActUniv-Cadenas.csv" # Esta extracción de MM solo tiene CADEIA y mktr

# Extraer información de tiendas de cadena de SMS - Scan
library(dplyr)
library(RODBC)
smss<-odbcConnect('SMSS',uid='scanning',pwd='scanning')
smssh<-odbcConnect('SMSSH',uid='scanning',pwd='scanning')
# quero.scan<-paste("SELECT ips.source_id, sm.cadena_de_tiendas FROM source_master sm RIGHT JOIN (SELECT source_id FROM index_period_source WHERE period_id =",periodo_scan,") ips ON sm.source_id=ips.source_id ",sep="")
quero.smss<-paste("SELECT source_id FROM index_period_source WHERE index_id = 1 AND formato IN (1,2,3,4,5,14) AND period_id =",periodo_scan," AND status_id IN (6,7,8,9)",sep="")
extra.scan<-sqlQuery(smssh,query = quero.smss)
tiendas_scan<-paste(extra.scan$source_id,sep=",",collapse = ",")
quero.smssh<-paste("SELECT source_id, cadena_de_tiendas FROM source_master WHERE source_id IN (",tiendas_scan,")")
extra.scan.sm<-sqlQuery(smss,query = quero.smssh)
close(smss)
close(smssh)

# Extraer información de tiendas de cadena de SMS - Retail
smsh<-odbcConnect('smsh',uid='nretail',pwd='nretail')
sms<-odbcConnect('sms',uid='nretail',pwd='nretail')
          #quero.retail<-paste("SELECT source_id, cadena_de_tiendas FROM index_period_source WHERE period_id IN (",periodo_retail,") ",sep="")
quero.retailh<-paste("SELECT source_id FROM index_period_source WHERE period_id =",periodo_retail," AND status_id IN (6,7,8,9)",sep="")
extra.retailh<-sqlQuery(smsh,query = quero.retailh)
tiendas_retail<-paste(extra.retailh$source_id,sep=",",collapse = ",")
quero.retail<-paste("SELECT source_id, cadena_de_tiendas FROM source_master WHERE source_id IN (",tiendas_retail,")")
extra.retail.sm<-sqlQuery(sms,query = quero.retail)
# Dejar solamente tiendas SOT
sot.retail<-filter(extra.retail.sm,cadena_de_tiendas>0)
close(sms)
close(smsh)

# Abrir información sobre las cadenas
library(xlsx)
CADEIAS<-read.csv("C:/Users/franro04/Documents/Uzyteczne pliki/DiccionarioCadeiasScan-Retail.csv")
CADEIAS.RET<-CADEIAS

# Adjuntar información de cadenas a las extracciones
cod.unicos<-duplicated(CADEIAS$CADSCAN)
CADEIAS<-CADEIAS[!cod.unicos,] # Remueve los CODSCAN repetidos - Scan
CADEIAS$BANDEIRA<-as.character(CADEIAS$BANDEIRA)
CADEIAS<-select(CADEIAS,CADEIA,BANDEIRA,CADSCAN)
scan.cadenas<-merge(extra.scan.sm,CADEIAS,by.x="cadena_de_tiendas",by.y="CADSCAN",all.x=TRUE)
if (dim(scan.cadenas)[1]!=dim(extra.scan.sm)[1]) { stop ("Merge incorrecto, hay CADSCAN duplicadas.")}
scan.cadenas$CADSCAN<-as.integer(scan.cadenas$cadena_de_tiendas)
scan.cadenas$cadena_de_tiendas<-as.integer(scan.cadenas$CADEIA)
scan.cadenas<-select(scan.cadenas,source_id,cadena_de_tiendas,CADSCAN,BANDEIRA)


# Adjuntar información de cadenas a las extracciones - Retail
retail.cadenas<-merge(sot.retail,CADEIAS,by.x="cadena_de_tiendas",by.y="CADEIA",all.x=TRUE,all.y=FALSE)
retail.cadenas<-select(retail.cadenas,source_id,cadena_de_tiendas,CADSCAN,BANDEIRA)

# Abrir MM y adjuntar informaciín de Cadenas
UNIV<-read.csv(nombre.univ)                                 # AQUI HACER CAMBIOS!!!!!!!
#univ.cadenas<-merge(UNIV,CADEIAS,by="CADEIA",all.x=TRUE)         # AQUI HACER CAMBIOS!!!!!!!
univ.cadenas<-merge(UNIV,scan.cadenas,by.x="mktr",by.y = "source_id",all.x=TRUE)
univ.cadenas<-select(univ.cadenas,mktr,CADEIA,CADSCAN,BANDEIRA,END,RAZSOC,CONDICAO,mktr,CELL_ID,CELL_NAME,area_exp)

# Generar informe
scant<-table(scan.cadenas$BANDEIRA)
     scant.nm<-names(scant)
     scant.ct<-as.integer(scant)
     scan.tot<-data.frame(BANDEIRA=scant.nm,SCAN=scant.ct)
univt<-table(univ.cadenas$BANDEIRA)
     univt.nm<-names(univt)
     univt.ct<-as.integer(univt)
     univ.tot<-data.frame(BANDEIRA=univt.nm,MM=univt.ct)
retail<-table(retail.cadenas$BANDEIRA)
     retail.nm<-names(retail)
     retail.ct<-as.integer(retail)
     retail.tot<-data.frame(BANDEIRA=retail.nm,RETAIL=retail.ct)

informe<-merge(scan.tot,univ.tot,by="BANDEIRA",all=TRUE)
informe<-merge(informe,retail.tot,by="BANDEIRA",all=TRUE)

# En caso de querer comparar tienda por tienda las de Scan en la MM
#comparacion<-merge(scan.cadenas,univ.cadenas,by.x="source_id",by.y="mktr",all.x=TRUE)
#write.csv(comparacion,"comparacion.csv",row.names = FALSE)

write.csv(informe,file = "informe2.csv")

