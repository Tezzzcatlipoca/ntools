
# Extrae celdas de cadena de SMS-Scan [Periodo 1]
# Extrae celdas de cadena de MM [Generado por PED]
# Extrae celdas de cadena de SMS-Retail [Periodo 2]

periodos_scan<-c()
periodo_retail<-2016018

library(RODBC)
scan<-odbcConnect('scan',uid='scanning',pwd='scanning')
quero.scan<-paste("SELECT source_id, cadena_de_tiendas FROM source_master WHERE period_id IN (",periodos_scan,") ",sep="")
extra.scan<-sqlQuery(scan,query = quero.scan)

smsh<-odbcConnect('smsh',uid='nretail',pwd='nretail')
quero.retail<-paste("SELECT source_id, cadena_de_tiendas FROM source_master WHERE period_id IN (",periodo_retail,") ",sep="")
extra.retail<-sqlQuery(smsh,quero.retail)

library(xlsx)
CADEIAS<-read.xlsx("J:/ESTATOC/DATA/CADEIAS.xls",sheetName = "RELACAO CADEIAS")



