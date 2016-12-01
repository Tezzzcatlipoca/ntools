
# Programa para saber qué pasó con las tiendas de una celda (cómo están relacionadas)

ind<-27
celda<-5416
origen_MM<-TRUE
periodo<-ult.periodo(27)

compara_celdas<-function(ind,celda,periodo,origen_MM=TRUE){

IEnh<-c(60,61,62,63,64)
if (length(ind %in% IEnh)>0) {
     tot<-read.csv(ultimouniv())
     Enh<-TRUE
} else {
     tot<-read.csv(ultimotot(ind))
     Enh<-FALSE
}
names(tot)<-tolower(names(tot)) # Hace más fácil encontrar las variables

library(RODBC)
if (origen_MM) {
     # MM por celda
     observadas<-tot[tot$cell==celda & !is.na(tot$cell),c('loja1','fator','mktr','condicao')]
     mktrs<-paste0(observadas$mktr[!is.na(observadas$mktr)],collapse = ",")
     # SMS-Scan por mktr (de MM)
     linea<-paste0("SELECT source_id AS mktr, source_id AS scan_source_id, cod_amostra_mestra FROM source_master WHERE source_id IN (",mktrs,")")
     scan<-odbcConnect('scan','scanning','scanning')    # ODBC
     obs_scan<-sqlQuery(scan,linea)
     # SMS-Retail por celda
     smsh<-odbcConnect('smsh','nretail','nretail')      # ODBC
     linea2<-paste0("SELECT source_id, status_id AS retail_status_id FROM index_period_source WHERE cell_id IN (",celda,") AND index_id = ",ind," AND period_id = ",periodo)
     obs_ips<-sqlQuery(smsh,linea2)
     sources<-paste0(obs_ips$source_id,collapse = ",")
     linea3<-paste0("SELECT source_id, loja_amostra_mestra AS loja1 FROM source_master WHERE source_id IN (",sources,")")
     sms<-odbcConnect('sms','nretail','nretail')      # ODBC
     obs_retail<-sqlQuery(sms,linea3)
     # Juntar todas las tablas en una sola
     # primero por loja1 luego por mktr
     MM_Retail<-merge(observadas,obs_retail,by="loja1",all=TRUE)
     if (is.null(dim(obs_scan))) {
          obs_scan<-data.frame(mktr=NA,scan_source_id=NA,cod_amostra_mestra=NA)
     }
     todas<-merge(MM_Retail,obs_scan,by="mktr",all=TRUE)
} else {
     # MM
     observadas<-tot[tot$cell==celda & !is.na(tot$cell),c('loja1','fator','mktr','condicao')]
     mktrs<-paste0(observadas$mktr[!is.na(observadas$mktr)],collapse = ",")
     # SMS-Retail por celda
     linea2<-paste0("SELECT source_id, status_id AS retail_status_id FROM index_period_source WHERE cell_id IN (",celda,") AND index_id = ",ind," AND period_id = ",periodo)
     smsh<-odbcConnect('smsh','nretail','nretail')      # ODBC
     obs_ips<-sqlQuery(smsh,linea2)
     sources<-paste0(obs_ips$source_id,collapse = ",")
     linea3<-paste0("SELECT source_id, loja_amostra_mestra AS loja1 FROM source_master WHERE source_id IN (",sources,")")
     sms<-odbcConnect('sms','nretail','nretail')      # ODBC
     obs_retail<-sqlQuery(sms,linea3)
     # SMS-Scan por mktr (de MM)
     linea<-paste0("SELECT source_id AS scan_source_id, source_id AS mktr, cod_amostra_mestra FROM source_master WHERE source_id IN (",mktrs,")")
     scan_id<-paste0(linea$scan_source_id,collapse = ",")
     scan<-odbcConnect('scan','scanning','scanning')     # ODBC
     obs_scan<-sqlQuery(scan,linea)
     # Juntar todas las tablas en una sola
     # primero por loja1 luego por mktr    
     MM_Retail<-merge(observadas,obs_retail,by="loja1",all=TRUE)
     if (is.null(dim(obs_scan))) {
          obs_scan<-data.frame(mktr=NA,scan_source_id=NA,cod_amostra_mestra=NA)
     }
     todas<-merge(MM_Retail,obs_scan,by="mktr",all=TRUE)
}

todas  # Devuelve la tabla con las tiendas encontradas en la celda

} # Fin de la función
