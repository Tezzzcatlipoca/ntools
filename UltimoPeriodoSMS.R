
#
# Programa para extraer el �ltimo periodo abierto para un �ndice espec�fico
#

ult.periodo<-function(index_id) {
     library(RODBC)
     smsh<-odbcConnect('smsh',uid='nretail',pwd='nretail')
     quer.per<-paste0("SELECT period_id, index_id FROM index_period_source WHERE status_id = 2 AND index_id = ",index_id)
     ind.periodos<-sqlQuery(smsh,quer.per)
     periodos<-as.integer(as.character(unique(ind.periodos$period_id)))
     period_id<-max(periodos)
     period_id
     
} # End of function