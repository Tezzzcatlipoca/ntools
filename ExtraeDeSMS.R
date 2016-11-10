
ipccur<-function(index_id,variables,args=NA){
     library(RODBC)
     sms<-odbcConnect('sms','nretail','nretail')
     varos<-paste0(variables,collapse = ", ")
     if (!is.na(args)) {
          linea<-paste0("SELECT ",varos," FROM index_period_cell WHERE status_id IN (2) AND index_id = ",index_id," AND ",args)
     } else {
          linea<-paste0("SELECT ",varos," FROM index_period_cell wHERE status_id IN (2) AND index_id = ",index_id)
     }
     out<-sqlQuery(sms,linea)
     close(sms)
     out
}

ipscur<-function(index_id,variables,args=NA){
     library(RODBC)
     sms<-odbcConnect('sms','nretail','nretail')
     varos<-paste0(variables,collapse = ", ")
     if (!is.na(args)) {
          linea<-paste0("SELECT ",varos," FROM index_period_source WHERE status_id IN (6,7,8,9) AND index_id = ",index_id," AND ",args)
     } else {
          linea<-paste0("SELECT ",varos," FROM index_period_source wHERE status_id IN (6,7,8,9) AND index_id = ",index_id)
     }
     out<-sqlQuery(sms,linea)
     close(sms)
     out
}

ipc<-function(index_id,variables,args=NA){
     library(RODBC)
     smsh<-odbcConnect('smsh','nretail','nretail')
     varos<-paste0(variables,collapse = ", ")
     per<-ult.periodo(index_id)
     if (!is.na(args)) {
          linea<-paste0("SELECT ",varos," FROM index_period_cell WHERE status_id IN (2) AND index_id = ",index_id, " AND period_id = ",per," AND ",args)
     } else {
          linea<-paste0("SELECT ",varos," FROM index_period_cell wHERE status_id IN (2) AND index_id = ",index_id, " AND period_id = ",per)
     }
     out<-sqlQuery(smsh,linea)
     close(smsh)
     out
}

ips<-function(index_id,variables,args=NA){
     library(RODBC)
     smsh<-odbcConnect('smsh','nretail','nretail')
     per<-ult.periodo(index_id)
     varos<-paste0(variables,collapse = ", ")
     if (!is.na(args)) {
          linea<-paste0("SELECT ",varos," FROM index_period_source WHERE status_id IN (6,7,8,9) AND index_id = ",index_id," AND period_id = ",per," AND ",args)
     } else {
          linea<-paste0("SELECT ",varos," FROM index_period_source wHERE status_id IN (6,7,8,9) AND index_id = ",index_id," AND period_id = ",per)
     }
     out<-sqlQuery(smsh,linea)
     close(smsh)
     out
}