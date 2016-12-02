
ipm<-function(index_id,variables,args=NA){
     library(RODBC)
     smsh<-odbcConnect('smsh','nretail','nretail')
     varos<-paste0(variables,collapse = ", ")
     per<-ult.periodo(index_id)
     if (!is.na(args)) {
          linea<-paste0("SELECT ",varos," FROM index_period_market WHERE status_id IN (2) AND index_id = ",index_id, " AND period_id = ",per," AND ",args)
     } else {
          linea<-paste0("SELECT ",varos," FROM index_period_market wHERE status_id IN (2) AND index_id = ",index_id, " AND period_id = ",per)
     }
     out<-sqlQuery(smsh,linea)
     close(smsh)
     out
}

ipmcur<-function(index_id,variables,args=NA){
     library(RODBC)
     sms<-odbcConnect('sms','nretail','nretail')
     varos<-paste0(variables,collapse = ", ")
     if (!is.na(args)) {
          linea<-paste0("SELECT ",varos," FROM index_period_market WHERE status_id IN (2) AND index_id = ",index_id," AND ",args)
     } else {
          linea<-paste0("SELECT ",varos," FROM index_period_market wHERE status_id IN (2) AND index_id = ",index_id)
     }
     out<-sqlQuery(sms,linea)
     close(sms)
     out
}

ipm_raw<-function(index_id,variables,args=NA){
     library(RODBC)
     smsh<-odbcConnect('smsh','nretail','nretail')
     varos<-paste0(variables,collapse = ", ")
     #per<-ult.periodo(index_id)
     if (!is.na(args)) {
          linea<-paste0("SELECT ",varos," FROM index_period_market WHERE status_id IN (2) AND index_id = ",index_id, " AND ",args)
     } else {
          linea<-paste0("SELECT ",varos," FROM index_period_market wHERE status_id IN (2) AND index_id = ",index_id)
     }
     out<-sqlQuery(smsh,linea)
     close(smsh)
     out
}

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

ipc_raw<-function(index_id,variables,args=NA){
     library(RODBC)
     smsh<-odbcConnect('smsh','nretail','nretail')
     varos<-paste0(variables,collapse = ", ")
     #per<-ult.periodo(index_id)
     if (!is.na(args)) {
          linea<-paste0("SELECT ",varos," FROM index_period_cell WHERE status_id IN (2) AND index_id = ",index_id, " AND ",args)
     } else {
          linea<-paste0("SELECT ",varos," FROM index_period_cell wHERE status_id IN (2) AND index_id = ",index_id)
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

ips_raw<-function(index_id,variables,args=NA){
     library(RODBC)
     smsh<-odbcConnect('smsh','nretail','nretail')
     #per<-ult.periodo(index_id)
     varos<-paste0(variables,collapse = ", ")
     if (!is.na(args)) {
          linea<-paste0("SELECT ",varos," FROM index_period_source WHERE status_id IN (6,7,8,9) AND index_id = ",index_id," AND ",args)
     } else {
          linea<-paste0("SELECT ",varos," FROM index_period_source wHERE status_id IN (6,7,8,9) AND index_id = ",index_id)
     }
     out<-sqlQuery(smsh,linea)
     close(smsh)
     out
}

scan<-function(variables,args=NA){
     library(RODBC)
     smsh<-odbcConnect('scan','scanning','scanning')
     index_id<-1
     ind.periodos<-sqlQuery(smsh,"SELECT DISTINCT period_id FROM index_period_source WHERE index_id=1")
     periodos<-as.integer(as.character(unique(ind.periodos$period_id)))
     per<-max(periodos)
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

sm<-function(variables,args=NA){
     library(RODBC)
     sms<-odbcConnect('sms','nretail','nretail')
     varos<-paste0(variables,collapse = ", ")
     if (!is.na(args)) {
          linea<-paste0("SELECT ",varos," FROM source_master WHERE ",args)
     } else {
          linea<-paste0("SELECT ",varos," FROM source_master")
     }
     out<-sqlQuery(sms,linea)
     close(sms)
     out
}

mm<-function(Enh=1,variables,args=NA){
     if(Enh==1 & !exists('uniEnh15')){
          uniEnh15<<-read.csv(ultimouniv())
     } else if (Enh==0 & !exists('uni')) {
          uni<<-read.csv(ultimouniv(FALSE))          
     } else if (Enh>1 || Enh<0){
          stop('Error. El valor de Enh debe ser 0 o 1.')
     }
     
     if (Enh==1) {nome<-'uniEnh15'} else {nome<-'uni'}
     
     library(sqldf)
     varos<-paste0(variables,collapse = ", ")
     if (!is.na(args)) {
          linea<-paste0("SELECT ",varos," FROM ",nome," WHERE ",args)
     } else {
          linea<-paste0("SELECT ",varos," FROM ",nome)
     }
     out<-sqldf(linea)
     out
}
