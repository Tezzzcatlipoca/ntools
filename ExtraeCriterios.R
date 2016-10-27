
#
#         Programa para extraer los criterios de cada mercado
#

#mercados<-c("CDD.MOOCA","CDD.DIAD","DIASUL.IN")
tag_name<-NA
mbd_id<-1554
indice<-51

# Cómo llamar a la función
criterio(51,1554) # Por mbd_id
criterio(51,tag_name = 'CDD.MOOCA') # Por tag_name

# Función
criterio<-function(indice=NA,mbd_id=NA,tag_name=NA){
     
library(RODBC)
smsh<-odbcConnect("smsh",uid="nretail",pwd="nretail")
if(is.na(mbd_id) & is.na(tag_name)) {stop('Necesario proveer mbd_id o tag_name')}
if(is.na(mbd_id)) {
     quer1<-paste0("SELECT period_id, index_id, mbd_id, tag_name, icriteria_id FROM index_period_market WHERE index_id = ",indice," AND tag_name = '",tag_name,"'")
} else {
     quer1<-paste0("SELECT period_id, index_id, mbd_id, tag_name, icriteria_id FROM index_period_market WHERE index_id = ",indice," AND mbd_id = '",mbd_id,"'")
}
#quer1<-paste0("SELECT period_id, index_id, mbd_id, tag_name, icriteria_id FROM index_period_market WHERE tag_name = '",mercado, "'")

mbds<-sqlQuery(smsh,query = quer1)
# Los periodos nuevos primero
mbds<-mbds[order(mbds$period_id,decreasing = TRUE),]
icriteria_id<-mbds$icriteria_id[1] # Traduce TAG_NAME a icriteria_id

if(length(icriteria_id)<1){stop('No existe el mercado')}
close(smsh)

# Extraer criterios
sms<-odbcConnect('sms',uid='nretail',pwd = 'nretail')
quer2<-paste0("SELECT icriteria_id, detail_seq, entity_id, char_id, char_name, operator_id, value_id, value, logical_operator, xlevel, operator_not FROM index_criteria_detail WHERE icriteria_id = ",icriteria_id)
quer3<-paste0("SELECT icriteria_id, detail_seq, value_seq, value_id, value FROM index_criteria_detail_val WHERE icriteria_id = ", icriteria_id)
criterios<-sqlQuery(sms,query = quer2)
valores<-sqlQuery(sms,query = quer3)
close(sms)

# función para traducir criterios
opz<-function(crit){
     crit1<-data.frame(num=crit,id=seq(length(crit)))
     defs<-data.frame(num=c(0,1,2,3,4,5,6,7,8,9,10,11),op=c('','= ~','<> ~','< ~','> ~','(~)','>= ~','<= ~','AND ~','OR ~','NOT IN (~)','IN (~)'))
     crit2<-merge(crit1,defs,by='num',all.x=TRUE)
     as.character(crit2$op[order(crit2$id)])
}

# Ordenar detalles
criterios<-criterios[order(criterios$detail_seq),]

# Ordenar valores de index_criteria_detail_val
criterios$value<-as.character(criterios$value)
detalle_id<-unique(as.character(valores$detail_seq))
cuantos<-length(detalle_id)
for(i in 1:cuantos){
     det_actual<-detalle_id[i]
     val.table<-valores[as.character(valores$detail_seq)==det_actual,]
     val.table<-val.table[order(val.table$value_seq),]
     vals<-paste0(val.table$value,collapse = ",")
     criterios$value[criterios$detail_seq==det_actual]<-vals
}

# Traducir operadores
criterios$operator<-opz(criterios$operator_id)
criterios$logical<-sub("~","",opz(criterios$logical_operator))

# Calcular paréntesis
cuenta<-dim(criterios)[1]
criterios$parent.ant<-''
criterios$parent.des<-''
for(i in 1:cuenta) {
     if(i==1){
          criterios$parent.ant[i]<-paste0(rep("(",criterios$xlevel[i]),collapse="")
     } else {
          out<-criterios$xlevel[i]-criterios$xlevel[i-1]
          if (out>0) {
               criterios$parent.ant[i]<-paste0(rep("(",out),collapse = "")
          } 
          if (out<0) {
               criterios$parent.des[i]<-paste0(rep(")",abs(out)),collapse = "")
          }
     }
     
}

criterios$string<-paste(criterios$parent.ant,criterios$char_name,criterios$operator,criterios$parent.des,criterios$logical)

for (w in 1:cuenta) {
     criterios$string[w]<-sub("~",criterios$value[w],criterios$string[w])
}

linea.completa<-paste0(criterios$string,collapse = " ")
linea.completa<-gsub("[ ]{2,}"," ",linea.completa) # Eliminar espacios extra
linea.completa
}

# Función vectorizada (basada en la función anterior)

varios_criterios<-function(vector_m){
if(dim(vector_m)[1]<1){stop('El vector no tiene observaciones.')}
if(dim(vector_m)[2]<3){stop('El vector no tiene variables necesarias (índice, mbd_id, tag_name). Variables ausentes pueden llenarse con NAs.')}
if(class(vector_m[1])!='integer'){stop('El índice debe ser primero y debe ser tipo integer.')}
if(class(vector_m[2])!='integer'){stop('El mbd_id debe ser segundo y debe ser tipo integer.')}
if(class(vector_m[3])!='character'){stop('El tag_name debe ser tercero y debe ser tipo character.')}
               
out_m<-c()
     for (i in 1:(dim(vector_m)[1])) {
          out_m<-c(out_m,criterio(vector_m[i,1],vector_m[i,2],vector_m[i,3]))
     }
out_m
}
