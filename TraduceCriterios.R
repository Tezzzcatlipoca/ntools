#
#         Programa para TRADUCIR los criterios de cada mercado
#

#mercados<-c("CDD.MOOCA","CDD.DIAD","DIASUL.IN")
tag_name<-NA #"DIA.AR4"
mbd_id<-1261
indice<-51

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

# Traducir variables de SMS a variables geográficas
# Abre archivo guía: Cuáles variables deben traducirse
trads<-read.csv("C:/Users/franro04/Documents/R Code/char_traducibles.csv")
traducibles<-unique(trads$char_id[trads$code==1])
criterios$traducible<-0
criterios$traducible[criterios$char_id %in% traducibles]<-1
directos<-unique(trads$char_id[trads$code==2])
criterios$traducible[criterios$char_id %in% directos]<-2

# Extrae traducciones de caracter
linea<-paste0("SELECT * FROM index_period_char_values WHERE index_id = ",indice," AND icriteria_id > 0")
#linea<-paste0("SELECT * FROM index_period_char_values WHERE index_id = ",index_id)
smsh<-odbcConnect('smsh',uid='nretail',pwd='nretail')
ipch<-sqlQuery(smsh,query=linea)
close(smsh)

# Generar llave para saber cuales valores están repetidos
ipch$key<-paste0(ipch$char_id,"-",ipch$value_id)

library(plyr)
ipch<-arrange(ipch,desc(period_id))
dupl<-duplicated(ipch$key)
ipch.limpia<-ipch[!dupl,]


# Agregar nuevos icriteria_id donde sean necesarios
criterios$icrit2<-'0'
criterios$icrit2[criterios$traducible==1]

chars<-criterios$char_id[criterios$traducible==1]
vals<-criterios$value_id[criterios$traducible==1]
key<-paste0(chars,"-",vals)

#ipch$key<-paste0(ipch$char_id,"-",ipch$value_id) # Repetido
crits.a.usar<-ipch[ipch$key %in% key,]
crits.orden<-crits.a.usar[match(crits.a.usar$key,key),]

# Hay caracteres identificados como traducibles que no aparecen en la extracción
# de index_period_characteristics y que por tanto no se pueden traducir. Ej: 126,


criterios$string<-paste(criterios$parent.ant,criterios$char_name,criterios$operator,criterios$parent.des,criterios$logical)

for (w in 1:cuenta) {
     criterios$string[w]<-sub("~",criterios$value[w],criterios$string[w])
}

linea.completa<-paste0(criterios$string,collapse = " ")
linea.completa<-gsub("[ ]{2,}"," ",linea.completa) # Eliminar espacios extra
linea.completa
