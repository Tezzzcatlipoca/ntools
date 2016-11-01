
NEW_TOT<-'REFRI2015_NOENH_oct.csv'
OLD_TOT<-'REFRI2015_noenh_jun.csv'
NEW_LIB<-'H:/ESTATIST/PROCESSOS E CLIENTES/Migração Universos entre IVPO-OE E MSci/2015/Ind 51/Octubre16'
OLD_LIB<-'H:/ESTATIST/PROCESSOS E CLIENTES/Migração Universos entre IVPO-OE E MSci/2015/Ind 51/Julio16'
ACT_CAD<-'J:/CENSOOBS/DATA/1CICLO/CENSO2015/Actualización de Cadenas/Octubre'
PERIOD<-2016021
index<-51
ruta<-'2015/Ind 51/Octubre16'
NEW<-NEW_LIB # Libreria
OLD<-OLD_LIB # Libreria

cons<-consolidados(index) # Cargar función primero

cuenta_cons<-function(list_celdas){
     outq<-c()
     for (q in 1:length(list_celdas)) {
          celda_prov<-list_celdas[q]
          cuanto_cez<-dim(cons[cons$cell_id==celda_prov,])[1] # Cuantos cezinhos
          outq<-c(outq,cuanto_cez)
     }
     outq
}

library(dplyr)

nombre_NTOT<-paste0(NEW,"/",NEW_TOT)
NTOT<-read.csv(nombre_NTOT) 
names(NTOT)<-tolower(names(NTOT))
NTOT<-NTOT[NTOT$condicao %in% c(1,2) & !is.na(NTOT$condicao),]
ZCELDA<-select(NTOT,cell, loja1, mktr, area, estado, munic, tipo, cadeia, fator)
nombre_include<-paste0(ACT_CAD,"/tiendas_nuevas_R.txt")
registros<-read.csv(nombre_include)
cuales<-registros[,1]
ZCELDA<-ZCELDA[ZCELDA$mktr %in% cuales & !is.na(ZCELDA$mktr),]


nombre_OLDTOT<-paste0(OLD,"/",OLD_TOT)
OTOT<-read.csv(nombre_OLDTOT)
names(OTOT)<-tolower(names(OTOT))
OTOT<-OTOT[OTOT$condicao %in% c(1,2) & !is.na(OTOT$condicao),]
ZCELDA2<-select(OTOT, cell, loja1, mktr, area, estado, munic, tipo, cadeia, fator)
nombre_include<-paste0(ACT_CAD,"/tiendas_inactivas_R.txt")
registros<-read.csv(nombre_include)
cuales<-registros[,1]
ZCELDA2<-ZCELDA2[ZCELDA2$mktr %in% cuales & !is.na(ZCELDA2$mktr),]

find_mktr<-function(celda){
     extracto<-NTOT[NTOT$cell==celda & !is.na(NTOT$cell),'mktr']
     extracto
}

vec_mktr<-function(vec_celda){
     outt<-list()
     for (i in 1:length(vec_celda)) {
          provisional<-find_mktr(vec_celda[i])
          if (length(provisional)==0) {
               outt[[i]]<-0
          } else {
               outt[[i]]<-provisional
          }
     }
     outt
}

celdas<-rbind(ZCELDA,ZCELDA2)
celdas<-select(celdas,cell)

#library(sqldf)
NCELDA<-xtabs(fator~cell,data=NTOT) # Array, cells as names
NCELDA_t<-data.frame(cell=as.integer(as.character(names(NCELDA))),N_TOT=as.integer(NCELDA))

library(RODBC)
SMSH<-odbcConnect('smsh','nretail','nretail')
SMSSH<-odbcConnect('scan','scanning','scanning')

query1<-paste0("SELECT cell_id as cell, cell_name, period_id, index_id, status_id, universe_source, ideal_source, sample_source FROM index_period_cell WHERE period_id IN (SELECT max(period_id) FROM index_period_cell)")
scancell<-sqlQuery(SMSSH,query1)

query3<-paste0("SELECT source_id, period_id, index_id, status_id FROM index_period_source WHERE status_id IN (7,8) AND period_id IN (SELECT max(period_id) FROM index_period_cell)")
scantiendas<-sqlQuery(SMSSH,query3)

query2<-paste0("SELECT cell_id as cell, cell_name, period_id, index_id, status_id, universe_source, ideal_source, sample_source FROM index_period_cell WHERE period_id = ",PERIOD," AND index_id= ",index)
zcell<-sqlQuery(SMSH,query2)

zcell<-dplyr::inner_join(zcell, celdas, by="cell")

cuenta_en_scan<-function(lista){ # Función vectorizada
     outp<-c()
     for (w in 1:length(lista)) {
          cuenta<-length(scantiendas$source_id[scantiendas$source_id %in% lista[[w]]])
          outp<-c(outp,cuenta)
     }
     outp
}

zcelda_new<-xtabs(fator~cell,data=ZCELDA)
zcelda_old<-xtabs(fator~cell,data=ZCELDA2)
zcelda_new_t<-data.frame(cell=as.integer(as.character(names(zcelda_new))),sumar=as.integer(zcelda_new))
zcelda_old_t<-data.frame(cell=as.integer(as.character(names(zcelda_old))),restar=as.integer(zcelda_old))

#zcelda_new<-zcelda_new[,-c('_type_','_freq_')]
#zcelda_old<-zcelda_old[,-c('_type_','_freq_')]

mktrs<-vec_mktr(NCELDA_t$cell) # Obtiene de MM lista de mktrs para cada celda
NCELDA_t$N_Scan<-cuenta_en_scan(mktrs)   # Obtiene N de tiendas que están activas en Scan
NCELDA_t$Cuanta_consolidada<-cuenta_cons(NCELDA_t$cell) # Obtiene Número de celdas consolidadas en la celda

final<-join_all(list(zcell, zcelda_new_t, zcelda_old_t, NCELDA_t), by="cell")
#final<-merge(zcell, zcelda_new, zcelda_old, celdas, by="cell") # Left join

final$sumar[is.na(final$sumar)]<-0
final$restar[is.na(final$restar)]<-0
final$universo<-final$universe_source+final$sumar-final$restar
final$ideal<-final$universo
final$Igual<-0


outfile<-paste0("H:/ESTATIST/PROCESSOS E CLIENTES/Migração Universos entre IVPO-OE E MSci/",ruta,"/Cambio de universos_ME51_vR.csv")
write.csv(final, outfile)




