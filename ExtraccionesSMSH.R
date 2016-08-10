
library(RODBC)
smsh<-odbcConnect('SMSH',uid='nretail',pwd='nretail')
sms<-odbcConnect('SMS',uid='nretail',pwd='nretail')
variables<-sqlQuery(smsh,query="exec sp_columns [index_period_cell]")
nombres<-as.character(variables$COLUMN_NAME)
IPC<-sqlQuery(smsh,query="SELECT period_id, cell_id, cell_name, status_id, cellsot, top_estratificada FROM index_period_cell WHERE period_id > 2016004")
# Is there any CK in cell_names ?
IPC$CK<-grepl("CK",IPC$cell_name)
IPC$INDEP<-grepl("INDEP",IPC$cell_name)
IPS<-sqlQuery(smsh,query="SELECT index_id, source_id, cell_id FROM index_period_source WHERE period_id > 2016004")
source_master<-sqlQuery(sms,query="SELECT source_id, source_name, cadena_de_tiendas, scan_source_id FROM source_master")
source_cell<-merge(IPS,source_master,by="source_id",all.x=TRUE,all.y=FALSE)
juntos<-merge(IPC,source_cell,by="cell_id",all.x=TRUE, all.y = FALSE)
juntos$borrar<-is.na(juntos$scan_source_id)
juntos<-juntos[!juntos$borrar,]
cadenas<-juntos[juntos$scan_source_id>0,]
estrat<-unique(cadenas$cell_id[cadenas$top_estratificada!=0])
noestrat<-unique(cadenas$cell_id[cadenas$top_estratificada==0])

DebenSerEstrat<-IPC[IPC$cell_id %in% estrat,]
write.csv(DebenSerEstrat,file = "Estrat.csv",row.names = FALSE,quote = FALSE)

#Exportar también lista de las que debieran ser NO estratificadas y checar si coinciden


# Hacer extraccion de IPC
# Crear variable CK para los cell_name que contengan CK y/o num-num
# Cruzar con IPS y source_master para sacar scan_source_id = mktr
# Juntar tablas
# Filtrar sólo las que tienen mktr>0
# Meter en una base todas las que tienen mktr>0
# Meter en una base todas las que tienen mktr=0
# Sacar valores únicos de cell_id para cada una

# De éstas separar por top_estratificada y exportar archivo
# Las que tengan top_estratificada > 0 y CK == TRUE deben ser estratificadas, el resto no



