
# Programa para extraer de Index_period_source y de Source_master las variables de la
# muestra operativa para NSOP

# Abrir archivo con source_ids
tiends<-read.csv("TiendasActivas105.csv")

vars<-c("source_id", "index_id", "period_id", "prod_id", "status_id", "source_acv", "quebra", "size", "Area", "cell_id", "loja_amostra_mestra", "Canal_Nielsen", "Canal","canal", "tipo_nielsen", "tipo", "tipo_de_tienda", "source_name", "address1", "address2", "ciudad", "nome_do_municipio", "area_nielsen", "estibge", "uf", "split", "micro_obs13", "coddist", "nome_do_distrito", "no_checkouts", "cadena_de_tiendas", "cell_id", "cell_name", "territorio_nso")

sms<-odbcConnect('sms',uid='nretail',pwd='nretail')
smsh<-odbcConnect('smsh',uid='nretail',pwd='nretail')

sm<-sqlColumns(sms,"source_master")
ips<-sqlColumns(smsh,"index_period_source")
ipc<-sqlColumns(smsh,"index_period_cell")

sm.name<-sm$COLUMN_NAME
ips.name<-ips$COLUMN_NAME
ipc.name<-ipc$COLUMN_NAME

insm<-vars[vars %in% sm.name]
inips<-vars[vars %in% ips.name]
inipc<-vars[vars %in% ipc.name]

sources<-paste0(tiends$source_id,collapse = ", ") #Nombre de la variable puede cambiar
varsips<-paste0(inips,collapse = ", ")
varssm<-paste0(insm,collapse = ", ")
varsipc<-paste0(inipc,collapse = ", ")

linea.ips<-paste0("SELECT ",varsips," FROM index_period_source WHERE source_id IN (",sources,") AND status_id IN (6,7,8,9) AND index_id = 105 AND period_id IN (SELECT max(period_id) as periodo FROM index_period_source WHERE index_id = 105)")
linea.sm<-paste0("SELECT ",varssm," FROM source_master WHERE source_id IN (",sources,")")

ext.sm<-sqlQuery(sms,query = linea.sm)
ext.ips<-sqlQuery(sms,query = linea.ips)

celdas<-paste0(ext.ips$cell_id,collapse = ", ")
linea.ipc<-paste0("SELECT ",varsipc," FROM index_period_cell WHERE status_id = 2 AND index_id = 105 AND cell_id IN (",celdas,") AND period_id IN (SELECT max(period_id) as periodo FROM index_period_cell WHERE index_id = 105)")
ext.ipc<-sqlQuery(sms,query = linea.ipc)
#ext.ipc$cell_name<-as.character(ext.ipc$cell_name)

juntas<-merge(ext.ips,ext.sm,by="source_id",all.x = TRUE)
juntas.cell<-merge(juntas,ext.ipc,by="cell_id",all.x= TRUE)


write.csv(juntas.cell,"TiendasOutNSOP-2.csv",row.names = FALSE)


