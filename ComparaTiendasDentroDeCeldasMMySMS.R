
#
# Este programa identifica qué tiendas de SMS que no están en la MM para alguna celda
#         Esto es útil para encontrar tiendas de cadena (sot) que aun no han sido
#         agregadas a la muestra maestra
#


library(RODBC)
input<-read.csv("RevisarFuera.csv")

largo<-dim(input)[1]
for (i in 1:largo) {
     
     ind<-as.integer(as.character(input$Indice[i]))
     cell<-as.integer(as.character(input$Cell_id[i]))
     sms<-odbcConnect('sms',uid='nretail',pwd='nretail')
     smsh<-odbcConnect('smsh',uid='nretail',pwd='nretail')
     # Sacar últimpo periodo
     n.ult<-paste("SELECT period_id FROM index_period_source WHERE index_id = ",ind, " AND status_id = 2")   
     ult<-sqlQuery(smsh,query = n.ult)
     unic<-unique(ult)
     periodo<-max(unic)
     # Extraer source_ids
     n.quero<-paste("SELECT source_id, cell_id FROM index_period_source WHERE index_id = ",ind," AND period_id = ",periodo," AND cell_id = ",cell, " AND status_id IN (6,7,8,9)" , sep="")
     quero<-sqlQuery(smsh,query = n.quero)
     celdas<-paste(quero$source_id, collapse = ",")
     
     # Extraer mktrs
     n.quero2<-paste("SELECT source_id, scan_source_id FROM source_master WHERE source_id IN (",celdas,")"  ,sep="")
     SMS<-sqlQuery(sms,query = n.quero2)
     close(sms)
     close(smsh)
     
     # Extraer tiendas de MM
     MM<-verif(ind,cell)
     
     # Identificar diferencias entre ambos
     juntos<-merge(MM,SMS,by.x="mktr",by.y="scan_source_id",all.x=TRUE)
     outter<-juntos[is.na(juntos$source_id),c('mktr','LOJA1')]
     lista<-paste(outter$mktr,collapse = ";")
     lojas<-paste(outter$LOJA1,collapse = ";")
     input$noCoinc[i]<-lista
     input$lojas[i]<-lojas
}

