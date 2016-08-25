
verif<-function(ind,celda){
     if (ind==20) {pivot<-tot20}
     if (ind==21) {pivot<-tot21}
     if (ind==27) {pivot<-tot27}
     if (ind==32) {pivot<-tot32}
     if (ind==33) {pivot<-tot33}
     if (ind==44) {
          pivot<-tot44
          pivot$CELL<-pivot$CELSORV
     }
     if (ind==51) {pivot<-tot51}
     if (ind==84) {pivot<-tot84}
     
     sum(pivot[pivot$CELL==celda,'FATOR'])
}

versms<-function(ind,celda){
     smsh<-odbcConnect('smsh',uid='nretail',pwd='nretail')
     tempo<-sqlQuery(smsh,query = paste("SELECT period_id FROM index_period_cell WHERE index_id = ", ind ," AND status_id =2",sep=""))
     unicas<-unique(tempo$period_id)
     ult<-max(unicas)
     quero<-paste("SELECT index_id, cell_id, universe_source, ideal_source FROM index_period_cell WHERE index_id = ", ind, " AND cell_id = ", celda, " AND period_id = ", ult ," AND status_id = 2",sep="")
     extrac<-sqlQuery(smsh,query = quero)
     close(smsh)
     extrac
}