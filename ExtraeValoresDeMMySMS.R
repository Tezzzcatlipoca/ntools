
verif2<-function(ind,celda){
     if (ind==20) {pivot<-tot20}
     if (ind==21) {pivot<-tot21}
     if (ind==27) {pivot<-tot27}
     if (ind==32) {pivot<-tot32}
     if (ind==33) {pivot<-tot33}
     if (ind==34) {pivot<-tot34}
     if (ind==44) {
          pivot<-tot44
          pivot$CELL<-pivot$CELSORV
     }
     if (ind==51) {pivot<-tot51}
     if (ind==84) {pivot<-tot84}
     if (ind==60 | ind==61 | ind==62 | ind==64) { 
          pivot<-uniEnh15
          pivot$FATOR<-pivot$fator_fim
     }
     if (sum(grepl(names(pivot),pattern = "Cell"))>0 & sum(grepl(names(pivot),pattern = "CELL"))==0) { pivot$CELL<-pivot$Cell }
     if (sum(grepl(names(pivot),pattern = "CELL_ID"))>0) { pivot$CELL<-pivot$CELL_ID }
     if (sum(grepl(names(pivot),pattern = "cell_id"))>0) { pivot$CELL<-pivot$cell_id }
     if (sum(grepl(names(pivot),pattern = "fator"))>0) { pivot$FATOR<-pivot$fator }
     if (sum(grepl(names(pivot),pattern = "MKTR"))>0) { pivot$mktr<-pivot$MKTR }
     pivot2<-pivot[pivot$CONDICAO %in% c(1,2) & !is.na(pivot$CONDICAO),]
     sum(pivot2[pivot2$CELL==celda & !is.na(pivot2$CELL),c('FATOR')])
}

#         Sirve para devolverlos universos CONSOLIDADOS -VECTORIZADO-

verif2.vec<-function(path) {            # Función igual a la anterior, pero Vectorizada
     # Se devuelven los universos DESCONSOLIDADOS
     
     arch<-read.csv(path)
     variables<-names(arch)
     donde.ind<-variables[grepl(variables,pattern = "ind|Ind|IND")]
     donde.cell<-variables[grepl(variables,pattern = "cell_id|Cell_id|CELL_ID|cell$|CELL$|Cell$")]
     if (length(donde.ind)!=1 & length(donde.ind)!=1) {stop('No queda claro dónde están índice y celda')}
     if (length(donde.ind)!=1) {stop('No queda claro dónde está codificado el índice')}
     if (length(donde.ind)!=1) {stop('No queda claro dónde está codificada la celda')}
     observaciones<-dim(arch)[1]
     out<-rep(NA,observaciones)
     for (i in 1:observaciones) {
          ind<-as.integer(as.character(arch[i,donde.ind]))
          cell<-as.integer(as.character(arch[i,donde.cell]))
          out[i]<-verif2(ind,cell)
     }
     arch$FatorRev<-out
     # Crear nombre del archivo de salida
     where.period<-regexpr(".csv",path)[1]
     nom2<-path
     substr(nom2,where.period,where.period)<-"~"
     out.name<-gsub("~","out-Descons.",nom2)
     # Escribir archivo de salida
     write.csv(arch,file = out.name)
     print('Grabado archivo de salida con universos NO CONSOLIDADOS de MM')
}


verif2.vec.cons<-function(path) {  
     # Se devuelven los universos CONSOLIDADOS
     arch<-read.csv(path)
     variables<-names(arch)
     donde.ind<-variables[grepl(variables,pattern = "ind|Ind|IND")]
     donde.cell<-variables[grepl(variables,pattern = "cell_id|Cell_id|CELL_ID|cell$|CELL$|Cell$")]
     if (length(donde.ind)!=1 & length(donde.ind)!=1) {stop('No queda claro dónde están índice y celda')}
     if (length(donde.ind)!=1) {stop('No queda claro dónde está codificado el índice')}
     if (length(donde.ind)!=1) {stop('No queda claro dónde está codificada la celda')}
     observaciones<-dim(arch)[1]
     out<-rep(NA,observaciones)
     for (i in 1:observaciones) {
          ind<-as.integer(as.character(arch[i,donde.ind]))
          nombre.cons<-paste0("cons",ind)
          if (length(ls(pattern = nombre.cons))<1) { # Si no existe el registro genéralo
               cons.generico<-consolidados(ind)
               if (ind==19){cons19<-cons.generico}
               if (ind==20){cons20<-cons.generico}
               if (ind==21){cons21<-cons.generico}
               if (ind==27){cons27<-cons.generico}
               if (ind==31){cons31<-cons.generico}
               if (ind==32){cons32<-cons.generico}
               if (ind==33){cons33<-cons.generico}
               if (ind==34){cons34<-cons.generico}
               if (ind==44){cons44<-cons.generico}
               if (ind==51){cons51<-cons.generico}
               if (ind==84){cons84<-cons.generico}
               if (ind>59 && ind<65) {cons60<-cons.generico}
          } else {                                # Si ya existe, extráelo
               if (ind==19){cons19->cons.generico}
               if (ind==20){cons20->cons.generico}
               if (ind==21){cons21->cons.generico}
               if (ind==27){cons27->cons.generico}
               if (ind==31){cons31->cons.generico}
               if (ind==32){cons32->cons.generico}
               if (ind==33){cons33->cons.generico}
               if (ind==34){cons34->cons.generico}
               if (ind==44){cons44->cons.generico}
               if (ind==51){cons51->cons.generico}
               if (ind==84){cons84->cons.generico}
               if (ind>59 && ind<65) {cons60->cons.generico}
          }
          cell<-as.integer(as.character(arch[i,donde.cell]))
          hijas<-cons.generico[cons.generico$cell_id==cell,'consolidada']
          parientes<-c(cell,hijas)
          total<-0
          for (cada.hijo in 1:length(parientes)) {
               total<-total+verif2(ind,parientes[cada.hijo])
          }
          out[i]<-total
     }
     arch$FatorRev<-out
     # Crear nombre del archivo de salida
     where.period<-regexpr(".csv",path)[1]
     nom2<-path
     substr(nom2,where.period,where.period)<-"~"
     out.name<-gsub("~","out-Cons.",nom2)
     # Escribir archivo de salida
     write.csv(arch,file = out.name)
     print('Grabado archivo de salida con universos CONSOLIDADOS de MM')
}

# Extrae tiendas y mktrs para una celda específica en MM

verif<-function(ind,celda){
     if (ind==20) {pivot<-tot20}
     if (ind==21) {pivot<-tot21}
     if (ind==27) {pivot<-tot27}
     if (ind==32) {pivot<-tot32}
     if (ind==33) {pivot<-tot33}
     if (ind==34) {pivot<-tot34}
     if (ind==44) {
          pivot<-tot44
          pivot$CELL<-pivot$CELSORV
     }
     if (ind==51) {pivot<-tot51}
     if (ind==84) {pivot<-tot84}
     if (sum(grepl(names(pivot),pattern = "Cell"))>0 & sum(grepl(names(pivot),pattern = "CELL"))==0) { pivot$CELL<-pivot$Cell }
     if (sum(grepl(names(pivot),pattern = "fator"))>0) { pivot$FATOR<-pivot$fator }
     if (sum(grepl(names(pivot),pattern = "MKTR"))>0) { pivot$mktr<-pivot$MKTR }
     pivot[pivot$CELL==celda & !is.na(pivot$CELL),c('LOJA1','CELL','mktr','CONDICAO')]
}

# Extrae las tiendas y universos para celdas específicas en SMS

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