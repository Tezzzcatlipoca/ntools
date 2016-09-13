
#
#         Este programa identifica las consolidaciones de celdas en SMS 
#         para un índice específico en el último periodo disponible
#

consolidados<-function(index_id,graba=0) { 
     
     # Si graba=0 entonces los valores son devueltos a la línea de comandos como tabla
     # Si graba=1 entonces los valores son grabados en un archivo y NO en la línea
     #    de comando.
     
library(RODBC)
smsh<-odbcConnect('SMSH', uid='nretail', pwd = 'nretail')
quer.per<-paste0("SELECT period_id, index_id FROM index_period_source WHERE status_id = 2 AND index_id = ",index_id)
ind.periodos<-sqlQuery(smsh,quer.per)
periodos<-as.integer(as.character(unique(ind.periodos$period_id)))
period_id<-max(periodos)
quer<-paste("SELECT ","cell_id, index_id, period_id, cell_name, universe_source, ideal_source, status_id, sample_source, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16"," FROM index_period_cell WHERE period_id = ",period_id," AND index_id = ", index_id, sep="")
cells<-sqlQuery(smsh,query=quer)
odbcClose(smsh)

if (dim(cells)[1]==0) {
     stop('El índice no contiene datos en ningún periodo.')
}

#C1
try({
     extr1<-cells[cells$c1>0 & cells$status_id==2,]
     extr1$consolidada<-extr1$c1
     extr1$cezinho<-'c1'
     extr1<-extr1[,c('cell_id','cell_name','consolidada','cezinho')]
},silent = TRUE)

#C2
try({
     extr2<-cells[cells$c2>0 & cells$status_id==2,]
     extr2$consolidada<-extr2$c2
     extr2$cezinho<-'c2'
     extr2<-extr2[,c('cell_id','cell_name','consolidada','cezinho')]
},silent = TRUE)

#C3
try({
     extr3<-cells[cells$c3>0 & cells$status_id==2,]
     extr3$consolidada<-extr3$c3
     extr3$cezinho<-'c3'
     extr3<-extr3[,c('cell_id','cell_name','consolidada','cezinho')]
},silent = TRUE)

#C4
try({
     extr4<-cells[cells$c4>0 & cells$status_id==2,]
     extr4$consolidada<-extr4$c4
     extr4$cezinho<-'c4'
     extr4<-extr4[,c('cell_id','cell_name','consolidada','cezinho')]
},silent = TRUE)

#C5
try({
     extr5<-cells[cells$c5>0 & cells$status_id==2,]
     extr5$consolidada<-extr5$c5
     extr5$cezinho<-'c5'
     extr5<-extr5[,c('cell_id','cell_name','consolidada','cezinho')]
},silent = TRUE)

#C6
try({
     extr6<-cells[cells$c6>0 & cells$status_id==2,]
     extr6$consolidada<-extr6$c6
     extr6$cezinho<-'c6'
     extr6<-extr6[,c('cell_id','cell_name','consolidada','cezinho')]
},silent = TRUE)

#C7
try({
     extr7<-cells[cells$c7>0 & cells$status_id==2,]
     extr7$consolidada<-extr7$c7
     extr7$cezinho<-'c7'
     extr7<-extr7[,c('cell_id','cell_name','consolidada','cezinho')]
},silent = TRUE)

#C8
try({
     extr8<-cells[cells$c8>0 & cells$status_id==2,]
     extr8$consolidada<-extr8$c8
     extr8$cezinho<-'c8'
     extr8<-extr8[,c('cell_id','cell_name','consolidada','cezinho')]
},silent = TRUE)

#C9
try({
     extr9<-cells[cells$c9>0 & cells$status_id==2,]
     extr9$consolidada<-extr9$c9
     extr9$cezinho<-'c9'
     extr9<-extr9[,c('cell_id','cell_name','consolidada','cezinho')]
},silent = TRUE)

#C10
try({
     extr10<-cells[cells$c10>0 & cells$status_id==2,]
     extr10$consolidada<-extr10$c10
     extr10$cezinho<-'c10'
     extr10<-extr10[,c('cell_id','cell_name','consolidada','cezinho')]
},silent = TRUE)

#C11
try({
     extr11<-cells[cells$c11>0 & cells$status_id==2,]
     extr11$consolidada<-extr11$c11
     extr11$cezinho<-'c11'
     extr11<-extr11[,c('cell_id','cell_name','consolidada','cezinho')]
},silent = TRUE)

#C12
try({
     extr12<-cells[cells$c12>0 & cells$status_id==2,]
     extr12$consolidada<-extr12$c12
     extr12$cezinho<-'c12'
     extr12<-extr12[,c('cell_id','cell_name','consolidada','cezinho')]
},silent = TRUE)

#C13
try({
     extr13<-cells[cells$c13>0 & cells$status_id==2,]
     extr13$consolidada<-extr13$c13
     extr13$cezinho<-'c13'
     extr13<-extr13[,c('cell_id','cell_name','consolidada','cezinho')]
},silent = TRUE)

#C14
try({
     extr14<-cells[cells$c14>0 & cells$status_id==2,]
     extr14$consolidada<-extr14$c14
     extr14$cezinho<-'c14'
     extr14<-extr14[,c('cell_id','cell_name','consolidada','cezinho')]
},silent = TRUE)

#C15
try({
     extr15<-cells[cells$c15>0 & cells$status_id==2,]
     extr15$consolidada<-extr15$c15
     extr15$cezinho<-'c15'
     extr15<-extr15[,c('cell_id','cell_name','consolidada','cezinho')]
},silent = TRUE)

#C16
try({
     extr16<-cells[cells$c16>0 & cells$status_id==2,]
     extr16$consolidada<-extr16$c16
     extr16$cezinho<-'c16'
     extr16<-extr16[,c('cell_id','cell_name','consolidada','cezinho')]
},silent = TRUE)

consolida<-rbind(extr1, extr2, extr3, extr4, extr5, extr6, extr7, extr8, extr9, extr10, extr11, extr12, extr13, extr14, extr15, extr16)

consolida<-consolida[order(consolida$consolidada),]
consolida<-consolida[order(consolida$cell_id),]
consolida<-consolida[!is.na(consolida$cell_id),]

if (graba==1) {
     nombre<-paste0("Celdas_",index_id,"_",period_id,".csv")
     write.csv(consolida, file=nombre, quote=FALSE, row.names = FALSE)
} else {
     consolida
}

} # End of function

