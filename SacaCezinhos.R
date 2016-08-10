

sacaCezinhos<-function (period_id,index_id,grabatodos=TRUE,busca=0) {

library(RODBC)
#period_id<-2016018
#index_id<-21

# Leer SMS
smsh<-odbcConnect('SMSH', uid='nretail', pwd = 'nretail')
quer<-paste("SELECT ","cell_id, index_id, period_id, cell_name, universe_source, ideal_source, status_id, sample_source, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16"," FROM index_period_cell WHERE period_id = ",period_id," AND index_id = ", index_id, sep="")
cells<-sqlQuery(smsh,query=quer)
odbcClose(smsh)

if (dim(cells)[1]==0) {
     stop('El índice no contiene datos para este periodo.')
}

#C1
try({
     extr1<-cells[cells$c1>0 & cells$status_id==2,]
     extr1$Consolidada<-extr1$c1
     extr1$Cezinho<-'c1'
     extr1<-extr1[,c('cell_id','cell_name','Consolidada','Cezinho')]
},silent = TRUE)

#C2
try({
     extr2<-cells[cells$c2>0 & cells$status_id==2,]
     extr2$Consolidada<-extr2$c2
     extr2$Cezinho<-'c2'
     extr2<-extr2[,c('cell_id','cell_name','Consolidada','Cezinho')]
},silent = TRUE)

#C3
try({
     extr3<-cells[cells$c3>0 & cells$status_id==2,]
     extr3$Consolidada<-extr3$c3
     extr3$Cezinho<-'c3'
     extr3<-extr3[,c('cell_id','cell_name','Consolidada','Cezinho')]
},silent = TRUE)

#C4
try({
     extr4<-cells[cells$c4>0 & cells$status_id==2,]
     extr4$Consolidada<-extr4$c4
     extr4$Cezinho<-'c4'
     extr4<-extr4[,c('cell_id','cell_name','Consolidada','Cezinho')]
},silent = TRUE)

#C5
try({
     extr5<-cells[cells$c5>0 & cells$status_id==2,]
     extr5$Consolidada<-extr5$c5
     extr5$Cezinho<-'c5'
     extr5<-extr5[,c('cell_id','cell_name','Consolidada','Cezinho')]
},silent = TRUE)

#C6
try({
     extr6<-cells[cells$c6>0 & cells$status_id==2,]
     extr6$Consolidada<-extr6$c6
     extr6$Cezinho<-'c6'
     extr6<-extr6[,c('cell_id','cell_name','Consolidada','Cezinho')]
},silent = TRUE)

#C7
try({
     extr7<-cells[cells$c7>0 & cells$status_id==2,]
     extr7$Consolidada<-extr7$c7
     extr7$Cezinho<-'c7'
     extr7<-extr7[,c('cell_id','cell_name','Consolidada','Cezinho')]
},silent = TRUE)

#C8
try({
     extr8<-cells[cells$c8>0 & cells$status_id==2,]
     extr8$Consolidada<-extr8$c8
     extr8$Cezinho<-'c8'
     extr8<-extr8[,c('cell_id','cell_name','Consolidada','Cezinho')]
},silent = TRUE)

#C9
try({
     extr9<-cells[cells$c9>0 & cells$status_id==2,]
     extr9$Consolidada<-extr9$c9
     extr9$Cezinho<-'c9'
     extr9<-extr9[,c('cell_id','cell_name','Consolidada','Cezinho')]
},silent = TRUE)

#C10
try({
     extr10<-cells[cells$c10>0 & cells$status_id==2,]
     extr10$Consolidada<-extr10$c10
     extr10$Cezinho<-'c10'
     extr10<-extr10[,c('cell_id','cell_name','Consolidada','Cezinho')]
},silent = TRUE)

#C11
try({
     extr11<-cells[cells$c11>0 & cells$status_id==2,]
     extr11$Consolidada<-extr11$c11
     extr11$Cezinho<-'c11'
     extr11<-extr11[,c('cell_id','cell_name','Consolidada','Cezinho')]
},silent = TRUE)

#C12
try({
     extr12<-cells[cells$c12>0 & cells$status_id==2,]
     extr12$Consolidada<-extr12$c12
     extr12$Cezinho<-'c12'
     extr12<-extr12[,c('cell_id','cell_name','Consolidada','Cezinho')]
},silent = TRUE)

#C13
try({
     extr13<-cells[cells$c13>0 & cells$status_id==2,]
     extr13$Consolidada<-extr13$c13
     extr13$Cezinho<-'c13'
     extr13<-extr13[,c('cell_id','cell_name','Consolidada','Cezinho')]
},silent = TRUE)

#C14
try({
     extr14<-cells[cells$c14>0 & cells$status_id==2,]
     extr14$Consolidada<-extr14$c14
     extr14$Cezinho<-'c14'
     extr14<-extr14[,c('cell_id','cell_name','Consolidada','Cezinho')]
},silent = TRUE)

#C15
try({
     extr15<-cells[cells$c15>0 & cells$status_id==2,]
     extr15$Consolidada<-extr15$c15
     extr15$Cezinho<-'c15'
     extr15<-extr15[,c('cell_id','cell_name','Consolidada','Cezinho')]
},silent = TRUE)

#C16
try({
     extr16<-cells[cells$c16>0 & cells$status_id==2,]
     extr16$Consolidada<-extr16$c16
     extr16$Cezinho<-'c16'
     extr16<-extr16[,c('cell_id','cell_name','Consolidada','Cezinho')]
},silent = TRUE)

consolida<-rbind(extr1, extr2, extr3, extr4, extr5, extr6, extr7, extr8, extr9, extr10, extr11, extr12, extr13, extr14, extr15, extr16)

consolida<-consolida[order(consolida$Consolidada),]
consolida<-consolida[order(consolida$cell_id),]
consolida<-consolida[!is.na(consolida$cell_id),]

nombre<-paste("Celdas_",index_id,"_",period_id,".csv",sep="")
write.csv(consolida, file=nombre, quote=FALSE, row.names = FALSE)

} # End of Function