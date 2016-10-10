#
# Grafica ACVs por tienda, ACV - SMS
#

library(RODBC)
queryo<-"SELECT index_id, period_id, source_id, status_id, source_acv, cell_id FROM index_period_source WHERE cell_id = 2050"
smsh<-odbcConnect('smsh',uid='nretail',pwd='nretail')
out<-sqlQuery(smsh,queryo)

#out$period_id2<-as.factor(out$period_id)
out$index_id2<-as.factor(out$index_id)
toplot<-as.data.frame(xtabs(source_acv~cell_id+period_id+index_id,data=out))

# Plot
library(ggplot2)
# Grafica cada tienda
qplot(period_id,source_acv,data=out,color=index_id2)

# Grafica cada celda
qplot(period_id,Freq,data=toplot,color=index_id)

#toplot<-xtabs(source_acv~period_id+index_id,data=out)

