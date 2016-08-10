
buscala<-function(buscadas) {
# Hasta este punto ya se extraheron todos los cezinhos
padre<-buscadas
#Saca celdas parientes

#padre<-consolida$cell_id[consolida$Consolidada %in% buscadas]
#relacion<-consolida[consolida$Consolidada %in% buscadas,]
hijas<-consolida$Consolidada[consolida$cell_id %in% buscadas]
relacion2<-consolida[consolida$cell_id %in% padre,]
parientes<-c(padre,hijas)


activas<-c(1,2)
uni15$fator<-uni15$fator_fim
data1<-uni15[uni15$CONDICAO %in% activas & uni15$CELL_ID %in% parientes,c('CELL_ID','mktr','fator','LOJA1','ACV')]
data3<-data1
data3$cell<-data3$CELL_ID

#Saber si es una celda SOT o no
totalregistros<-nrow(data3)
mktrs<-data3$mktr
no<-is.na(mktrs)
newmktr<-mktrs[!no]
totalsot<-length(newmktr)
if (totalregistros==totalsot) {
     sot<-1
} else {
     sot<-0
}
if (sot==1) {
     print('Celda SOT. Revisar si la tienda reportada por IV aparece abajo:')
     print(data3)
}

#Converting from factors to numbers to be able to count
data3$ACV<-as.double(as.character(data3$ACV))
data3$fator<-as.integer(as.character(data3$fator))

# Tabla de ACV por celda
ACVs<-aggregate(ACV~cell,data3,FUN=sum)
Ns<-aggregate(fator~cell,data3,FUN=sum)
NewACV<-ACVs[ACVs$cell==buscadas,"ACV"]  # Propuesta de Nuevo ACV
sumN<-sum(Ns$fator)
valores<-unique(data3$CELL_ID)

tabla<-data.frame(cell=0,proportion=0)
tabla<-tabla[-1,]

for (i in 1:length(valores)) {
     ourN<-Ns[Ns$cell==valores[i],"fator"]
     proporcion<-ourN/sumN
     linea<-data.frame(cell=valores[i],proportion=proporcion)
     tabla<-rbind(tabla,linea)
}
     
print(tabla)

#ourN<-Ns[Ns$cell==buscadas,"fator"]
#proporcion<-ourN/sumN
#proporcion

} #End of function
