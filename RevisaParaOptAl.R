
# ***********************************************************************************************************
#  Esta función sirve para identificar los mercados que deben ser inactivados para poder correr             *
#  el archivo en OptAl.                                                                                     *
# ***********************************************************************************************************

# Cómo llamar la función
revisaOptAl("InsumoCorr5.xls")


# Función
revisaOptAl<-function(archivo) {

library(XLConnect)

wk = loadWorkbook(archivo) 
df = readWorksheet(wk, sheet="Cell")

columnas<-length(names(df))
# 13 primeras columnas son info de celda
# columna 14 - 29 son mercados (16 columnas)

#Primero agregar a cada mercado en cada columna la parte del índice
for (i in 14:columnas) {
    df[,i]<-paste(df$index,df[,i],sep="-")
}

#Crear tabla de mercados y sus sumas de cbar
marcadores<-data.frame(ind=0,mbd=0,cbar=0,N=0)
marcadores<-marcadores[-1,]
for (i in 14:columnas) {
    #solomercados<-df[,i]
    #ind_merc<-paste(df$index,solomercados,sep="-")
    #unicas<-unique(ind_merc)
    unicas<-unique(df[,i])
    borrar<-grepl("NULL",unicas) # Eliminar NULLs
    unicas<-unicas[!borrar]
    nummerc<-length(unicas)
    for (j in 1:nummerc) {
        cbarsum<-sum(df$cbar[df[,i]==unicas[j]]) # Extrae suma de cbar
        Nsum<-sum(df$N[df[,i]==unicas[j]]) # Extrae suma de N
        bito<-data.frame(mbd=unicas[j],cbar=cbarsum,N=Nsum)
        marcadores<-rbind(marcadores,bito)
    }
}

# Identificar cuando un mercado tiene cbar = 0
primero<-marcadores[marcadores$cbar==0,]
if (dim(primero)[1]==0) {
    print('No hay mercados con cbar==0')
} else {
    print('Los siguientes mercados tienen cbar==0, su flag debe ser 0')
    print(primero[,c(1,2)])
}

# Identificar cuando un mercado tiene N < 30
segundo<-marcadores[marcadores$N<30,]
if (dim(segundo)[1]==0) {
    print('No hay mercados con N<30')
} else {
    print('Los siguientes mercados tienen N<30, su mbdmin debe ser igual o menor a N:')
    print(segundo[,c(1,3)])
}

} # Fin de la función
