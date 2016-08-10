
#  Para cucharear los ideales
#
# 1. El programa debe poder identificar todas las filas para las cuales hay reducción posible
# 2. Después debe identificar los que tienen un cambio porcentual alto
# 3. Debe crearse una columna nueva con las propuestas de reducción para 

# Cómo llamar a la función

cucharea("_OptAlloc_Result-TOTAL.xls")


# Función
cucharea<-function(archivo, reordena=TRUE) {
  
# Abrir archivo de excel
library(XLConnect)
wb<-loadWorkbook(archivo)
sheet<-readWorksheet(wb,sheet="Cell Report")

# Se genera la tabla de factores máximos para cada "n"
contador<-seq(3,40)
factor<-seq(.7,.10,length.out = 38)
neg<-seq(-.3,-.1,length.out = 38)
tabla<-data.frame(n=contador,factor=factor,negative=neg)

# Diferencia entre ideales y físicas
if (sum(names(sheet)=='n.round_OPTIMAL')==1) {
    sheet$Diff<-sheet$n.round_OPTIMAL-sheet$n.current
} else {
    sheet$Diff<-sheet$n.int_OPTIMAL-sheet$n.current
}
sheet$proc<-sheet$Diff/sheet$n.current

# Calcular nivel de cambios aceptables
tres<-merge(sheet,tabla,by.x = 'n.current',by.y='n',all.x=TRUE,all.y=FALSE)
tres$NAs<-is.na(tres$factor)
tres$factor[tres$NAs==TRUE & tres$n.current<3]<-.7
tres$factor[tres$NAs==TRUE & tres$n.current>40]<-.1
tres$negative[tres$NAs==TRUE & tres$n.current<3]<-(-0.3)
tres$negative[tres$NAs==TRUE & tres$n.current>40]<-(-0.1)

tres$NAs<-NULL


# Insertar variable de ideal propuesto
tres$propuesta<-0
# Ajustar los que aumentan más de lo normal
tres$propuesta[tres$proc>tres$factor]<-ceiling((1+tres$factor[tres$proc>tres$factor])*(tres$n.current[tres$proc>tres$factor])) 

### Está generando columna propuesta con valores de 0
### Abajo debe corregirse esto pero no lo hace, revisar por qué -----------------------------------------------
### -------------------------------------------------------------------

# Ajustar los que disminuyen más de lo normal
tres$propuesta[tres$proc<tres$negative]<-ceiling((1+tres$negative[tres$proc<tres$negative])*(tres$n.current[tres$proc<tres$negative]))
# Los que no representan cambios grandes dejarlos iguales
if (sum(names(sheet)=='n.round_OPTIMAL')==1) {
     tres$propuesta[tres$propuesta==0]<-tres$n.round_OPTIMAL[tres$propuesta==0]
} else {
     tres$propuesta[tres$propuesta==0]<-tres$n.int_OPTIMAL[tres$propuesta==0]
}

# Asgina nuevo orden a las columnas de salida
if (reordena==TRUE) {
    hojaout<-tres[,c(2,3,4,5,6,1,7,8,9,10,41,11,12,13,37,38,14,39,40,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36)]
} else {
    hojaout<-tres
}
    
createSheet(wb,name="Cell Report 2")
writeWorksheet(wb,hojaout,sheet="Cell Report 2")
saveWorkbook(wb)

} # Fin de la función

