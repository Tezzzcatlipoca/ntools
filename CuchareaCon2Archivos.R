
#*************************************************************************************#
#                                                                                     #
# Programa para ajustar datos según los límites definidos para estratos específicos   #
#                                                                                     #
#    El programa necesita 2 archivos CSV:                                             #
#                                                                                     #
#    Un archivo con DATOS y sus factores a ser modificados,                           #
#    con las siguientes variables:                                                    #
#    * Estrato                                                                        #
#    * Valor                                                                          #
#                                                                                     #
#    Un archivo con Estratos y sus límites, con las siguientes variables:             #
#    * Estrato                                                                        #
#    * Limite                                                                         #
#                                                                                     #
#    El programa asigna límites a estratos según el archivo 2, manteniendo las        #
#    proporciones de las observaciones individuales del estrato, contenidas en        #
#    el archivo 1.                                                                    #
#                                                                                     #
#*************************************************************************************#

forzarestratos<-function(limites,BD){

# Abre archivo con límites para los factores
estrato.fat<-read.csv(limites)
names(estrato.fat)<-tolower(names(estrato.fat))
# Abre archivo con datos a limitar
datos<-read.csv(BD)
names(datos)<-tolower(names(datos))

# Extrae estratos
estrat.unic<-unique(estrato.fat$estrato)
estrat.dats<-unique(datos$estrato)

# Verifica si todos los estratos aparecen en la base cargada
czy.wiecej<-!estrat.unic %in% estrat.dats

# Marca error si no es el caso
if(sum(czy.wiecej)>0) {
     nums<-paste0(which(!estrat.unic %in% estrat.dats),collapse = ", ")
     mensaje<-paste0('Los sig. estratos no están en el archivo de datos: ', nums,collapse = " " )
     stop(mensaje)
}

# Inicializa variable principal
datos$ajustado<-NA

# Ajusta cada estrato
tam<-dim(estrato.fat)[1]
for (estrato in 1:tam) {
     estrato.ahora<-estrato.fat[estrato,'estrato']
     limite.ahora<-estrato.fat[estrato,'limite']
     # Se extraen datos del estrato
     extracto.datos<-datos[datos$estrato==estrato.ahora & !is.na(datos$estrato),]
     # Sacar ratio entre límite y suma de valores
     total.extracto<-sum(extracto.datos$valor)
     ratio<-limite.ahora/total.extracto # Por orden de divisor/dividendo, los datos van a ser aumentados
     # Convertir valores
     datos$ajustado[datos$estrato==estrato.ahora & !is.na(datos$estrato)]<-datos$valor[datos$estrato==estrato.ahora & !is.na(datos$estrato)]*ratio
     # Revisar que ningún valor sea menor a 1, sino ajustar
     valores<-datos$valor[datos$estrato==estrato.ahora & !is.na(datos$estrato)]*ratio
     prueba<-valores<1
     # A todas las observaciones con valores irreconciliables con objetivo, igualar a 1
     # (Para evitar que haya valores menores a 1)
     # Si son todos menores a 1, o si la suma de todos ellos es menor a su conteo
     # (es decir, cuando en promedio su valor es menor a 1)
     if (sum(prueba)==length(prueba) | sum(valores)<length(valores)){
          # Todos los valores son menores a 1. No es factible. Igualar todo a 1
          datos$ajustado[datos$estrato==estrato.ahora & !is.na(datos$estrato)]<-1
          datos[datos$estrato==estrato.ahora & !is.na(datos$estrato),'coment']<-'No es factible reducir mas'
     } else if (min(valores)<1){
          # Aumentar menores a 1 y reducir misma cantidad en otros factores.
          # A cada número se le recortará sólo lo
          # que sea posible cortarle.
          
          # Direcciones en el dataset original que apuntan a los datos a ser modificados
          donde.observados<-which(datos$estrato==estrato.ahora & !is.na(datos$estrato))
          # Distancias entre los datos a ser modificados y la unidad mínima posible (1)
          diferencias<-1-datos$ajustado[donde.observados]
          # Datos menores a la unidad mínima (1)
          a_restar<-diferencias[diferencias>0]
          # Ubicación local de los datos menores a la unidad mínima (1)
          donde_restar<-which(diferencias>0)
          # Ubicación original de los datos mayores a la unidad mínima (1)
          local_aumentar<-donde.observados[donde_restar] # Dónde están
          # Se ajustan los datos que deben ser igualados a la unidad mínima (1)
          datos$ajustado[local_aumentar]<-1 # Ajustar
          datos[local_aumentar,'coment']<-'Ajustado a la alta'
          
          ## Ajustar los que deben ser disminuidos
          # Cantidad total a ser redistribuida entre los valores que pueden ser disminuidos
          total_resta<-sum(a_restar)
          # Datos mayores a la unidad mínima (pueden ser disminuidos)
          a_sumar<-diferencias[diferencias<=0]
          # Ubicación local de los datos mayores a la unidad mínima
          donde_sumar<-which(diferencias<=0)
          # Sacar ratio (para saber cuánta capacidad de reducción tiene cada valor)
          total_suma<-sum(a_sumar) # Suma
          ratios_sumar<-a_sumar/total_suma # Ratio/Proporción
          # Distribuir las caídas según la posibilidad de cada valor de ser
          # disminuido sin caer debajo el valor mínimo (1)
          restas<-ratios_sumar*total_resta
          # Se aplican los cambios a los valores originales
          resultados<-(-a_sumar+1)-restas # Se restan porque los segundos son positivos
          
          # Ubicación original de los datos mayores a la unidad mínima (1)
          local_restar<-donde.observados[donde_sumar]
          # Se aplican los cambios
          datos$ajustado[local_restar]<-resultados
          datos[local_restar,'coment']<-'Ajustado a la baja'
          
     }
     
     
}

try({
     datos[is.na(datos$coment),'coment']<-''
})

nombreout<-sub(".csv","-out.csv",BD)
write.csv(datos,nombreout)

} # Fin de función
