
#
#                        Este programa escoge la muestra para diferentes estratos 
#                           
#    Se toman dos archivos:
#    -Archivo con estratos y sus tamaños de muestra. Debe contener las variables:
#              * "Estrato": Identificador de estrato (Estratos deben coincidir con los del archivo de MM)
#              * "Ideal": Tamaño ideal de la muestra para cada estrato
#
#    -Archivo con tiendas (MM) con sus estratos. Debe contener las variables:
#              * "ID": Identificador único de tienda
#              * "Estrato": Identificador de estrato (Estratos deben coincidir con los del archivo de Ideales)
#    
#    Todos los estratos de la MM deben aparecer en la lista de Ideales y deben tener un ideal numérico válido.
#

# INPUT:
arch.ideales<-"Ideales.csv"   # Formato CSV
arch.mm<-"Papelarias por Microregião_final_apoio_v2.csv"    # Formato CSV
semilla<-1508 # Para reproducibilidad. Escoger otro número -aleatoriamente- cada que se quiera una muestra distinta.
archivo.salida<-"Papelerias-Sample2.txt"
#
#


library(dplyr)

ideales<-read.csv(arch.ideales)
papelerias<-read.csv(arch.mm)
papelerias$key<-1:dim(papelerias)[1]
set.seed(semilla) # Para garantizar reproducibilidad. 

factores<-unique(papelerias$Estrato)
cuenta<-length(factores)

# Inicializar variable de status (Elegida o no)
papelerias$status<-'-'

for (i in 1:cuenta) {
     # Escoger las tiendas
     factor.actual<-factores[i] # Choose stratum
     ideal.actual<-ideales[ideales$Estrato==factor.actual,"Ideal"] # Get stratum sample size
     nums.disponibles<-papelerias$key[papelerias$Estrato==factor.actual] # Get stratum's available store IDs
     escogidos<-sample(nums.disponibles,ideal.actual)  # Get store sample
     rechazados<-nums.disponibles[!nums.disponibles %in% escogidos] # Recognize stores not chosen
     opcionales<-sample(rechazados,ideal.actual*3)     # Get second (optional) sample
     # Marcar las tiendas
     papelerias$status[papelerias$key %in% escogidos]<-'Obligatoria'  
     papelerias$status[papelerias$key %in% opcionales]<-'Opcional'
}

# Para evitar que el archivo aparezca como SYLK en Excel
variables<-names(papelerias)
coinc<-sum(grepl(variables,pattern = "id"))
if (coinc==0) {
     papelerias$ID<-as.character(papelerias$ID)
     papelerias$id<-papelerias$ID 
} else {
     papelerias$id<-as.character(papelerias$id)
}

papelerias<-select(papelerias,id,Estrato,status)


write.table(papelerias,file = archivo.salida, row.names = F, col.names = T,sep = "\t")


