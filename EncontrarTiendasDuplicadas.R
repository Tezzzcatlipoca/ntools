
# Encontrar tiendas duplicadas

base<-read.table("EncontrarDuplicadas.txt",sep="\t",quote = "",header = TRUE)

parecen<-function(uno,dos){        # Busca variaciones de palabra uno en palabra dos
     uno<-sub("\\(","",uno)
     uno<-sub("\\)","",uno)
     uno<-tolower(uno)
     dos<-sub("\\(","",dos)
     dos<-sub("\\)","",dos)
     dos<-tolower(dos)
     if (length(a)>1) { stop('Error. Variable a comparar debe tener sólo 1 observación.') }
     palabras<-strsplit(uno," ")[[1]]
     marcador<-0
     for (pal in 1:length(palabras)) {
          slowo<-palabras[pal]
          rozmiar<-nchar(slowo)
          for (let in 1:rozmiar) {
               patron<-slowo
               substr(patron,let,let)<-"."
               if (grepl(dos,pattern = patron) & rozmiar>2) {
                    marcador<-marcador+rozmiar
                    break
               }
          }
     }
     espacios<-sum(grepl(uno,pattern = " "))
     tamano.uno<-marcador+espacios
     espacios<-sum(grepl(dos,pattern = " "))
     tamano.dos<-nchar(dos)-espacios

     ratios<-tamano.uno/tamano.dos
     if (ratios>.8) {
          TRUE
     } else {
          FALSE
     }
}

# Crear llaves para comparación
base$key3<-paste0(tolower(base$RAZSOC),tolower(base$END),base$Numero)
base$key4<-paste0(base$Cep,tolower(base$END),base$Numero)
base$key5<-paste0(tolower(base$Municipio),tolower(base$END),base$Numero)
base$key6<-paste0(base$Telefone,tolower(base$Municipio))

# Identificar duplicadas
#1        Por CNPJ
base$dup1<-duplicated(base$Cnpj)
base$dup1[base$Cnpj==0]<-FALSE # Evitar contar los valores 0 repetidos
base$dup1[is.na(base$Cnpj)]<-FALSE # Evitar contar NAs repetidos

#2             NO SIRVE : HAY DEMASIADAS OBSERVACIONES REPETIDAS QUE NO SON TIENDAS REPETIDAS
#base$dup2<-duplicated(base$RAZSOC)
#base$dup2[nchar(as.character(base$RAZSOC))<2]<-FALSE
#base$dup2[is.na(base$RAZSOC)]<-FALSE # Evitar contar NAs repetidos

#3        Razon Social - Endereco - Numero
base$dup3<-duplicated(base$key3)
base$dup3[nchar(as.character(base$RAZSOC))<2]<-FALSE
base$dup3[is.na(base$END)]<-FALSE
base$dup3[nchar(as.character(base$END))<2]<-FALSE
base$dup3[is.na(base$RAZSOC)]<-FALSE # Evitar contar NAs repetidos
base$dup3[grepl(base$RAZSOC,pattern = "nao ")]<-FALSE
base$dup3[nchar(as.character(base$RAZSOC))<4]<-FALSE
base$dup3[grepl(base$Numero,pattern = "s")]<-FALSE
base$dup3[is.na(base$Numero)]<-FALSE
base$dup3[base$Numero==0]<-FALSE

#4        CP - Endereco - Numero
base$dup4<-duplicated(base$key4)
base$dup4[is.na(base$Cep)]<-FALSE
base$dup4[base$Cep==0]<-FALSE
base$dup4[is.na(base$END)]<-FALSE
base$dup4[nchar(as.character(base$END))<2]<-FALSE
base$dup4[is.na(base$Numero)]<-FALSE
base$dup4[grepl(base$Numero,pattern = "s")]<-FALSE
base$dup4[base$Numero==0]<-FALSE

#5        EST - Municipio - Endereco - Numero
base$dup5<-duplicated(base$key5)
base$dup5[is.na(base$END)]<-FALSE
base$dup5[nchar(as.character(base$END))<2]<-FALSE
base$dup5[is.na(base$Numero)]<-FALSE
base$dup5[grepl(base$Numero,pattern = "s")]<-FALSE
base$dup5[base$Numero==0]<-FALSE

#6        Telefono - Municipio
base$dup6<-duplicated(base$key6)
base$dup6[is.na(base$Telefone)]<-FALSE
base$dup6[base$Telefone==0]<-FALSE

# Aparear las observaciones duplicadas

#1
found1<-which(base$dup1)
valores1<-unique(base$Cnpj[found1])
base$dup.id1<-''
for (i in 1:length(valores1)) {
     direcciones<-which(base$Cnpj==valores1[i])
     base$dup.id1[direcciones]<-i
}

#2
#found2<-which(base$dup2)
#valores2<-unique(base$RAZSOC[found2])
base$dup.id2<-''
#for (i in 1:length(valores2)) {
#     direcciones<-which(base$RAZSOC==valores2[i])
#     base$dup.id2[direcciones]<-i
#}

#3
found3<-which(base$dup3)
valores3<-unique(base$key3[found3])
base$dup.id3<-''
for (i in 1:length(valores3)) {
     direcciones<-which(base$key3==valores3[i])
     base$dup.id3[direcciones]<-i
}

#4
found4<-which(base$dup4)
valores4<-unique(base$key4[found4])
base$dup.id4<-''
for (i in 1:length(valores4)) {
     direcciones<-which(base$key4==valores4[i])
     base$dup.id4[direcciones]<-i
}

#5
found5<-which(base$dup5)
valores5<-unique(base$key5[found5])
base$dup.id5<-''
for (i in 1:length(valores5)) {
     direcciones<-which(base$key5==valores5[i])
     base$dup.id5[direcciones]<-i
}

#6
found6<-which(base$dup6)
valores6<-unique(base$key6[found6])
base$dup.id6<-''
for (i in 1:length(valores6)) {
     direcciones<-which(base$key6==valores6[i])
     base$dup.id6[direcciones]<-i
}

# Identifica tiendas en centros comerciales para ignorarlas

tot.4<-unique(base$dup.id4)
maxo<-length(tot.4)
for (i in 1:maxo) {
     ya<-0
     cuales<-base[base$dup.id4==tot.4[i],]
     cuantos<-dim(cuales)[1]
     if (cuantos>3 | cuantos==1) {                     # Ignora las tiendas que aparecen muchas juntas
          base$dup.id4[base$dup.id4==tot.4[i]]<-''     # (muy posiblemente dentro de centros comerciales)
          ya<-1
     }
     todref<-paste0(cuales$REFEREN,collapse = " ")    # Ignora las tiendas que estan dentro de centros comerciales
     todref<-tolower(todref)
     if (grepl(todref,pattern = "^praca|shop|dentro|mall") & ya==0) {
          base$dup.id4[base$dup.id4==tot.4[i]]<-''
          ya<-1
     }
     parec1<-parecen(cuales$NOME[1],cuales$NOME[2])
     parec2<-parecen(cuales$NOME[2],cuales$NOME[1])
     parecidos<-parec1||parec2  # Si se parece uno u otro
     if (!parecidos & ya==0) {                         # Omite todas las tiendas que tienen direcciones parecidas
          base$dup.id4[base$dup.id4==tot.4[i]]<-''     # pero nombres distintos
          ya<-1
     }
}

tot.5<-unique(base$dup.id5)
maxo<-length(tot.5)
for (i in 1:maxo) {
     ya<-0
     cuales<-base[base$dup.id5==tot.5[i],]
     cuantos<-dim(cuales)[1]
     if (cuantos>3 | cuantos==1) {
          base$dup.id5[base$dup.id5==tot.5[i]]<-''
          ya<-1
     }
     todref<-paste0(cuales$REFEREN,collapse = " ")     # Ignora las tiendas que estan dentro de centros comerciales
     todref<-tolower(todref)
     if (grepl(todref,pattern = "^praca|shop|dentro|mall") & ya==0) {
          base$dup.id5[base$dup.id5==tot.5[i]]<-''
          ya<-1
     }
     parec1<-parecen(cuales$NOME[1],cuales$NOME[2])
     parec2<-parecen(cuales$NOME[2],cuales$NOME[1])
     parecidos<-parec1||parec2  # Si se parece uno u otro
     if (!parecidos & ya==0) {
          base$dup.id5[base$dup.id5==tot.5[i]]<-''
          ya<-1
     }
}

# Genera identificador global de repetidas
base$rep.tot.id<-paste(base$dup.id1,base$dup.id2,base$dup.id3,base$dup.id4,base$dup.id5,base$dup.id6,sep = "-")

# Graba registros en disco
write.table(base,file = "DuplicadasEncontradas2.txt",row.names = FALSE,sep = "\t",quote = FALSE)


