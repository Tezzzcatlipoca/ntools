

# Programa para encontrar los criterios ideales para Peq y Gde.
# Agrupa las tiendas en Gde y Peq con diferentes criterios y va calculando la 
# variación. La grafica.

facv<-read.csv("func-acv.csv")
facv$rat<-as.factor(facv$ACV)
facv.old<-facv
facv<-facv[facv$ACV<max(facv$ACV,na.rm = TRUE) & !is.na(facv$ACV),]


# Para verificar por Número de Empleados (NFUNC)
strata<-as.numeric(names(table(facv$NFUNC)))
posibles<-length(strata)-1


for (i in 1:posibles) {
     g1.st<-strata[1:i]
     g2.st<-strata[(i+1):length(strata)]
     g1<-facv[facv$NFUNC %in% g1.st & !is.na(facv$ACV),]
     g2<-facv[facv$NFUNC %in% g2.st & !is.na(facv$ACV),]
     sum.g1<-sum(g1$ACV)
     sum.g2<-sum(g2$ACV)
     var.g1<-sd(g1$ACV)
     var.g2<-sd(g2$ACV)
     if(i==1){
          nomes<-paste0("PeqMax",strata[i])
          var.tot<-var.g1+var.g2
          var.intra<-sd(c(sum.g1,sum.g2))
     } else {
          nomes<-c(nomes,paste0("PeqMax",strata[i]))
          var.tot<-c(var.tot,(var.g1+var.g2))
          var.intra<-c(var.intra,sd(c(sum.g1,sum.g2)))
     }
}
names(var.tot)<-nomes
plot(var.tot)
#lines(var.intra,colour="red")

# Para verificar por ACV
strata<-unique(facv$ACV) # Esto funciona solamente cuanto hay pocos ACVs diferentes
posibles<-length(strata)-1

for (i in 1:posibles) {
     g1.st<-strata[1:i]
     g2.st<-strata[(i+1):length(strata)]
     g1<-facv[facv$ACV %in% g1.st & !is.na(facv$ACV),]
     g2<-facv[facv$ACV %in% g2.st & !is.na(facv$ACV),]
     sum.g1<-sum(g1$ACV)
     sum.g2<-sum(g2$ACV)
     var.g1<-sd(g1$ACV)
     var.g2<-sd(g2$ACV)
     if(i==1){
          nomes<-paste0("PeqMax",strata[i])
          var.tot2<-var.g1+var.g2
          var.intra2<-sd(c(sum.g1,sum.g2))
     } else {
          nomes<-c(nomes,paste0("PeqMax",strata[i]))
          var.tot2<-c(var.tot2,(var.g1+var.g2))
          var.intra2<-c(var.intra2,sd(c(sum.g1,sum.g2)))
     }
}
names(var.tot2)<-nomes
plot(var.tot2)

# Por área de ventas

strata<-as.numeric(names(table(facv$area_de_vendas)))
posibles<-length(strata)-1


for (i in 1:posibles) {
     g1.st<-strata[1:i]
     g2.st<-strata[(i+1):length(strata)]
     g1<-facv[facv$area_de_vendas %in% g1.st & !is.na(facv$ACV),]
     g2<-facv[facv$area_de_vendas %in% g2.st & !is.na(facv$ACV),]
     sum.g1<-sum(g1$ACV)
     sum.g2<-sum(g2$ACV)
     var.g1<-sd(g1$ACV)
     var.g2<-sd(g2$ACV)
     if(i==1){
          nomes<-paste0("PeqMax",strata[i])
          var.tot3<-var.g1+var.g2
          var.intra3<-sd(c(sum.g1,sum.g2))
     } else {
          nomes<-c(nomes,paste0("PeqMax",strata[i]))
          var.tot3<-c(var.tot3,(var.g1+var.g2))
          var.intra3<-c(var.intra3,sd(c(sum.g1,sum.g2)))
     }
}
names(var.tot3)<-nomes
plot(var.tot3)

# Por Número de Checkouts

strata<-as.numeric(names(table(facv$no_checkouts)))
posibles<-length(strata)-1


for (i in 1:posibles) {
     g1.st<-strata[1:i]
     g2.st<-strata[(i+1):length(strata)]
     g1<-facv[facv$no_checkouts %in% g1.st & !is.na(facv$ACV),]
     g2<-facv[facv$no_checkouts %in% g2.st & !is.na(facv$ACV),]
     sum.g1<-sum(g1$ACV)
     sum.g2<-sum(g2$ACV)
     var.g1<-sd(g1$ACV)
     var.g2<-sd(g2$ACV)
     if(i==1){
          nomes<-paste0("PeqMax",strata[i])
          var.tot4<-var.g1+var.g2
          var.intra4<-sd(c(sum.g1,sum.g2))
     } else {
          nomes<-c(nomes,paste0("PeqMax",strata[i]))
          var.tot4<-c(var.tot4,(var.g1+var.g2))
          var.intra4<-c(var.intra4,sd(c(sum.g1,sum.g2)))
     }
}
names(var.tot4)<-nomes
plot(var.tot4)



# library(Hmisc) # En caso de que hubieran demasiados ACVs diferentes
#facv$grupos.acv<-cut2(facv$ACV,g=6) 


