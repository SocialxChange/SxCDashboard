library(plyr)
library(dplyr)
library(sjlabelled)
setwd("~/Dropbox/Coding/BasesSxC/2018-06-17")
load("Donaciones.RData")
load("Tabla_Entidad.RData")
options(scipen=100)
options(digits=1)

Proyectos2016 <- aggregate(Donaciones_2016$monto.total.donacion.num, by=list(Category=Donaciones_2016$NOMBRE.DEL.PROYECTO,Category=Donaciones_2016$DONATARIO), FUN = sum)
colnames(Proyectos2016)<-c("proyecto", "organizacion", "total")
Proyectos2016$total<-Proyectos2016$total/1000000
total2016<-sum(Proyectos2016$total)
Proyectos2016<-cbind(Proyectos2016,(Proyectos2016$total/total2016)*100)
colnames(Proyectos2016)<-c("proyecto", "organizacion", "total", "ptotal")
Proyectos2016<-merge(Proyectos2016,aggregate(Donaciones_2016$monto.total.donacion.num, by=list(Category=Donaciones_2016$NOMBRE.DEL.PROYECTO), FUN = length), by.x="proyecto", by.y = "Category")
colnames(Proyectos2016)<-c("proyecto", "organizacion", "total", "pTotal", "nDonaciones")
Proyectos2016$mDonacion<-Proyectos2016$total/Proyectos2016$nDonaciones
ComunaEntidades<-Tabla_Entidad[,c(1,6)]
Proyectos2016<-merge(Proyectos2016,ComunaEntidades,by.x="organizacion",by.y="Entidad")
MetricasComuna<-data.frame(levels(as.factor(Proyectos2016$Comuna)), c(6.9, 6.1, 13.9, 0.6, 2.4, 11, 11, 0.7, 8, 6.6, 5.9, 0), c(24.2, 23.8, 42.4, 4.8, 10.7, 31, 31, 4.6, 27.1, 23.7, 11.6, 2.8))
colnames(MetricasComuna)<-c("comuna", "pobrezaIng", "pobrezaMult")
Proyectos2016<-merge(Proyectos2016,MetricasComuna,by.x="Comuna",by.y="comuna")
Proyectos2016<-Proyectos2016[,c(3,2,4:9,1)]
Categorias<-cbind(levels(as.factor(Proyectos2016$proyecto)), c(NA,"educacion", "salud", "infancia", "infancia", "educacion", "infancia", "salud","salud","educacion", "salud", "educacion","deporte","infancia", "urbanismo", "educacion", "urbanismo", "salud", "cultura", "emprendimiento", "emprendimiento", "integracion", "educacion", "emprendimiento", "educacion", "educacion", "salud", "salud", "infancia","infancia","educacion", "educacion", "urbanismo", "urbanismo", "infancia", "urbanismo", "educacion", "infancia", "urbanismo", "educacion", "deporte", "educacion", "educacion", "educacion", "educacion", "educacion", "drogas", "discapacidad"))
colnames(Categorias)<-c("proyecto", "categoria")
TendCat<-cbind(levels(as.factor(Categorias[,2])),c(50,74,74,74,50,3,2,50,74,1))
colnames(TendCat)<-c("categoria", "gtrend")
Proyectos2016<-merge(Proyectos2016,Categorias,by.x="proyecto",by.y="proyecto")
Proyectos2016<-merge(Proyectos2016,TendCat,by.x="categoria",by.y="categoria")
Proyectos2016$socialrank<-1/Proyectos2016$pTotal+as.numeric(Proyectos2016$gtrend)+as.numeric(Proyectos2016$pobrezaMult)
Proyectos2016<-Proyectos2016[,c(2:9,11:12,1,10)]

Proyectos2016<-set_label(Proyectos2016,label = c("Proyecto", "Organización", "Total Recibido (Millones)", "Porcentaje del Total", "Nro. Donaciones", "Don Promedio (Millones)", "Pobreza de Ingreso", "Pobreza Multidimensional", "Google Trend", "SocialRank", "Categoría", "Comuna"))

library(readr)
gtrends <- read_csv("gtrends.csv")
colnames(gtrends)=c("fecha", "educacion", "salud", "emprendimiento", "urbanismo", "infancia")
gtrends<-set_label(gtrends,label = c("Fecha", "Educación", "Salud", "Emprendimiento", "Urbanismo", "Infancia"))



TotCategorias <- aggregate(Proyectos2016$total, by=list(Category=Proyectos2016$categoria), FUN = sum)
MetricasComuna<-MetricasComuna[-6,]
MetricasComuna<-droplevels(MetricasComuna)

save(gtrends, Proyectos2016,TotCategorias, MetricasComuna, file="app/Prototipo.RData")

rm(list=ls())

load("app/Prototipo.RData")

Proyectos2016Red<-merge(Proyectos2016,aux,by.x="categoria",by.y="categoria")

aux<-data.frame(levels(Proyectos2016$categoria),c("educacion", "salud", "salud", "salud", "educacion", "economia","infancia","educacion","salud","urbanismo"))
colnames(aux)<-c("categoria", "categoriaRed")
TotCategoriasRed <- aggregate(Proyectos2016Red$total, by=list(Category=Proyectos2016Red$categoriaRed), FUN = sum)
Donatario2016 <- aggregate(Proyectos2016$total, by=list(Category=Proyectos2016$organizacion), FUN = sum)

