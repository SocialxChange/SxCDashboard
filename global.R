Sys.setlocale("LC_ALL", 'UTF-8')
library(shiny)
library(dplyr)
library(shinydashboard)
library(bubbles)        # devtools::install_github("jcheng5/bubbles")
library(plotly)         # grafico tendencia gtrend
library(DT) 
library(colorspace)
library(scales)
library(leaflet)
library(leaflet.extras)
library(readr)
library(readxl)


options(digits=2)
options(scipen=100)





PathDatos<-"datos/"

nomina <- read_delim(paste(PathDatos,"nomina.csv",sep=""), 
                     ";", escape_double = FALSE, trim_ws = TRUE)
carreras <- read_excel(paste(PathDatos,"Buscador-empleabilidad-e-ingresos_mifuturo_2020.xlsx",sep=""))
carreras<- carreras[1:(nrow(carreras)-3),]
load(paste(PathDatos,"Prototipo.RData",sep=""))
load(paste(PathDatos,"Censo_SG.RDa",sep=""))



#Generada luego de linea 87 en Mall_Plaza.Rmd
load(paste(PathDatos,"20180920RankingMallPlaza2009_2016.RData",sep=""))
#Generada por  Mall_Plaza.R 
load(paste(PathDatos,"20180920Donaciones_MallPlaza2009_2016.RData",sep=""))
#Importada de planilla de Arturo Celedon
load(paste(PathDatos,"colunga.RData",sep=""))
# Cambio nombre colunga$`EVALUACION EFECTIVIDAD AÑO 0 (enero 2017`
colnames(colunga)[15] <- "AnioAsignacion"
colnames(colunga)[18] <- "AporteConvenio"
colnames(colunga)[30] <- "Efectividad02017"
colunga$Efectividad02017<-as.numeric(colunga$Efectividad02017)
colunga$REGION[which(colunga$REGION=="Bío - Bío" | 
                       colunga$REGION=="Bio Bio" | 
                       colunga$REGION=="Bío Bío" | 
                       colunga$REGION=="Biobio")]<-"Bío Bío"
colunga$REGION[which(colunga$REGION=="Araucania")]<-"Araucanía"
colunga$REGION[which(colunga$REGION=="Arica")]<-"Arica Parinacota"
colunga$REGION[which(colunga$REGION=="Cuarta región")]<-"Coquimbo"
colunga$REGION[which(colunga$REGION=="nacional")]<-"Nacional"


#colunga$latitud<-NA
#colunga$latitud<-ifelse(colunga$REGION=="Metropolitana",-33.4569397,colunga$latitud)
#colunga$latitud[colunga$REGION=="Metropolitana"]<--33.4569397
#colunga$latitud[colunga$REGION=="Santiago"]<--33.4569397
#colunga$latitud[colunga$REGION=="Valparaíso"]<--33.0359993
#colunga$latitud[colunga$REGION=="Antofagasta"]<--23.6523609
#colunga$latitud[colunga$REGION=="Araucanía"]<--38.7396507
#colunga$latitud[colunga$REGION=="Arica Parinacota"]<--18.4745998
#colunga$latitud[colunga$REGION=="Coquimbo"]<--29.9045296
#colunga$latitud[colunga$REGION=="Bío Bío"]<--36.8269882
#colunga$latitud[colunga$REGION=="Maule"]<--35.4263992

#colunga$latitud[colunga$REGION=="Manabí"]<--0.96212
#colunga$latitud[colunga$REGION=="Perú"]<--12.0431800
#colunga$latitud[colunga$REGION=="México"]<--102.0000000


#colunga$longitud<-NA
#colunga$longitud<-ifelse(colunga$REGION=="Metropolitana",-70.6482697,colunga$longitud)
#colunga$longitud[colunga$REGION=="Metropolitana"]<--70.6482697
#colunga$longitud[colunga$REGION=="Santiago"]<--70.6482697
#colunga$longitud[colunga$REGION=="Valparaíso"]<--71.629631
#colunga$longitud[colunga$REGION=="Antofagasta"]<--70.395401
#colunga$longitud[colunga$REGION=="Araucanía"]<--72.5984192
#colunga$longitud[colunga$REGION=="Arica Parinacota"]<--70.2979202
#colunga$longitud[colunga$REGION=="Coquimbo"]<--71.2489395
#colunga$longitud[colunga$REGION=="Bío Bío"]<--73.0497665
#colunga$longitud[colunga$REGION=="Maule"]<--71.6554184

#colunga$longitud[colunga$REGION=="Manabí"]<-80.7127075
#colunga$longitud[colunga$REGION=="Perú"]<--77.0282400
#colunga$longitud[colunga$REGION=="México"]<-23.0000000



variables<-c(LETTERS[1:20],'A')
datos1<-runif(20)*100
datos2<-runif(20)*100
datos3<-runif(20)*100
datos4<-runif(20)*100
datos5<-runif(5)*100
datos6<-c(2014:2018)
