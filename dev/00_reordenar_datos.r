library(janitor)
library(dplyr)

# carga ----

PathDatos<-"old/datos/"
load(paste(PathDatos,"Prototipo.RData",sep=""))

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

colunga <- colunga %>%
    clean_names()

use_data(colunga)

donaciones_mallplaza <- Donaciones_MallPlaza %>%
    as_tibble() %>%
    clean_names()

use_data(donaciones_mallplaza)

ranking_mallplaza <- RankingMallPlaza %>%
    as_tibble() %>%
    clean_names()

# prototipo ----

use_data(gtrends)

metricas_comuna <- MetricasComuna %>%
    as_tibble() %>%
    clean_names()

use_data(metricas_comuna)

proyectos_2016 <- Proyectos2016 %>%
    as_tibble() %>%
    clean_names()

use_data(proyectos_2016)

tot_categorias <- TotCategorias %>%
    as_tibble() %>%
    clean_names()

use_data(tot_categorias)

variables<-c(LETTERS[1:20],'A')
datos1<-runif(20)*100
datos2<-runif(20)*100
datos3<-runif(20)*100
datos4<-runif(20)*100
datos5<-runif(5)*100
datos6<-c(2014:2018)

use_data(variables)
use_data(datos1)
use_data(datos2)
use_data(datos3)
use_data(datos4)
use_data(datos5)
use_data(datos6)
