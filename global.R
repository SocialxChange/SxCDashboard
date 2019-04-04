Sys.setlocale("LC_ALL", 'UTF-8')
library(shiny)
library(dplyr)
library(shinydashboard)
library(bubbles)        # devtools::install_github("jcheng5/bubbles")
library(plotly)         # grafico tendencia gtrend
library(DT) 
library(colorspace)
library(scales)

options(digits=2)
options(scipen=100)



PathDatos<-"datos/"
load(paste(PathDatos,"Prototipo.RData",sep=""))
#Generada luego de linea 87 en Mall_Plaza.Rmd
load(paste(PathDatos,"20180920RankingMallPlaza2009_2016.RData",sep=""))
#Generada por  Mall_Plaza.R 
load(paste(PathDatos,"20180920Donaciones_MallPlaza2009_2016.RData",sep=""))
#Importada de planilla de Arturo Celedon
load(paste(PathDatos,"colunga.RData",sep=""))
# Cambio nombre colunga$`EVALUACION EFECTIVIDAD AÑO 0 (enero 2017`
colnames(colunga)[30] <- "Efectividad02017"
colunga$Efectividad02017<-as.numeric(colunga$Efectividad02017)
colunga$REGION[which(colunga$REGION=="Bío - Bío" | 
                       colunga$REGION=="Bio Bio" | 
                       colunga$REGION=="Bío Bío" | 
                       colunga$REGION=="Biobio")]<-"Bío Bío"

variables<-c(LETTERS[1:20],'A')
datos1<-runif(20)*100
datos2<-runif(20)*100
