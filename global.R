Sys.setlocale("LC_ALL", 'UTF-8')
library(shiny)
library(dplyr)
library(shinydashboard)
library(bubbles)        # devtools::install_github("jcheng5/bubbles")
library(plotly)         # grafico tendencia gtrend
library(DT) 
library(colorspace)
library(scales)





PathDatos<-"datos/"
load(paste(PathDatos,"Prototipo.RData",sep=""))
#Generada luego de linea 87 en Mall_Plaza.Rmd
load(paste(PathDatos,"20180920RankingMallPlaza2009_2016.RData",sep=""))
#Generada por  Mall_Plaza.R 
load(paste(PathDatos,"20180920Donaciones_MallPlaza2009_2016.RData",sep=""))
