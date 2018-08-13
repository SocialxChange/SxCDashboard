library(shiny)
library(dplyr)
library(shinydashboard)
library(bubbles)        # devtools::install_github("jcheng5/bubbles")
library(plotly)         # grafico tendencia gtrend

PathDatos<-"~/SxCShinyApp/datos/"
load(paste(PathDatos,"Prototipo.RData",sep=""))
     