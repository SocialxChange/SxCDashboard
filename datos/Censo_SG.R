rm(list=ls())

Sys.setlocale("LC_ALL", 'UTF-8')

setwd("/Users/joaquinfernandez/Documents/Proyecto Educacional Batuco/Bases de Datos/Censo")
getwd()

library(plyr)

load("Censo2017_per.RData")

Censo_SG_p <- subset(Censo_personas, COMUNA == 2103)

save(Censo_SG_p,file="Censo_SG.Rda")

## Por Edad 
SG_sin_educ<-subset(Censo_SG_p$PERSONAN, Censo_SG_p$P13 != 1)
SG_4_6<-subset(Censo_SG_p$PERSONAN, Censo_SG_p$P09 >= 4 & Censo_SG_p$P09 <= 6)
SG_6_14<-subset(Censo_SG_p$PERSONAN, Censo_SG_p$P09 >= 6 & Censo_SG_p$P09 <= 14)
SG_14_19<-subset(Censo_SG_p$PERSONAN, Censo_SG_p$P09 >= 14 & Censo_SG_p$P09 <= 19)
SG_4_19<-subset(Censo_SG_p$PERSONAN, Censo_SG_p$P09 >= 4 & Censo_SG_p$P09 <= 19)