rm(list=ls())

Sys.setlocale("LC_ALL", 'UTF-8')

setwd("/Users/joaquinfernandez/Documents/SxCDashboard")
getwd()

library(plyr)
library(readxl)
library(readr)
library(gdata)


PathDatos<-"datos/"

## Cargo beneficiarios por año y proyecciones comunales SG
Becados<-read_csv(paste(PathDatos,"Becas por anho - Hoja 1.csv",sep=""))

Proyec_Comunas<-read_excel("/Users/joaquinfernandez/Documents/SxCDashboard/Datos/Proyec_comunas.xlsx")
Proyecc_SG<-subset(Proyec_Comunas, Proyec_Comunas$`Nombre Comuna`=="Sierra Gorda")


#####################
###Rangos etarios ###
#####################

###Edad 4 a 6 años
P_46SG<-subset(Proyecc_SG, Proyecc_SG$Edad >= 4 & Proyecc_SG$Edad <= 6) 
P_46SG<-P_46SG[,18:26]
A <- rbind(P_46SG, c("Parvulo", colSums(P_46SG[,1:9])))
A<-A[7,]
names(A)[1] <- "Rango Etario"
names(A)[2] <- "2012"
names(A)[3] <- "2013"
names(A)[4] <- "2014"
names(A)[5] <- "2015"
names(A)[6] <- "2016"
names(A)[7] <- "2017"
names(A)[8] <- "2018"
names(A)[9] <- "2019"


###Edad 7 a 14 años
P_714SG<-subset(Proyecc_SG, Proyecc_SG$Edad >= 7 & Proyecc_SG$Edad <= 14) 
P_714SG<-P_714SG[,18:26]
B <- rbind(P_714SG, c("Basica", colSums(P_714SG[,1:9])))
B<-B[17,]
names(B)[1] <- "Rango Etario"
names(B)[2] <- "2012"
names(B)[3] <- "2013"
names(B)[4] <- "2014"
names(B)[5] <- "2015"
names(B)[6] <- "2016"
names(B)[7] <- "2017"
names(B)[8] <- "2018"
names(B)[9] <- "2019"


###Edad 14 a 18 años
P_1418SG<-subset(Proyecc_SG, Proyecc_SG$Edad >= 14 & Proyecc_SG$Edad <= 18) 
P_1418SG<-P_1418SG[,18:26]
C <- rbind(P_1418SG, c("Media", colSums(P_1418SG[,1:9])))
C<-C[11,]
names(C)[1] <- "Rango Etario"
names(C)[2] <- "2012"
names(C)[3] <- "2013"
names(C)[4] <- "2014"
names(C)[5] <- "2015"
names(C)[6] <- "2016"
names(C)[7] <- "2017"
names(C)[8] <- "2018"
names(C)[9] <- "2019"


###Edad 18 a 25 años
P_1825SG<-subset(Proyecc_SG, Proyecc_SG$Edad >= 18 & Proyecc_SG$Edad <= 25) 
P_1825SG<-P_1825SG[,18:26]
D <- rbind(P_1825SG, c("Superior", colSums(P_1825SG[,1:9])))
D<-D[17,]
names(D)[1] <- "Rango Etario"
names(D)[2] <- "2012"
names(D)[3] <- "2013"
names(D)[4] <- "2014"
names(D)[5] <- "2015"
names(D)[6] <- "2016"
names(D)[7] <- "2017"
names(D)[8] <- "2018"
names(D)[9] <- "2019"


Pob_objetivo <- rbind(C, D)

y<-t(Pob_objetivo)
y<-y[-1,]
y_num <- as.numeric(y)


df<- as.data.frame(t(Pob_objetivo[,-1]))

names(df)[1] <- "Media"
names(df)[2] <- "Superior"

df$Media<-as.numeric(as.character(df$Media))
df$Superior<-as.numeric(as.character(df$Superior))

Becados$tasa_Media<-Becados$Media/df$Media
Becados$tasa_Superior<-Becados$Superior/df$Superior

### guardando data frame en CSV
write.csv(Becados,"/Users/joaquinfernandez/Documents/SxCDashboard/datos/Becados_tincidencia.csv", row.names = FALSE)

