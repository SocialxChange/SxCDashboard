rm(list=ls())

Sys.setlocale("LC_ALL", 'UTF-8')

setwd("~/Documents/Proyecto Educacional Batuco/Bases de Datos/Datos Matricula")

### Cargando bases
#Año 2019
Mat_nacional_2019 = read.csv("mat_2019.csv", header = TRUE)
Mat_nacional_2018 = read.csv("mat_2018.csv", header = TRUE)
Mat_nacional_2017 = read.csv("mat_2017.csv", header = TRUE)
Mat_nacional_2016 = read.csv("mat_2016.csv", header = TRUE)
Mat_nacional_2015 = read.csv("mat_2015.csv", header = TRUE)
Mat_nacional_2014 = read.csv("mat_2014.csv", header = TRUE)
Mat_nacional_2013 = read.csv("mat_2013.csv", header = TRUE)
Mat_nacional_2012 = read.csv("mat_2014.csv", header = TRUE)


####### Subset de matricula por comuna ####
Mat_RII_2019 <- subset(Mat_nacional_2019, COD_REG_RBD == 2) 
Mat_RII_2018 <- subset(Mat_nacional_2018, COD_REG_RBD == 2) 
Mat_RII_2017 <- subset(Mat_nacional_2017, COD_REG_RBD == 2) 
Mat_RII_2016 <- subset(Mat_nacional_2016, cod_reg_rbd == 2) 
Mat_RII_2015 <- subset(Mat_nacional_2015, cod_reg_rbd == 2) 
Mat_RII_2014 <- subset(Mat_nacional_2014, cod_reg_rbd == 2) 
Mat_RII_2013 <- subset(Mat_nacional_2013, cod_reg_rbd == 2) 
Mat_RII_2012 <- subset(Mat_nacional_2012, cod_reg_rbd == 2) 



####### Matriculas totales por comuna ####

### Matricula 2019
Mat_com_2019<-aggregate(list(Mat_RII_2019$MAT_ENS_1, Mat_RII_2019$MAT_ENS_2,Mat_RII_2019$MAT_ENS_5,Mat_RII_2019$MAT_ENS_7,Mat_RII_2019$MAT_TOTAL), by = list(Mat_RII_2019$NOM_COM_RBD), sum)
Mat_com_2019$año<-2019
names(Mat_com_2019)[1] <- "comuna"
names(Mat_com_2019)[2] <- "Parvulo"
names(Mat_com_2019)[3] <- "Basica"
names(Mat_com_2019)[4] <- "Media HC"
names(Mat_com_2019)[5] <- "Media TP"
names(Mat_com_2019)[6] <- "Total"

### Matricula 2018
Mat_com_2018<-aggregate(list(Mat_RII_2018$MAT_ENS_1, Mat_RII_2018$MAT_ENS_2,Mat_RII_2018$MAT_ENS_5,Mat_RII_2018$MAT_ENS_7,Mat_RII_2018$MAT_TOTAL), by = list(Mat_RII_2018$NOM_COM_RBD), sum)
Mat_com_2018$año<-2018
names(Mat_com_2018)[1] <- "comuna"
names(Mat_com_2018)[2] <- "Parvulo"
names(Mat_com_2018)[3] <- "Basica"
names(Mat_com_2018)[4] <- "Media HC"
names(Mat_com_2018)[5] <- "Media TP"
names(Mat_com_2018)[6] <- "Total"

### Matricula 2017
Mat_com_2017<-aggregate(list(Mat_RII_2017$MAT_ENS_1, Mat_RII_2017$MAT_ENS_2,Mat_RII_2017$MAT_ENS_5,Mat_RII_2017$MAT_ENS_7,Mat_RII_2017$MAT_TOTAL), by = list(Mat_RII_2017$NOM_COM_RBD), sum)
Mat_com_2017$año<-2017
names(Mat_com_2017)[1] <- "comuna"
names(Mat_com_2017)[2] <- "Parvulo"
names(Mat_com_2017)[3] <- "Basica"
names(Mat_com_2017)[4] <- "Media HC"
names(Mat_com_2017)[5] <- "Media TP"
names(Mat_com_2017)[6] <- "Total"

### Matricula 2016
Mat_com_2016<-aggregate(list(Mat_RII_2016$mat_ens_1, Mat_RII_2016$mat_ens_2,Mat_RII_2016$mat_ens_5,Mat_RII_2016$mat_ens_7,Mat_RII_2016$mat_total), by = list(Mat_RII_2016$nom_com_rbd), sum)
Mat_com_2016$año<-2016
names(Mat_com_2016)[1] <- "comuna"
names(Mat_com_2016)[2] <- "Parvulo"
names(Mat_com_2016)[3] <- "Basica"
names(Mat_com_2016)[4] <- "Media HC"
names(Mat_com_2016)[5] <- "Media TP"
names(Mat_com_2016)[6] <- "Total"

### Matricula 2015
Mat_com_2015<-aggregate(list(Mat_RII_2015$mat_ens_1, Mat_RII_2015$mat_ens_2,Mat_RII_2015$mat_ens_5,Mat_RII_2015$mat_ens_7,Mat_RII_2015$mat_total), by = list(Mat_RII_2015$nom_com_rbd), sum)
Mat_com_2015$año<-2015
names(Mat_com_2015)[1] <- "comuna"
names(Mat_com_2015)[2] <- "Parvulo"
names(Mat_com_2015)[3] <- "Basica"
names(Mat_com_2015)[4] <- "Media HC"
names(Mat_com_2015)[5] <- "Media TP"
names(Mat_com_2015)[6] <- "Total"

### Matricula 2014
Mat_com_2014<-aggregate(list(Mat_RII_2014$mat_ens_1, Mat_RII_2014$mat_ens_2,Mat_RII_2014$mat_ens_5,Mat_RII_2014$mat_ens_7,Mat_RII_2014$mat_total), by = list(Mat_RII_2014$nom_com_rbd), sum)
Mat_com_2014$año<-2014
names(Mat_com_2014)[1] <- "comuna"
names(Mat_com_2014)[2] <- "Parvulo"
names(Mat_com_2014)[3] <- "Basica"
names(Mat_com_2014)[4] <- "Media HC"
names(Mat_com_2014)[5] <- "Media TP"
names(Mat_com_2014)[6] <- "Total"

### Matricula 2013
Mat_com_2013<-aggregate(list(Mat_RII_2013$mat_ens_1, Mat_RII_2013$mat_ens_2,Mat_RII_2013$mat_ens_5,Mat_RII_2013$mat_ens_7,Mat_RII_2013$mat_total), by = list(Mat_RII_2013$nom_com_rbd), sum)
Mat_com_2013$año<-2013
names(Mat_com_2013)[1] <- "comuna"
names(Mat_com_2013)[2] <- "Parvulo"
names(Mat_com_2013)[3] <- "Basica"
names(Mat_com_2013)[4] <- "Media HC"
names(Mat_com_2013)[5] <- "Media TP"
names(Mat_com_2013)[6] <- "Total"

### Matricula 2012
Mat_com_2012<-aggregate(list(Mat_RII_2012$mat_ens_1, Mat_RII_2012$mat_ens_2,Mat_RII_2012$mat_ens_5,Mat_RII_2012$mat_ens_7,Mat_RII_2012$mat_total), by = list(Mat_RII_2012$nom_com_rbd), sum)
Mat_com_2012$año<-2012
names(Mat_com_2012)[1] <- "comuna"
names(Mat_com_2012)[2] <- "Parvulo"
names(Mat_com_2012)[3] <- "Basica"
names(Mat_com_2012)[4] <- "Media HC"
names(Mat_com_2012)[5] <- "Media TP"
names(Mat_com_2012)[6] <- "Total"


library(gtools)
### Creando panel para matriculas historicas en la regioón
Panel_Mat_12_19<- smartbind(Mat_com_2019, Mat_com_2018, Mat_com_2017, Mat_com_2016,Mat_com_2015, Mat_com_2014, Mat_com_2013, Mat_com_2012)

### Unificando nombres de comunas en el panel
Panel_Mat_12_19$comuna[grep("OLLAG", Panel_Mat_12_19$comuna)] <- "OLLAGÜE"
Panel_Mat_12_19$comuna[grep("MAR", Panel_Mat_12_19$comuna)] <- "MARIA ELENA"

### guardando data frame en CSV
write.csv(Panel_Mat_12_19,"/Users/joaquinfernandez/Documents/SxCDashboard/datos/Mat_hist_RII.csv", row.names = FALSE)
