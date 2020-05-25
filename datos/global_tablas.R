## Clear all
rm(list = ls())

## Setup
Sys.setlocale("LC_ALL", 'UTF-8')
library(shiny)
library(dplyr)
library(shinydashboard)
# library(bubbles)        # devtools::install_github("jcheng5/bubbles")
library(plotly)         # grafico tendencia gtrend
library(DT) 
library(colorspace)
library(scales)
# library(leaflet)
# library(leaflet.extras)
library(readr)
library(readxl)


options(digits=2)
options(scipen=100)



PathDatos<-"datos/"

pob_hm<-read.csv(paste(PathDatos,"Pob_HM.csv",sep=""))
pob_tipoeduc<-read.csv(paste(PathDatos,"Pob_TipoEduc.csv",sep=""))
Mat_hist_RII<-read.csv(paste(PathDatos,"Mat_hist_RII.csv",sep=""))
becados_tincidencia<-read.csv(paste(PathDatos,"Becados_tincidencia.csv",sep=""))


nomina <- read_delim(paste(PathDatos,"nomina.csv",sep=""), 
                     ";", escape_double = FALSE, trim_ws = TRUE)
carreras <- read_excel(paste(PathDatos,"Buscador-empleabilidad-e-ingresos_mifuturo_2020.xlsx",sep=""))
carreras<- carreras[1:(nrow(carreras)-3),]

### Importando datos contextuales de la región
Pob_RII <- read_csv(paste(PathDatos,"Pob_RII.csv",sep=""))
## Renombrando comuna
names(Pob_RII)[1]<-"Comuna"
names(Pob_RII)[5]<-"Pueblos originarios"
names(Pob_RII)[6]<-"Pob 4 a 6 años"
names(Pob_RII)[7]<-"Pob 7 a 14 años"
names(Pob_RII)[8]<-"Pob 15 a 18 años"
names(Pob_RII)[9]<-"Pob 19 a 25 años"

## Renombrando comuna
RII_Educ<-read_csv(paste(PathDatos,"RII_Educ.csv",sep=""))
names(RII_Educ)[1]<-"Comuna"
RII_Empleo<-read_csv(paste(PathDatos,"RII_Empleo.csv",sep=""))  
names(RII_Empleo)[1]<-"Comuna"
Mat_hist<-read_csv(paste(PathDatos,"Mat_hist_RII.csv",sep="")) 
names(Mat_hist)[1]<-"Comuna"

### Importar data de becas entregada por años
becas<-read_csv(paste(PathDatos,"Becas por anho - Hoja 1.csv",sep=""))

### Importar datos empleo
Des_ciudades <- read_csv(paste(PathDatos,"ENE_ciudades.csv",sep = ""))
Des_region <- read_csv(paste(PathDatos,"ENE_regiones.csv",sep = ""))

### Desempleo regional
Desempleo<-filter(Des_region[,c(4,6,8,9)], Des_region$Región == "Región de Antofagasta")

### Importa datos pobreza
Pob_Ing_2015 <- read_excel("datos/Casen2015.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Pob_Ing_2017 <- read_excel("datos/Casen2017.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Multi_2015<- read_excel("datos/Casen2015.xlsx", sheet = 2, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Multi_2017<- read_excel("datos/Casen2017.xlsx", sheet = 2, col_names = TRUE, col_types = NULL, na = "", skip = 0)

## Renombrando colunmas de Dataframes
names(Pob_Ing_2015)[3]<-"comuna"
names(Pob_Ing_2017)[3]<-"comuna"
names(Multi_2015)[3]<-"comuna"
names(Multi_2017)[3]<-"comuna"
names(Pob_Ing_2015)[5]<-"Pobreza_Ing"
names(Pob_Ing_2017)[5]<-"Pobreza_Ing"
names(Multi_2015)[5]<-"PobrezaMulti"
names(Multi_2017)[5]<-"PobrezaMulti"

### Trabajando en bases de Pobreza
Pobreza_Multi<-merge(Multi_2015,Multi_2017, by="comuna")
Pob_Multi<-filter(Pobreza_Multi[,c(1,5,12)], Pobreza_Multi$Región.x == "II de Antofagasta")
names(Pob_Multi)[2]<-"2015"
names(Pob_Multi)[3]<-"2017"

Pobreza_Ing<-merge(Pob_Ing_2015 ,Pob_Ing_2017, by="comuna")
Pov_Ing<-filter(Pobreza_Ing[,c(1,5,12)], Pobreza_Ing$Región.x == "II de Antofagasta")
names(Pov_Ing)[2]<-"2015"
names(Pov_Ing)[3]<-"2017"

Pov_RII<-merge(Pov_Ing, Pob_Multi, by="comuna")
names(Pov_RII)[2]<-"Ing_2015"
names(Pov_RII)[3]<-"Ing_2017"
names(Pov_RII)[4]<-"Multi_2015"
names(Pov_RII)[5]<-"Multi_2017"

HM_becarios<- nomina %>% group_by(Genero) %>% summarize(HM = n())
HM_becarios_porAño<- nomina %>% filter(Año=="2012")
HM_becarios_2012<- HM_becarios_porAño %>% group_by(Genero) %>% summarize("2012" = n())
HM_becarios_porAño<- nomina %>% filter(Año=="2013")
HM_becarios_2013<- HM_becarios_porAño %>% group_by(Genero) %>% summarize("2013" = n())
HM_becarios_porAño<- nomina %>% filter(Año=="2014")
HM_becarios_2014<- HM_becarios_porAño %>% group_by(Genero) %>% summarize("2014" = n())
HM_becarios_porAño<- nomina %>% filter(Año=="2015")
HM_becarios_2015<- HM_becarios_porAño %>% group_by(Genero) %>% summarize("2015" = n())
HM_becarios_porAño<- nomina %>% filter(Año=="2016")
HM_becarios_2016<- HM_becarios_porAño %>% group_by(Genero) %>% summarize("2016" = n())
HM_becarios_porAño<- nomina %>% filter(Año=="2017")
HM_becarios_2017<- HM_becarios_porAño %>% group_by(Genero) %>% summarize("2017" = n())
HM_becarios_porAño<- nomina %>% filter(Año=="2018")
HM_becarios_2018<- HM_becarios_porAño %>% group_by(Genero) %>% summarize("2018" = n())
HM_becarios_porAño<- nomina %>% filter(Año=="2019")
HM_becarios_2019<- HM_becarios_porAño %>% group_by(Genero) %>% summarize("2019" = n())
HM_becarios_Anual<-merge.data.frame(HM_becarios_2012,HM_becarios_2013,by="Genero",all.x=TRUE,all.y = TRUE)
HM_becarios_Anual<-merge.data.frame(HM_becarios_Anual,HM_becarios_2014,by="Genero",all.x=TRUE,all.y = TRUE)
HM_becarios_Anual<-merge.data.frame(HM_becarios_Anual,HM_becarios_2015,by="Genero",all.x=TRUE,all.y = TRUE)
HM_becarios_Anual<-merge.data.frame(HM_becarios_Anual,HM_becarios_2016,by="Genero",all.x=TRUE,all.y = TRUE)
HM_becarios_Anual<-merge.data.frame(HM_becarios_Anual,HM_becarios_2017,by="Genero",all.x=TRUE,all.y = TRUE)
HM_becarios_Anual<-merge.data.frame(HM_becarios_Anual,HM_becarios_2018,by="Genero",all.x=TRUE,all.y = TRUE)
HM_becarios_Anual<-merge.data.frame(HM_becarios_Anual,HM_becarios_2019,by="Genero",all.x=TRUE,all.y = TRUE)
HM_becarios_Anual <- as.data.frame(t(as.matrix(HM_becarios_Anual)))
names(HM_becarios_Anual) <- c("Femenino", "Masculino")
HM_becarios_Anual <- HM_becarios_Anual[-1,]
HM_becarios_Anual <- tibble::rownames_to_column(HM_becarios_Anual, "Año")
HM_becarios_Anual$Masculino[is.na(HM_becarios_Anual$Masculino)] <- 0
HM_becarios_Anual$Femenino[is.na(HM_becarios_Anual$Femenino)] <- 0
HM_becarios_Anual$Femenino <- as.numeric(HM_becarios_Anual$Femenino)
HM_becarios_Anual$Masculino <- as.numeric(HM_becarios_Anual$Masculino)