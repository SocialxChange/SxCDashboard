# Opción global de ruta de archivos de datos:
# PathDatos<-"datos/"
library(dplyr)
library(tidyverse)
library(fuzzyjoin)

# Cargamos datos originales
nomina <- read_delim(paste(PathDatos,"nomina.csv",sep=""), 
                     ";", escape_double = FALSE, trim_ws = TRUE)
carreras <- read_excel(paste(PathDatos,"Buscador-empleabilidad-e-ingresos_mifuturo_2020.xlsx",sep=""))
carreras<- carreras[1:(nrow(carreras)-3),]
## Reemplazamos carrera en nomina no existente en base de carreras
nomina$Carrera[which(nomina$Carrera=="Educadora de Párvulos")] <- "Educación Parvularia"
nomina$Carrera[which(nomina$Carrera=="Electricidad y Electrónica Industrial")] <- "Técnico en Electricidad y Electrónica Industrial"
nomina$Carrera[which(nomina$Carrera=="Ingeniería Geomensor")] <- "Ingeniería en Geomensura"
nomina$Carrera[which(nomina$Carrera=="Gastronomia Internacional y Tradicional Chilena")] <- "Técnico en Cocina Internacional y Tradicional Chilena"
nomina$Carrera[which(nomina$Carrera=="Ing. Prevención de Riesgo")] <- "Ingeniería en Prevención de Riesgos"
nomina$Carrera[which(nomina$Carrera=="Pedagogía General Básica")] <- "Pedagogía en Educación General Básica"
nomina$Carrera[which(nomina$Carrera=="Secretariado Ejecutivo en Computación")] <- "Secretariado Ejecutivo Bilingüe"
nomina$Carrera[which(nomina$Carrera=="Técnico Ejecución en Administración")] <- "Ingeniería de Ejecución en Administración"
nomina$Carrera[which(nomina$Carrera=="Técnico en Operador Planta")] <- "Técnico en Operaciones Mineras"
nomina$Carrera[which(nomina$Carrera=="Técnico Mecánico")] <- "Ingeniería de Ejecución Mecánica"

# Extraemos las carreras de los beneficiarios
carreraBen<-data.frame(carrera=levels(as.factor(nomina$Carrera)))
## Hay 29 beneficiarios y 23 carreras 


# Extraemos los nombres genéricos de carreras
carrerasOficial<-data.frame(carreraGenerica=levels(as.factor(carreras$`Nombre carrera`)))
## Hay 177 nombres genericos sobre 1479 carreras

# Hacemos un primer fuzzy join
## Con max_dist=3 psicología tiene más de un match
joined <- carreraBen %>%
  stringdist_inner_join(carrerasOficial, by = c(carrera = "carreraGenerica"), max_dist = 2)

# Agregamos Equivalencia
carreraBen <- merge(carreraBen,joined, by.x = "carrera", by.y="carrera", all.x=TRUE)

# Hacemos un segundo fuzzy join
## Con max_dist=5 hay matchs incorrectos
joined2 <- carreraBen %>% filter(is.na(carreraGenerica)) %>% select(carrera) %>%
  stringdist_inner_join(carrerasOficial, by = c(carrera = "carreraGenerica"), max_dist = 4)

# Agregamos nuevas equivalencias
carreraBen <- merge(carreraBen,joined2, by.x = "carrera", by.y="carrera", all.x=TRUE)

# Juntamos equivalencias en una sola columna
carreraBen <- carreraBen %>% mutate(carreraGenerica = coalesce(carreraGenerica.x,carreraGenerica.y)) %>%
  select(carrera, carreraGenerica)

##Esto permite ver potenciales equivalencias
# carreraBen$equiv <- ""
# for(i in 1:dim(carreraBen)[1]) {
#   if(is.na(carreraBen$carreraGenerica[i])){
#     x <- agrep(carreraBen$carrera[i], carrerasOficial$carrera,
#                ignore.case=TRUE, value=TRUE,
#                max.distance = 0.2, useBytes = TRUE)
#     print(x)
#     x <- paste0(x,"")
#     carreraBen$equiv[i] <- x
#   }
#   
# } 

nomina <-  merge(nomina, carreraBen, by.x = "Carrera", by.y="carrera")

# Nos restringimos a carreras de los beneficiarios
carreras <- carreras %>% filter(`Nombre carrera` %in% carreraBen$carreraGenerica)
# Decodificamos intervalo ingresos al 4 año de titulaciòn
## Separamos minimo de máximo
carreras<- carreras %>% 
  separate(`Ingreso promedio al 4° año de titulación`, into = c("ingmin", "ingmax"), sep = " a ")
## Guardamos el valor original
carreras$ingresoMin <- carreras$ingmin
## Extraemos "De $"
carreras$ingmin <- substring(carreras$ingmin,5)
## Separamos el compomente millón del número
carreras <- carreras %>% separate(ingmin, into = c("ingminM", "ingminMil"), sep=" millón")
## Guardamos Variable con información de si tiene dígito millón
carreras$sinMillón <- carreras$ingminMil
## Extraemos los miles para los que NO tienen millón
carreras$ingminMil[is.na(carreras$sinMillón)] <- substring(carreras$ingresoMin[is.na(carreras$sinMillón)],5,7)
## Reemplazamos por NA los que no tienen millón
carreras$ingminM[is.na(carreras$sinMillón)] <- NA
## Extraemos los miles para los que TIENEN millón
carreras$ingminMil[!is.na(carreras$sinMillón)] <- substring(carreras$ingminMil[!is.na(carreras$sinMillón)],2,4)
## Colapsamos digito del millón y cien mil en una sola variable
carreras <- carreras %>% 
  unite(ingmin, c("ingminM", "ingminMil"),sep = "", na.rm=TRUE)
## Reemplazamos por mil los valores 1 millón
carreras$ingmin[which(carreras$sinMillón=="")] <- 1000
## Reemplazamos por NA donde no hay información aka s/n
carreras$ingmin[which(carreras$ingmin=="")] <- NA


attach(carreras)
aggcarreras <-aggregate(carreras, by=list(`Nombre carrera`),
                    FUN=mean, na.rm=TRUE)
aggcarreras <- aggcarreras[,c(1,8,11)]

