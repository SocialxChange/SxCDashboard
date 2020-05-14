rm(list=ls())

Sys.setlocale("LC_ALL", 'UTF-8')

setwd("/Users/joaquinfernandez/Documents/Proyecto Educacional Batuco/Bases de Datos/Censo")
getwd()

library(plyr)

## Se carga una sola vez
load("Censo2017_per.RData")

##Datos censales para region de Antofagasta
Censo_RII <- subset(Censo_personas, REGION == 2)

setwd("/Users/joaquinfernandez/Documents/SxCDashboard/datos")
save(Censo_RII,file="Censo_RII.Rda")

##Hacemos bases censales por comuna
# 2101	Antofagasta
# 2102	Mejillones
# 2103	Sierra Gorda
# 2104	Taltal
# 2201	Calama
# 2202	Ollagüe
# 2203	San Pedro de Atacama
# 2301	Tocopilla
# 2302	María Elena

### Load Censo RII
load("Censo_RII.Rda")


### Subset data censal por comuna
Antofagasta<-subset(Censo_RII, COMUNA == 2101)
Mejillones<-subset(Censo_RII, COMUNA == 2102)
Sierra_Gorda<-subset(Censo_RII, COMUNA == 2103)
Taltal<-subset(Censo_RII, COMUNA == 2104)
Calama<-subset(Censo_RII, COMUNA == 2201)
Ollagüe<-subset(Censo_RII, COMUNA == 2202)
SP_Atacama<-subset(Censo_RII, COMUNA == 2203)
Tocopilla<-subset(Censo_RII, COMUNA == 2301)
María_Elena<-subset(Censo_RII, COMUNA == 2302)

##########################################
##1. Creación de datos a nivel poblacional 
#########################################

### Valores de datos
P_A<-sum(Censo_RII$COMUNA == 2101)
P_Mej<-sum(Censo_RII$COMUNA == 2102)
P_SG<-sum(Censo_RII$COMUNA == 2103)
P_TT<-sum(Censo_RII$COMUNA == 2104)
P_C<-sum(Censo_RII$COMUNA == 2201)
P_O<-sum(Censo_RII$COMUNA == 2202)
P_SPA<-sum(Censo_RII$COMUNA == 2203)
P_Toc<-sum(Censo_RII$COMUNA == 2301)
P_ME<-sum(Censo_RII$COMUNA == 2302)

### Hombres
P_Ah<-sum(Antofagasta$P08 == 1)
P_Mejh<-sum(Mejillones$P08 == 1)
P_SGh<-sum(Sierra_Gorda$P08 == 1)
P_TTh<-sum(Taltal$P08 == 1)
P_Ch<-sum(Calama$P08 == 1)
P_Oh<-sum(Ollagüe$P08 == 1)
P_SPAh<-sum(SP_Atacama$P08 == 1)
P_Toch<-sum(Tocopilla$P08 == 1)
P_MEh<-sum(María_Elena$P08 == 1)

### Mujeres
P_Am<-sum(Antofagasta$P08 == 2)
P_Mejm<-sum(Mejillones$P08 == 2)
P_SGm<-sum(Sierra_Gorda$P08 == 2)
P_TTm<-sum(Taltal$P08 == 2)
P_Cm<-sum(Calama$P08 == 2)
P_Om<-sum(Ollagüe$P08 == 2)
P_SPAm<-sum(SP_Atacama$P08 == 2)
P_Tocm<-sum(Tocopilla$P08 == 2)
P_MEm<-sum(María_Elena$P08 == 2)

#####################
###Rangos etarios ###
#####################

#### 4 a 6 años
P_A_46<-sum(Antofagasta$P09 >= 4 & Antofagasta$P09 <= 6)
P_Mej_46<-sum(Mejillones$P09 >= 4 & Mejillones$P09 <= 6)
P_SG_46<-sum(Sierra_Gorda$P09 >= 4 & Sierra_Gorda$P09 <= 6)
P_TT_46<-sum(Taltal$P09 >= 4 & Taltal$P09 <= 6)
P_C_46<-sum(Calama$P09 >= 4 & Calama$P09 <= 6)
P_O_46<-sum(Ollagüe$P09 >= 4 & Ollagüe$P09 <= 6)
P_SPA_46<-sum(SP_Atacama$P09 >= 4 & SP_Atacama$P09 <= 6)
P_Toc_46<-sum(Tocopilla$P09 >= 4 & Tocopilla$P09 <= 6)
P_ME_46<-sum(María_Elena$P09 >= 4 & María_Elena$P09 <= 6)

#### 7 a 14 años
P_A_714<-sum(Antofagasta$P09 >= 7 & Antofagasta$P09 <= 14)
P_Mej_714<-sum(Mejillones$P09 >= 7 & Mejillones$P09 <= 14)
P_SG_714<-sum(Sierra_Gorda$P09 >= 7 & Sierra_Gorda$P09 <= 14)
P_TT_714<-sum(Taltal$P09 >= 7 & Taltal$P09 <= 14)
P_C_714<-sum(Calama$P09 >= 7 & Calama$P09 <= 14)
P_O_714<-sum(Ollagüe$P09 >= 7 & Ollagüe$P09 <= 14)
P_SPA_714<-sum(SP_Atacama$P09 >= 7 & SP_Atacama$P09 <= 14)
P_Toc_714<-sum(Tocopilla$P09 >= 7 & Tocopilla$P09 <= 14)
P_ME_714<-sum(María_Elena$P09 >= 7 & María_Elena$P09 <= 14)

#### 15 a 18 años
P_A_1518<-sum(Antofagasta$P09 >= 15 & Antofagasta$P09 <= 18)
P_Mej_1518<-sum(Mejillones$P09 >= 15 & Mejillones$P09 <= 18)
P_SG_1518<-sum(Sierra_Gorda$P09 >= 15 & Sierra_Gorda$P09 <= 18)
P_TT_1518<-sum(Taltal$P09 >= 15 & Taltal$P09 <= 18)
P_C_1518<-sum(Calama$P09 >= 15 & Calama$P09 <= 18)
P_O_1518<-sum(Ollagüe$P09 >= 15 & Ollagüe$P09 <= 18)
P_SPA_1518<-sum(SP_Atacama$P09 >= 15 & SP_Atacama$P09 <= 18)
P_Toc_1518<-sum(Tocopilla$P09 >= 15 & Tocopilla$P09 <= 18)
P_ME_1518<-sum(María_Elena$P09 >= 15 & María_Elena$P09 <= 18)

#### 19 a 25 años
P_A_1925<-sum(Antofagasta$P09 >= 19 & Antofagasta$P09 <= 25)
P_Mej_1925<-sum(Mejillones$P09 >= 19 & Mejillones$P09 <= 25)
P_SG_1925<-sum(Sierra_Gorda$P09 >= 19 & Sierra_Gorda$P09 <= 25)
P_TT_1925<-sum(Taltal$P09 >= 19 & Taltal$P09 <= 25)
P_C_1925<-sum(Calama$P09 >= 19 & Calama$P09 <= 25)
P_O_1925<-sum(Ollagüe$P09 >= 19 & Ollagüe$P09 <= 25)
P_SPA_1925<-sum(SP_Atacama$P09 >= 19 & SP_Atacama$P09 <= 25)
P_Toc_1925<-sum(Tocopilla$P09 >= 19 & Tocopilla$P09 <= 25)
P_ME_1925<-sum(María_Elena$P09 >= 19 & María_Elena$P09 <= 25)


########################
#### Pueblos Originarios
PO_A<-(sum(Antofagasta$P13 == 1)/P_A)*100
PO_Mej<-(sum(Mejillones$P13 == 1)/P_Mej)*100
PO_SG<-(sum(Sierra_Gorda$P13 == 1)/P_SG)*100
PO_TT<-(sum(Taltal$P13 == 1)/P_TT)*100
PO_C<-(sum(Calama$P13 == 1)/P_C)*100
PO_O<-(sum(Ollagüe$P13 == 1)/P_O)*100
PO_SPA<-(sum(SP_Atacama$P13 == 1)/P_SPA)*100
PO_Toc<-(sum(Tocopilla$P13 == 1)/P_Toc)*100
PO_ME<-(sum(María_Elena$P13 == 1)/P_ME)*100


### Creación de DataFrame con Población regional  
Pob_RII<- data.frame("Nom_Com" = c("Antofagasta","Mejillones","Sierra Gorda","Taltal","Calama","Ollagüe","SP Atacama", "Tocopilla","Maria Elena"), 
                     "Pob_Total" = c(P_A, P_Mej,P_SG,P_TT,P_C,P_O,P_SPA,P_Toc,P_ME),
                     "Hombre"=c(P_Ah, P_Mejh,P_SGh,P_TTh,P_Ch,P_Oh,P_SPAh,P_Toch,P_MEh),
                     "Mujeres"=c(P_Am, P_Mejm,P_SGm,P_TTm,P_Cm,P_Om,P_SPAm,P_Tocm,P_MEm),
                     "% Pob. que se considera perteneciente a pueblos originarios"=c(PO_A, PO_Mej,PO_SG,PO_TT,PO_C,PO_O,PO_SPA,PO_Toc,PO_ME),
                     "Población 4 a 6 años"=c(P_A_46, P_Mej_46,P_SG_46,P_TT_46,P_C_46,P_O_46,P_SPA_46,P_Toc_46,P_ME_46),
                     "Población 7 a 14 años"=c(P_A_714, P_Mej_714,P_SG_714,P_TT_714,P_C_714,P_O_714,P_SPA_714,P_Toc_714,P_ME_714),
                     "Población 15 a 18 años"=c(P_A_1518, P_Mej_1518,P_SG_1518,P_TT_1518,P_C_1518,P_O_1518,P_SPA_1518,P_Toc_1518,P_ME_1518),
                     "Población 19 a 25 años"=c(P_A_1925, P_Mej_1925,P_SG_1925,P_TT_1925,P_C_1925,P_O_1925,P_SPA_1925,P_Toc_1925,P_ME_1925)
                     )

### guardando data frame en CSV
write.csv(Pob_RII,"/Users/joaquinfernandez/Documents/SxCDashboard/datos/Pob_RII.csv", row.names = FALSE)

Pob_HM<-Pob_RII[,-c(2,5:9)]
write.csv(Pob_HM,"/Users/joaquinfernandez/Documents/SxCDashboard/datos/Pob_HM.csv", row.names = FALSE)

Pob_Educ<-Pob_RII[, -c(3:5)]

### Convirtiendo a numerico los datos
Pob_Educ$Pob_Total<-as.numeric(as.character(Pob_Educ$Pob_Total))
Pob_Educ$Población.4.a.6.años<-as.numeric(as.character(Pob_Educ$Población.4.a.6.años))
Pob_Educ$Población.7.a.14.años<-as.numeric(as.character(Pob_Educ$Población.7.a.14.años))
Pob_Educ$Población.15.a.18.años<-as.numeric(as.character(Pob_Educ$Población.15.a.18.años))
Pob_Educ$Población.19.a.25.años<-as.numeric(as.character(Pob_Educ$Población.19.a.25.años))


Pob_Educ$No_asiste<-Pob_Educ$Pob_Total-rowSums(Pob_Educ[,3:6])
Pob_Educ<-Pob_Educ[, -c(2,7)]

### Naming Columns
names(Pob_Educ)[1] <- "Comuna"
names(Pob_Educ)[2] <- "Parvulo"
names(Pob_Educ)[3] <- "Basica"
names(Pob_Educ)[4] <- "Media"
names(Pob_Educ)[5] <- "Superior"
write.csv(Pob_Educ,"/Users/joaquinfernandez/Documents/SxCDashboard/datos/Pob_TipoEduc.csv", row.names = FALSE)


                                            
##########################################
##2. Datos educacionales a nivel comunal 
#########################################

### Asiste
A_A<-sum(Antofagasta$P13 == 1)
A_Mej<-sum(Mejillones$P13 == 1)
A_SG<-sum(Sierra_Gorda$P13 == 1)
A_TT<-sum(Taltal$P13 == 1)
A_C<-sum(Calama$P13 == 1)
A_O<-sum(Ollagüe$P13 == 1)
A_SPA<-sum(SP_Atacama$P13 == 1)
A_Toc<-sum(Tocopilla$P13 == 1)
A_ME<-sum(María_Elena$P13 == 1)

### No Asiste
NA_A<-sum(Antofagasta$P13 == 2)
NA_Mej<-sum(Mejillones$P13 == 2)
NA_SG<-sum(Sierra_Gorda$P13 == 2)
NA_TT<-sum(Taltal$P13 == 2)
NA_C<-sum(Calama$P13 == 2)
NA_O<-sum(Ollagüe$P13 == 2)
NA_SPA<-sum(SP_Atacama$P13 == 2)
NA_Toc<-sum(Tocopilla$P13 == 2)
NA_ME<-sum(María_Elena$P13 == 2)

### Nunca Asistio
NuA_A<-sum(Antofagasta$P13 == 3)
NuA_Mej<-sum(Mejillones$P13 == 3)
NuA_SG<-sum(Sierra_Gorda$P13 == 3)
NuA_TT<-sum(Taltal$P13 == 3)
NuA_C<-sum(Calama$P13 == 3)
NuA_O<-sum(Ollagüe$P13 == 3)
NuA_SPA<-sum(SP_Atacama$P13 == 3)
NuA_Toc<-sum(Tocopilla$P13 == 3)
NuA_ME<-sum(María_Elena$P13 == 3)


RII_Educ<- data.frame("Nom_Com" = c("Antofagasta","Mejillones","Sierra Gorda","Taltal","Calama","Ollagüe","SP Atacama", "Tocopilla","Maria Elena"),
                      "Asiste" = c(A_A, A_Mej,A_SG,A_TT,A_C,A_O,A_SPA,A_Toc,A_ME),
                      "No Asiste" = c(NA_A, NA_Mej,NA_SG,NA_TT,NA_C,NA_O,NA_SPA,NA_Toc,NA_ME),
                      "Nunca Asistio" = c(NuA_A, NuA_Mej,NuA_SG,NuA_TT,NuA_C,NuA_O,NuA_SPA,NuA_Toc,NuA_ME)
                      )

### guardando data frame en CSV
write.csv(RII_Educ,"/Users/joaquinfernandez/Documents/SxCDashboard/datos/RII_Educ.csv", row.names = FALSE)




############################
#### Situacion laboral Censo ### * 2017

### % pob trabajando en empleo remunerado
L_A<-(sum(Antofagasta$P17 == 1)/P_A)*100
L_Mej<-(sum(Mejillones$P17 == 1)/P_Mej)*100
L_SG<-(sum(Sierra_Gorda$P17 == 1)/P_SG)*100
L_TT<-(sum(Taltal$P17 == 1)/P_TT)*100
L_C<-(sum(Calama$P17 == 1)/P_C)*100
L_O<-(sum(Ollagüe$P17 == 1)/P_O)*100
L_SPA<-(sum(SP_Atacama$P17 == 1)/P_SPA)*100
L_Toc<-(sum(Tocopilla$P17 == 1)/P_Toc)*100
L_ME<-(sum(María_Elena$P17 == 1)/P_ME)*100

### % empleo sin remuneracion
L2_A<-(sum(Antofagasta$P17 == 2)/P_A)*100
L2_Mej<-(sum(Mejillones$P17 == 2)/P_Mej)*100
L2_SG<-(sum(Sierra_Gorda$P17 == 2)/P_SG)*100
L2_TT<-(sum(Taltal$P17 == 2)/P_TT)*100
L2_C<-(sum(Calama$P17 == 2)/P_C)*100
L2_O<-(sum(Ollagüe$P17 == 2)/P_O)*100
L2_SPA<-(sum(SP_Atacama$P17 == 2)/P_SPA)*100
L2_Toc<-(sum(Tocopilla$P17 == 2)/P_Toc)*100
L2_ME<-(sum(María_Elena$P17 == 2)/P_ME)*100

#### Buscando Empleo
BE_A<-(sum(Antofagasta$P17 == 4)/P_A)*100
BE_Mej<-(sum(Mejillones$P17 == 4)/P_Mej)*100
BE_SG<-(sum(Sierra_Gorda$P17 == 4)/P_SG)*100
BE_TT<-(sum(Taltal$P17 == 4)/P_TT)*100
BE_C<-(sum(Calama$P17 == 4)/P_C)*100
BE_O<-(sum(Ollagüe$P17 == 4)/P_O)*100
BE_SPA<-(sum(SP_Atacama$P17 == 4)/P_SPA)*100
BE_Toc<-(sum(Tocopilla$P17 == 4)/P_Toc)*100
BE_ME<-(sum(María_Elena$P17 == 4)/P_ME)*100


### Estudiando
E_A<-(sum(Antofagasta$P17 == 5)/P_A)*100
E_Mej<-(sum(Mejillones$P17 == 5)/P_Mej)*100
E_SG<-(sum(Sierra_Gorda$P17 == 5)/P_SG)*100
E_TT<-(sum(Taltal$P17 == 5)/P_TT)*100
E_C<-(sum(Calama$P17 == 5)/P_C)*100
E_O<-(sum(Ollagüe$P17 == 5)/P_O)*100
E_SPA<-(sum(SP_Atacama$P17 == 5)/P_SPA)*100
E_Toc<-(sum(Tocopilla$P17 == 5)/P_Toc)*100
E_ME<-(sum(María_Elena$P17 == 5)/P_ME)*100

### Pensionado
P_A<-(sum(Antofagasta$P17 == 7)/P_A)*100
P_Mej<-(sum(Mejillones$P17 == 7)/P_Mej)*100
P_SG<-(sum(Sierra_Gorda$P17 == 7)/P_SG)*100
P_TT<-(sum(Taltal$P17 == 7)/P_TT)*100
P_C<-(sum(Calama$P17 == 7)/P_C)*100
P_O<-(sum(Ollagüe$P17 == 7)/P_O)*100
P_SPA<-(sum(SP_Atacama$P17 == 7)/P_SPA)*100
P_Toc<-(sum(Tocopilla$P17 == 7)/P_Toc)*100
P_ME<-(sum(María_Elena$P17 == 7)/P_ME)*100



RII_Empleo<- data.frame("Nom_Com" = c("Antofagasta","Mejillones","Sierra Gorda","Taltal","Calama","Ollagüe","SP Atacama", "Tocopilla","Maria Elena"),
                      "Remunerado" = c(L_A, L_Mej,L_SG,L_TT,L_C,L_O,L_SPA,L_Toc,L_ME),
                      "No Remunerado" = c(L2_A, L2_Mej,L2_SG,L2_TT,L2_C,L2_O,L2_SPA,L2_Toc,L2_ME),
                      "Buscando Empleo" = c(BE_A, BE_Mej,BE_SG,BE_TT,BE_C,BE_O,BE_SPA,BE_Toc,BE_ME),
                      "Estudiando" = c(BE_A, BE_Mej,BE_SG,BE_TT,BE_C,BE_O,BE_SPA,BE_Toc,BE_ME),
                      "Pensionado" = c(P_A, P_Mej,P_SG,P_TT,P_C,P_O,P_SPA,P_Toc,P_ME)
)

RII_Empleo$Otro<-(100-(RII_Empleo$Remunerado+RII_Empleo$No.Remunerado+RII_Empleo$Buscando.Empleo+RII_Empleo$Estudiando+RII_Empleo$Pensionado))

### guardando data frame en CSV
write.csv(RII_Empleo,"/Users/joaquinfernandez/Documents/SxCDashboard/datos/RII_Empleo.csv", row.names = FALSE)


