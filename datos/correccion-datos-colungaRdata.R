Sys.setlocale("LC_ALL", 'UTF-8')

setwd("~/SxcDashboard/datos")

load("colunga.RData")

colunga$REGION[which(colunga$REGION=="Bío - Bío" | 
                       colunga$REGION=="Bio Bio" | 
                       colunga$REGION=="Bío Bío" | 
                       colunga$REGION=="Biobio")]<-"Bío Bío"