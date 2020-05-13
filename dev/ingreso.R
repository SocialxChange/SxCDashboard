library(shiny)
library(plotly)
library(dplyr)

PathDatos<-Sys.getenv("PathDatos")

load(paste(PathDatos,"nominaEmpleabilidad.RData",sep=""))
load(paste(PathDatos,"ComunasAño.RData",sep=""))

# Cargamos credenciales de .Renviron
api_key<-Sys.getenv("INEkey")
base_url <- "http://api.datosabiertos.ine.cl/api/v2/datastreams/"
# Consultamos division Político Administrativa
## Usando paquete junr 
### install.packages("devtools")
### devtools::install_github("FvD/junr")
library("junr")
# https://mran.microsoft.com/snapshot/2017-02-04/web/packages/junr/README.html
dataINE <- get_data(base_url, api_key,"INGRE-MEDIO-MENSU-DE-LOS")
# https://www.rforexcelusers.com/remove-currency-dollar-sign-r/
dataINE$`Límite inferior` <- gsub("\\$", "", dataINE$`Límite inferior`)
dataINE$`Límite inferior` <- as.numeric(gsub("\\,", "", dataINE$`Límite inferior`))

dataINE$`Límite superior` <- gsub("\\$", "", dataINE$`Límite superior`)
dataINE$`Límite superior` <- as.numeric(gsub("\\,", "", dataINE$`Límite superior`))


attach(nomina)
ingresosAño<-aggregate(nomina, by=list(Año),
                      FUN=mean, na.rm=TRUE)
ingresosAño <- ingresosAño[,c(4,11:12)]
ingresosAño <- ingresosAño[which(ingresosAño$Año!=2014),]
ingresosAño$medio2017min <- dataINE$`Límite inferior`[dataINE$Región=="Antofagasta"]
ingresosAño$medio2017max <- dataINE$`Límite superior`[dataINE$Región=="Antofagasta"]


fig <- plot_ly(ingresosAño, x = ~Año, y = ~ingmax, type = 'scatter', mode = 'lines',
               line = list(color = 'rgba(0,100,80,1)'),
               showlegend = FALSE, name = 'Máximo') %>% 
              add_trace(y = ~ingmin, type = 'scatter', mode = 'lines',
                         fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'rgba(0,100,80,1)'),
                         showlegend = FALSE, name = 'Mínimo') %>% 
  layout(title = "Ingresos mínimos y máximos de beneficiarios",
         paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
         xaxis = list(title = "Año",
                      gridcolor = 'rgb(255,255,255)',
                      showgrid = TRUE,
                      showline = FALSE,
                      showticklabels = TRUE,
                      tickcolor = 'rgb(127,127,127)',
                      ticks = 'outside',
                      zeroline = FALSE),
         yaxis = list(title = "Ingresos (Miles de pesos)",
                      gridcolor = 'rgb(255,255,255)',
                      showgrid = TRUE,
                      showline = FALSE,
                      showticklabels = TRUE,
                      tickcolor = 'rgb(127,127,127)',
                      ticks = 'outside',
                      zeroline = FALSE))
fig

### Falta agregar lineas con datos INE 2017

add_trace(y = ~medio2017min, type = 'scatter', mode = 'lines',
          fill = 'tonexty', fillcolor='rgba(0,100,80,0.4)', line = list(color = 'rgba(0,100,80,1)'),
          showlegend = FALSE, name = 'Mínimo') %>% 
  add_trace(y = ~medio2017max, type = 'scatter', mode = 'lines',
            fill = 'tonexty', fillcolor='rgba(0,100,80,0.4)', line = list(color = 'rgba(0,100,80,1)'),
            showlegend = FALSE, name = 'Mínimo') %>% 
  

ui <- fluidPage(
  headerPanel('Desempeño esperado beneficiarios 2015-2017'),
  sidebarPanel(
    selectInput('indicador','Métrica', c("empleabilidad", "continuidad"), selected = "empleabilidad", multiple=FALSE, selectize=FALSE)),
  mainPanel(
    plotlyOutput('plot')
  )
)

server <- function(input, output) {
  
  
  
  z <- reactive({
    nomina %>% filter(comuna %in% input$comunasPer) %>% select(empleabilidad)
  })
  
  k <- reactive({
    nomina %>% filter(comuna %in% input$comunasPer) %>% select(continuidad)
  })
  
  output$plot <- renderPlotly(
    fig <- plot_ly(x = ~as.numeric(unlist(aggcomunas %>% filter(comuna=="Baquedano") %>% select(Año))), 
                   y = ~as.numeric(unlist(aggcomunas %>% filter(comuna=="Baquedano") %>% select(input$indicador))), 
                   name = 'Baquedano', type = 'scatter', mode = 'lines', fill = 'tonexty',fillcolor = '#5BCBFD') %>% 
      add_trace(x = ~as.numeric(unlist(aggcomunas %>% filter(comuna=="Sierra Gorda") %>% select(Año))), 
                y = ~as.numeric(unlist(aggcomunas %>% filter(comuna=="Sierra Gorda") %>% select(input$indicador))), 
                name = 'Sierra Gorda', fillcolor = '#EFC000') %>% 
      layout(title = 'Desempeño esperado beneficiarios',
             xaxis = list(title = "Año",
                          showgrid = FALSE),
             yaxis = list(title = input$indicador,
                          showgrid = FALSE,
                          ticksuffix = ''))
  )
  
}

shinyApp(ui,server)
