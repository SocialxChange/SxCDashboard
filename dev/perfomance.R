library(shiny)
library(plotly)
library(dplyr)

PathDatos<-Sys.getenv("PathDatos")

load(paste(PathDatos,"nominaEmpleabilidad.RData",sep=""))
load(paste(PathDatos,"ComunasAño.RData",sep=""))


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
