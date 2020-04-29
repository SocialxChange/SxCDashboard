library(shiny)
library(plotly)
library(dplyr)

load("~/SxCShinyApp/datos/nominaEmpleabilidad.RData")
load("~/SxCShinyApp/datos/ComunasAño.RData")


ui <- fluidPage(
  headerPanel('Pobreza por Comuna 2015-2017'),
  sidebarPanel(
    selectInput('comunasPer','Comuna', levels(nomina$Localidad), selected = levels(nomina$Localidad)[1], multiple=TRUE, selectize=FALSE)),
  mainPanel(
    plotlyOutput('plot')
  )
)

server <- function(input, output) {
  

  
  z <- reactive({
    nomina %>% filter(comuna %in% input$comunasPer) %>% select(`Empleabilidad 1er año`)
  })
  
  k <- reactive({
    nomina %>% filter(comuna %in% input$comunasPer) %>% select(`% titulados con continuidad de estudios`)
  })
  
  output$plot <- renderPlotly(
    fig <- plot_ly(
      type = 'scatterpolar',
      fill = 'toself'
    ) %>%
      add_trace(
        r = as.numeric(unlist(z())),
        theta = input$comunas, 
        name = '2015')  %>%
      add_trace(
        r = as.numeric(unlist(k())),
        theta = input$comunas, 
        name = '2017')
    
  )
  
}

shinyApp(ui,server)
