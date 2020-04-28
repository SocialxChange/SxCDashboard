library(shiny)
library(plotly)
library(dplyr)

data <- data.frame(comuna=LETTERS[1:10], pobreza15=runif(10)*100, pobreza17=runif(10)*100)

ui <- fluidPage(
  headerPanel('Pobreza por Comuna 2015-2017'),
  sidebarPanel(
    selectInput('comunas','Comuna', levels(data$comuna), selected = levels(data$comuna)[1:3], multiple=TRUE, selectize=TRUE)),
  mainPanel(
    plotlyOutput('plot')
  )
)

server <- function(input, output) {
  

  
  z <- reactive({
    data %>% filter(comuna %in% input$comunas) %>% select(pobreza15)
  })
  
  k <- reactive({
    data %>% filter(comuna %in% input$comunas) %>% select(pobreza17)
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
