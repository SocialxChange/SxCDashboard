library(shiny)
library(plotly)
library(dplyr)

data <- data.frame(comuna=c('A','B','C', 'D', 'E', 'F'), pobreza=runif(6)*100)

ui <- fluidPage(
  headerPanel('Pobreza por comuna 2015-2017'),
  sidebarPanel(
    selectInput('comunas','Comuna', levels(data$comuna), multiple=TRUE, selectize=TRUE),
    selected = levels(data$comunas)[1:3]),
  mainPanel(
    plotlyOutput('plot')
  )
)

server <- function(input, output) {
  
  x <- reactive({
    data %>% filter(comuna %in% input$comunas) %>% select(pobreza)
  })
  
  
  output$plot <- renderPlotly(
    fig <- plot_ly(
      r = as.numeric(unlist(data %>% filter(comuna %in% levels(data$comuna)[1:5]) %>% select(pobreza))), 
      theta= levels(data$comuna)[1:5],
      type = 'scatterpolar',
      fill = 'toself'),
    
    fig <- fig %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,100)
          )
        ),
        showlegend = F
      )
  )
  
}

shinyApp(ui,server)

