library(shiny)
library(plotly)
library(dplyr)

data <- data.frame(comuna=c('A','B','C', 'D', 'E', 'F'), pobreza=runif(6)*100, desempleo=runif(6)*100)

ui <- fluidPage(
  headerPanel('Example'),
  sidebarPanel(
    selectInput('xcol','X Variable', names(mtcars)),
    selectInput('ycol','Y Variable', names(mtcars)),
    selectInput('comunas','Z Variable', levels(data$comuna), selected = levels(data$comuna)[1:3], multiple=TRUE, selectize=TRUE)),
  mainPanel(
    plotlyOutput('plot'),
    plotlyOutput('plot2')
  )
)

server <- function(input, output) {
  
  x <- reactive({
    mtcars[,input$xcol]
  })
  
  y <- reactive({
    mtcars[,input$ycol]
  })
  
  z <- reactive({
    data %>% filter(comuna %in% input$comunas) %>% select(pobreza)
  })
  
  output$plot <- renderPlotly(
    plot1 <- plot_ly(
      x = x(),
      y = y(), 
      type = 'scatter',
      mode = 'markers')
  )
  
  output$plot2 <- renderPlotly(
    plot1 <- plot_ly(
      r = as.numeric(unlist(z())),
      theta = letters[1:length(as.numeric(unlist(z())))], 
      type = 'scatterpolar',
      fill = 'toself')
  )
  
}

shinyApp(ui,server)
