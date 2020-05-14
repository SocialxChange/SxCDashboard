
    
 fluidPage(
    titlePanel("Pobreza por Ingresos"),
    selectInput("variable", "Año:", choices=c("2015","2017"), selected = NULL),
    tableOutput("Pov_Ing")
  )
  
 
    output$Pov_Ing <- renderTable({
      Pov_Ing[, c("comuna", input$variable), drop = FALSE]
    }, rownames = TRUE)
  

  