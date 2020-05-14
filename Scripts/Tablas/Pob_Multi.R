fluidPage(
    titlePanel("Pobreza Multidimensional"),
    selectInput("variable", "Año:", choices=c("2015","2017"), selected = NULL),
    tableOutput("Pob_Multi")
  )
  

    output$Pob_Multi <- renderTable({
      Pob_Multi[, c("comuna", input$variable), drop = FALSE]
    }, rownames = TRUE)

