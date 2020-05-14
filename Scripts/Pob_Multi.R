### Creando grafico de burbujas para niveles de pobreza
 
if (interactive()) {
    
  ui <- fluidPage(
    titlePanel("Pobreza Multidimensional"),
    selectInput("variable", "Año:", choices=c("2015","2017"), selected = NULL),
    tableOutput("Pob_Multi")
  )
  
  server <- function(input, output, session) {
    output$Pob_Multi <- renderTable({
      Pob_Multi[, c("comuna", input$variable), drop = FALSE]
    }, rownames = TRUE)}
  
  shinyApp(ui, server)
  
}

