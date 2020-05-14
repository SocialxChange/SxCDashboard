if (interactive()) {
  
  ui <- fluidPage(
    checkboxGroupInput("variable", "Tipos empleo en % Pob:",
                       c("Empleo no remunerado" = "No.Remunerado",
                         "Desempleado" = "Buscando.Empleo",
                         "Estudiando",
                         "Pensionado",
                         "Otro")),
    tableOutput("data_Emp")
  )
  
  server <- function(input, output, session) {
    output$data_Emp <- renderTable({
      RII_Empleo[, c("Nom_Com", "Remunerado", input$variable), drop = FALSE]
    }, rownames = TRUE)
  }
  
  shinyApp(ui, server)
}
