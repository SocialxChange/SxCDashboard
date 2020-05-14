if (interactive()) {
  
  ui <- fluidPage(
    checkboxGroupInput("variable", "Tipos población:",
                       c("Hombre" = "Hombre",
                          "Mujer" = "Mujeres",
                          "Originarios" = "Pueblos originarios",
                          "Parvulo" = "Pob 4 a 6 años",
                          "Básica"  = "Pob 7 a 14 años",
                          "Media"= "Pob 15 a 18 años",
                          "Superior" = "Pob 19 a 25 años")),
    tableOutput("data")
  )
  
  server <- function(input, output, session) {
    output$data <- renderTable({
      Pob_RII[, c("Comuna", "Pob_Total", input$variable), drop = FALSE]
    }, rownames = TRUE)
  }
  
  shinyApp(ui, server)
}
    
