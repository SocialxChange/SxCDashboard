 fluidPage(
    checkboxGroupInput("variable", "Tipos empleo en % Pob:",
                       c("Empleo no remunerado" = "No.Remunerado",
                         "Desempleado" = "Buscando.Empleo",
                         "Estudiando",
                         "Pensionado",
                         "Otro")),
    tableOutput("data_Emp")
  )
  
    output$data_Emp <- renderTable({
      RII_Empleo[, c("Comuna", "Remunerado", input$variable), drop = FALSE]
    }, rownames = TRUE)
