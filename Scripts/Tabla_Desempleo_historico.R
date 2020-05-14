if (interactive()) {
    
  ### tabla interactiva con filtros.
  
    ui <- fluidPage(
      titlePanel("Desempleo historico regional"),
      fluidRow(column(4, div(dataTableOutput("Desempleo")))))
    
    
    server <- function(input, output, session) {
      output$Desempleo <-renderDT(Desempleo, # reactive data
      class = "display nowrap compact", # style
      filter = "top", # location of column filters
      options = list(  # options
        scrollX = TRUE # allow user to scroll wide tables horizontally
      ))}
    
    
    shinyApp(ui, server)
  }

