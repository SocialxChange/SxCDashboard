
    
  ### tabla interactiva con filtros.
  
    fluidPage(
      titlePanel("Desempleo historico regional"),
      fluidRow(column(4, div(dataTableOutput("Desempleo")))))
    
    
    
      output$Desempleo <-renderDT(Desempleo, # reactive data
      class = "display nowrap compact", # style
      filter = "top", # location of column filters
      options = list(  # options
        scrollX = TRUE # allow user to scroll wide tables horizontally
      ))
    
 

