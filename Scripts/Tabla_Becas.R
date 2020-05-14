if (interactive()) {
  
  ui <- fluidPage(
    titlePanel("Personas beneficiadas por programa"),
    DTOutput('becas')
  )
  
server <- function(input, output, session) {
  output$becas <-renderDT(becas, options = list(lengthChange = FALSE), rownames=FALSE, caption = "") 
  }
  
  
  shinyApp(ui, server)
}

