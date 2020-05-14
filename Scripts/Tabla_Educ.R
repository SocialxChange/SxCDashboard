if (interactive()) {
  
  ui <- fluidPage(
    titlePanel("Situación Educacional por comunas"),
    DTOutput('Educ')
  )
  
server <- function(input, output, session) {
  output$Educ <-renderDT(RII_Educ, options = list(lengthChange = FALSE), rownames=FALSE, caption = "") 
  }
  
  
  shinyApp(ui, server)
}

