fluidPage(
    titlePanel("Personas beneficiadas por programa"),
    DTOutput('becas')
  )
  
  output$becas <-renderDT(becas, options = list(lengthChange = FALSE), rownames=FALSE, caption = "") 
 
