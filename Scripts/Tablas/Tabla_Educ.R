
  
  fluidPage(
    titlePanel("Situación Educacional por comunas"),
    DTOutput('Educ')
  )
  

  output$Educ <-renderDT(RII_Educ, options = list(lengthChange = FALSE), rownames=FALSE, caption = "") 
  
  
  
  
