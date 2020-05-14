 fluidPage(
      titlePanel("Matriculas por año"),
      fluidRow(column(7, div(dataTableOutput("Mat")))))
    
    
   
      output$Mat <-renderDT(Mat_hist, # reactive data
      class = "display nowrap compact", # style
      filter = "top", # location of column filters
      options = list(  # options
        scrollX = TRUE # allow user to scroll wide tables horizontally
      )
      )
      
       
    