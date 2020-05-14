### Creando grafico de burbujas para niveles de pobreza
 
if (interactive()) {
    
  ui <- fluidPage(
    titlePanel("Pobreza por Ingresos"),
    selectInput("variable", "Año:", choices=c("2015","2017"), selected = NULL),
    tableOutput("Pov_Ing")
  )
  
  server <- function(input, output, session) {
    output$Pov_Ing <- renderTable({
      Pov_Ing[, c("comuna", input$variable), drop = FALSE]
    }, rownames = TRUE)}
  
  shinyApp(ui, server)
  
}


if (interactive()) {
  
  ui <- fluidPage(
    titlePanel("Pobreza por Ingresos"),
     leafletOutput("my_tmap"))

# in server part
  
  server <- function(input, output, session) {
   output$my_tmap = renderLeaflet({
  tm <- tm_shape(World) + tm_polygons("HPI", legend.title = "Happy Planet Index")
  tmap_leaflet(tm)  
  })
  }

   shinyApp(ui, server)
   
  }
