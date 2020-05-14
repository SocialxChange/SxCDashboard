library(shiny)  
library(ECharts2Shiny)
  
Gender <- read_csv("Pob_HM.csv")
names(Gender)[1] <- "Comuna"
Gender<-Gender[,-2]  

  
if (interactive()) {

  # UI layout -------------------------------------------------
  ui <- fluidPage(
    # We MUST load the ECharts javascript library in advance
    loadEChartsLibrary(),
    checkboxGroupInput("variable", "Tipos empleo en % Pob:",
                       c("Empleo no remunerado" = "No.Remunerado",
                         "Desempleado" = "Buscando.Empleo",
                         "Estudiando",
                         "Pensionado",
                         "Otro")),
    tableOutput("data_Emp"),
    tags$div(id="test", style="width:50%;height:400px;"),
    deliverChart(div_id = "test")
  )
  
  # Server function -------------------------------------------
  server <- function(input, output) {
    renderPieChart(div_id = "test",
                   data = Gender)
  }
  

  
  # Run the application --------------------------------------
  shinyApp(ui = ui, server = server)
}