dashboardPage(
# Definimos el encabezado del sitio
    dashboardHeader(title = "SocialXChange Beta"),
# Definimos la barra de navegaci??n del lado izquierdo
  dashboardSidebar(
    sidebarMenu(
# Home      
      menuItem("Home", tabName = "home", icon = icon("dashboard")),
# Tabla general  llamada comparador      
      menuItem("Comparador", tabName = "comparador", icon = icon("th"))
    )
  ),
# Definimos lo que se mostrar?? en el cuerpo central del sitio
  dashboardBody(
    tabItems(
      
# First tab content: relacionado al boton de home de la barra de navegaci??n
      tabItem(tabName = "home",
# Definimos la primera fila de objetos donde pondremos el grafico de burbujas,
# una mini pagina "fluidPage" que contiene los botones con total de donaciones y total de donante, y
# el grafico de tendencias de google
    fluidRow(
# Primer elemento con caja conteniendo grafico de burbujas      
      box(
        width = 6, status = "info", solidHeader = TRUE,
        title="Pobreza Multidimensional por Comuna",
# Imprimimos grafico preparado en server.R con comando renderBubbles        
        bubblesOutput("burbujas", width = "100%", height = 600)
      ),
# Definici??n de mini pagina       
      fluidPage(
# Columna con los dos botones        
        column(6,valueBoxOutput("cuantas",width=3),valueBoxOutput("cuanto",width=3)),
# Caja con grafico tendencias de Google       
        box(
        title = "Tendencias de Google",
        widht="auto", status= "info", solidHeader = TRUE,
        plot_ly(gtrends, x = ~fecha, y = ~educacion, name = 'Educaci??n', type = 'scatter', mode = 'lines') %>%
          add_trace(y = ~salud, name = 'Salud', mode = 'lines+markers') %>%
          add_trace(y = ~infancia, name = 'Infancia', mode = 'lines+markers')%>%
          add_trace(y = ~emprendimiento, name = 'Emprendimiento', mode = 'lines+markers')%>%
          add_trace(y = ~urbanismo, name = 'Urbanismo', mode = 'lines+markers')
      ))
    ),
# Segunda fila con selector de categor??as y tabla de proyectos
    fluidRow(
# Caja con selector de categorias que define variable input$categorias ocupada en server.R      
      box(width = 2, status = "info", solidHeader = TRUE,
                 title = "Categorías",
                 checkboxGroupInput("categorias","" ,levels(Proyectos2016$categoria),selected=levels(Proyectos2016$categoria))
                 ),
# Caja con selector de comunas que define variable input$comunas ocupada en server.R      
      box(width = 2, status = "info", solidHeader = TRUE,
          title = "Comunas",
         checkboxGroupInput("comunas","" ,levels(MetricasComuna$comuna), selected = levels(MetricasComuna$comuna))
         ),
# Tabla con proyectos
      box(title = "Proyectos",
          width = 8,
          status = "info",
          solidHeader = TRUE,
# Imprimimos tabla preparada en server.R con comando renderDataTable          
          dataTableOutput('tabla'))
    )
  ),
# Second tab content: definimos el contenido de la página definida en la barra lateral llamada comparador
  tabItem(tabName = "comparador",
# Imprimimos tabla preparada en server.R con comando renderDataTable          
          fluidRow(dataTableOutput('comparador'))
  )))
)