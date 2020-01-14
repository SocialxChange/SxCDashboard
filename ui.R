dashboardPage(
# Definimos el encabezado del sitio
    dashboardHeader(title = "Dashboard BHP"),
# Definimos la barra de navegaci??n del lado izquierdo
dashboardSidebar(
  sidebarMenu(id = 'sidebarmenu',
    menuItem("Home", tabName = "home", icon = icon("dashboard")),
    menuItem('Becas', tabName = 'becas', icon = icon('list'),
             menuSubItem('Home Becas',tabName = 'homebecas', icon = icon('line-chart')),
             menuSubItem('Beneficiarios',tabName = 'beneficiarios', icon = icon('line-chart')),
             menuSubItem('Carreras',tabName = 'carreras', icon = icon('line-chart'))
             )
    #,
    #menuItem('HUB', tabName = 'hub', icon = icon('th'),
     #       menuSubItem('Organizaciones', tabName = 'organizaciones', icon = icon('line-chart')))
            )),


# Definimos lo que se mostrar?? en el cuerpo central del sitio
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      
# First tab content: relacionado al boton de home de la barra de navegaci??n
      #includeScript("tabs/home.R"),
      tabItem(tabName = "home",
              h2("Región Antofagasta"),
              # Definimos la primera fila de objetos donde pondremos el grafico de burbujas,
              # una mini pagina "fluidPage" que contiene los botones con total de donaciones y total de donante, y
              # el grafico de tendencias de google
              fluidRow(
                # A static infoBox
                infoBox("Cantidad de Organizaciones", "45", icon = icon("fas fa-sitemap")),
                # Dynamic infoBoxes
                infoBoxOutput("progressBox"),
                infoBoxOutput("approvalBox")
              ),
              
              # infoBoxes with fill=TRUE
              fluidRow(
                infoBox("Cantidad de proyectos", 50, icon = icon("fas fa-project-diagram"), fill = TRUE),
                infoBoxOutput("progressBox2"),
                infoBoxOutput("approvalBox2")
              ),
              
              #fluidRow(
                # Clicking this will increment the progress amount
              #  box(width = 4, actionButton("count", "Increment progress"))
              #),
              
# Fluid Row Desempeño Colunga
              
              fluidRow(
                # Primer elemento con caja conteniendo grafico de burbujas      
                box(
                #  solidHeader = TRUE, 
                  title="Efectividad",
                  # Imprimimos grafico preparado en server.R con comando renderBubbles        
                  bubblesOutput("burbujasC", height="500px", width="400px")
                ),
                box(width = 2, solidHeader = TRUE,
                    title = "Ubicación",
                    checkboxGroupInput("location","" ,levels(as.factor(colunga$REGION)), selected = levels(as.factor(colunga$REGION)))
                ),
                box(width = 2, solidHeader = TRUE,
                    title = "Año",
                    checkboxGroupInput("year","" ,levels(as.factor(colunga$AnioAsignacion)), selected = levels(as.factor(colunga$AnioAsignacion)))
                )
              ),
# Fluid Row POBREZA
              fluidRow(
                # Primer elemento con caja conteniendo grafico de burbujas      
                box(
                  solidHeader = TRUE, 
                  title="Pobreza Multidimensional por Comuna",
                  # Imprimimos grafico preparado en server.R con comando renderBubbles        
                  bubblesOutput("burbujas", height="500px", width="400px")
                ),
                box(width = 2, solidHeader = TRUE,
                    title = "Comunas",
                    checkboxGroupInput("comunas","" ,levels(MetricasComuna$comuna), selected = levels(MetricasComuna$comuna))
                ),
                # Caja con grafico tendencias de Google       
                fluidRow(box(
                  title = "Tendencias de Google",
                  height = "500px",
                  solidHeader = TRUE,
                  plot_ly(gtrends, x = ~fecha, y = ~educacion, name = 'Educación', type = 'scatter', mode = 'lines') %>%
                    add_trace(y = ~salud, name = 'Salud', mode = 'lines+markers') %>%
                    add_trace(y = ~infancia, name = 'Infancia', mode = 'lines+markers') %>%
                    layout(
                      xaxis=list(title="Tiempo"),
                      yaxis=list(title="Importancia")
                    )
                ))
              )            

              # Segunda fila con selector de categor??as y tabla de proyectos
              #fluidRow(
                # Caja con selector de categorias que define variable input$categorias ocupada en server.R      
                #box(width = 2, status = "info", solidHeader = TRUE,
                #    title = "Categorías",
                #    checkboxGroupInput("categorias","" ,levels(Proyectos2016$categoria),selected=levels(Proyectos2016$categoria))
                #),
                # Caja con selector de comunas que define variable input$comunas ocupada en server.R      
                
                # Tabla con proyectos
                #box(title = "Proyectos",
                #    width = 8,
                #    status = "info",
                #    solidHeader = TRUE,
                #    # Imprimimos tabla preparada en server.R con comando renderDataTable          
                #    dataTableOutput('tabla'))
              #)
      ),

# Second tab content: FIS
tabItem(tabName = "homebecas",
        fluidPage(
          fluidRow(infoBox("Estudiantes", 168, icon = icon("fas fa-project-diagram"), fill = TRUE),
                   box(title = "Distribución Beneficiarios", status = "primary", collapsible = TRUE, 
                       plotlyOutput("barchart", height = "400px"))),
          fluidRow(
            plot_ly(data.frame(datos5,datos6),x = ~datos6, y = ~datos5,
                    name = 'Monto Financiado',
                    type = 'scatter', mode = 'lines'
            )
          ),
          fluidRow(
            leafletOutput("mymap"),
          )
       
       )
),

# Third tab content: stats
tabItem(tabName = "beneficiarios",
        fluidPage(
          titlePanel("Personas beneficiadas por programa"),
          DTOutput('tbl')
          
            )
),

# Fourth tab content: datos carreras de beneficiarios
  tabItem(tabName = "carreras",
# Imprimimos tabla preparada en server.R con comando renderDataTable          
fluidPage(
  titlePanel("Carreras de Personas beneficiadas por programa"),
  DTOutput('tblcarreras')
  
)
        )
))
)
