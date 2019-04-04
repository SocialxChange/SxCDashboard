dashboardPage(
# Definimos el encabezado del sitio
    dashboardHeader(title = "Reporte SxC"),
# Definimos la barra de navegaci??n del lado izquierdo
dashboardSidebar(
  sidebarMenu(id = 'sidebarmenu',
    menuItem("Home", tabName = "home", icon = icon("dashboard")),
    menuItem('FIS', tabName = 'fis', icon = icon('list'),
            menuSubItem('Proyectos',tabName = 'proyectos', icon = icon('line-chart'))),
    menuItem('HUB', tabName = 'hub', icon = icon('th'),
            menuSubItem('Organizaciones', tabName = 'organizaciones', icon = icon('line-chart')))
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
              h2("Fundación Colunga"),
              # Definimos la primera fila de objetos donde pondremos el grafico de burbujas,
              # una mini pagina "fluidPage" que contiene los botones con total de donaciones y total de donante, y
              # el grafico de tendencias de google
              fluidRow(
                # A static infoBox
                infoBox("Cantidad de Organizaciones", "50 mil millones", icon = icon("fas fa-sitemap")),
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
                    checkboxGroupInput("year","" ,levels(as.factor(colunga$`AÑO ASIGNACION`)), selected = levels(as.factor(colunga$`AÑO ASIGNACION`)))
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

# Second tab content: reporte
tabItem(tabName = "gestion",
        fluidPage(
          titlePanel("Posicionamiento Indicadores Gestión"),
          fluidRow(
            box(title = "Perfil comparado",
                width="auto", solidHeader = TRUE,
                    plot_ly(
                      type = 'scatterpolar',
                      fill = 'toself'
                    ) %>%
                      add_trace(
                        r = c(datos1,datos1[1]),
                        theta = variables,
                        name = 'Protectora de Todos'
                      ) %>%
                      add_trace(
                        r = c(datos2,datos2[1]),
                        theta = variables,
                        name = 'Promedio organizaciones'
                      ) %>%
                      layout(
                        polar = list(
                          radialaxis = list(
                            visible = T,
                            range = c(0,100)
                          )
                        )
                      )),
                    # Caja con grafico donaciones XY
                    box(
                      title = "Monto Donaciones XY 2011-2016",
                      width="auto", solidHeader = TRUE,
                      plot_ly(Donaciones_MallPlaza, x = ~Year, y = ~MontoTotal, name = 'Donación', type = 'scatter', mode = 'lines')
                    )
          ),
          
          navlistPanel(
            "",
            tabPanel("Este es el informe tipo de SocialxChange para nuestro clientes, en este se incluye un breve análisis de los programas y temáticas en las que nuestros clientes estan donando, del desempeño social de los programas realizados de manera directa o a través de la gestión de terceras parte y recomendaciones para aumentar su impacto social.

                     XY, la gestión social se basa en una serie de componentes los que incluyen involucramiento local (Espacio Comunidad) e Inversión social. En el contexto de involucramiento con la comunidad, los centros comerciales buscan ser un lugar de encuentro y de visualizacion de proyectos y emprendimientos sociales. Por otra parte, en el caso de inversión social XY busca apoyar a las comunidades vulnerables cercanas a sus centros.  
                     
                     La siguiente tabla muestra las donaciones realizadas por XY desde 2009 a 2016, donde se aprecian los montos de las donaciones por año a cada institución, y los porcentajes de montos respecto a cada año. En términos de monto, la Fundación Acción Social Goreti (actual Junto Al Barrio, JAB) es la principal receptora de donaciones. En el caso de la Fundación Educacional Cerro Navia sólo se perciben montos en el año 2011. 
                     "),
            "Header B",
            tabPanel("Component 3"),
            tabPanel("Component 4"),
            "-----"
            
          )
       )
),

# Third tab content: stats
tabItem(tabName = "seguimiento",
        fluidPage(
          titlePanel("Cumplimiento y Validación Hitos"),
          
          navlistPanel(
            "",
            tabPanel("En los siguientes gráficos se ve las donaciones realizadas por XY por año y por filial en el marco de la ley de donaciones sociales. Se observa que desde 2009 a la fecha, existio un peak en terminos de monto de donaciones el año 2016. En terminos del promedio de montos, estos se situan en el rango de 0 a 30 millones."),
            "Header B",
            tabPanel("Component 3"),
            tabPanel("Component 4"),
            "-----",
            DTOutput('tbl')
            )
            )
),

# Fourth tab content: definimos el contenido de la página definida en la barra lateral llamada comparador
  tabItem(tabName = "comparador",
# Imprimimos tabla preparada en server.R con comando renderDataTable          
          fluidRow(dataTableOutput('comparador'))
        )
))
)