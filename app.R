###HEX COLORS SXC
#2797C9 azul celeste
#5BCBFD celeste
#F1CB31 amarillo claro
#EFC000 amarillo dorado
#1DC9A1 verde agua
#4E4E4E gris casi negro
#656565 gris oscuro
#707070 gris intermedio
#C4C4C4 gris claro 1
#D0D0D0 gris claro 2
#DDDDDD gris claro 3
#E4E7ED gris casi blanco
#F8F8F8 gris blanco
library(DBI)
library(broom)
library(dplyr)
library(fuzzyjoin)
library(generics)
library(gtools)
library(geosphere)
library(lubridate)
library(modelr)
library(reprex)
library(rvest)
library(selectr)
library(tidyverse)

source("datos/global_tablas.R")
source("datos/ETL_Carreras.R")

header<-  dashboardHeader(title="BHP",disable=TRUE)
sidebar<-  dashboardSidebar(
  collapsed=TRUE
)
body<-  dashboardBody(
h3("Indicadores a Nivel Regional"),
          fluidRow(
        box(title="Población/Género", status="primary",solidHeader = TRUE, collapsible=TRUE, collapsed = TRUE, width=6,
            selectInput("comuna_pobhm","Comuna", c("Antofagasta","Mejillones","Sierra Gorda","Taltal","Calama","Ollagüe","SP Atacama","Tocopilla","Maria Elena")),
            plotlyOutput("pob_hm")
        ) ,
        box(title="Población según edad/educación", status="primary", solidHeader=TRUE, collapsible = TRUE, collapsed=TRUE, width=6,
            selectInput("comuna_tipoeduc","Comuna", c("Antofagasta","Mejillones","Sierra Gorda","Taltal","Calama","Ollagüe","SP Atacama","Tocopilla","Maria Elena")),
            plotlyOutput("pob_tipoeduc"))
          ),
          fluidRow(
      box(title = "Pobreza Multidimensional", status = "warning", solidHeader=TRUE,collapsible = TRUE,collapsed=TRUE,width=6,
          selectInput("comuna",
                      "Comunas",
                      c("Elija Comuna",
                        unique(as.character(Pob_Multi$comuna))),
                      multiple = TRUE, selectize=FALSE, selected=levels(as.factor(Pob_Multi$comuna))[1:3]),
          plotlyOutput('plot')),
          
          box(title = "Pobreza por Ingreso", status = "warning", solidHeader=TRUE,collapsible = TRUE,collapsed=TRUE,width=6,
              selectInput("comuna_ing",
                          "Comunas",
                          c("Elija Comuna",
                            unique(as.character(Pov_Ing$comuna))),
                          multiple = TRUE, selectize=FALSE, selected=levels(as.factor(Pov_Ing$comuna))[1:3]),
              plotlyOutput("Pov_Ing_radar"))
      ),
    
    fluidRow(
        box(title = "Desempleo Histórico Regional", status = "primary", solidHeader=TRUE,collapsible = TRUE,collapsed=TRUE,width=6,
             dataTableOutput("Desempleo")),
        box(title = "Empleo", status = "primary", solidHeader=TRUE,collapsible = TRUE,collapsed=TRUE,width=6,
            checkboxGroupInput("variable", "Tipos empleo en % Pob:",
                         c("Empleo no remunerado" = "No.Remunerado",
                           "Desempleado" = "Buscando.Empleo",
                           "Estudiando",
                           "Pensionado",
                           "Otro")),
            tableOutput("data_Emp")
    )
    ),
  
  
    fluidRow(
      box(title = "Situación Educacional por Comunas", status = "warning", solidHeader=TRUE,collapsible = TRUE,collapsed=TRUE,width=6,
            dataTableOutput("Educ")),
      box(title = "Matrículas por Año", status = "warning", solidHeader=TRUE,collapsible = TRUE,collapsed=TRUE,width=6,
             selectInput("año",
                         "Año",
                         c("2012","2013","2014","2015","2016","2017","2018","2019")),
            plotlyOutput("Matriculas"))
          #tableOutput("Mat"))
    ),
h3("Indicadores del Programa"),
    fluidRow(
      box(title = "Personas Beneficiadas por el programa", status = "info", solidHeader=TRUE,collapsible = TRUE,collapsed=TRUE,width=12,
          plotlyOutput("BECAS"))
    ),

    fluidRow(
  box(title = "Tasa de Incidencia del Programa", status = "info", solidHeader=TRUE,collapsible = TRUE,collapsed=TRUE,width=12,
  column(6,   
         h4(HTML("<b>Escuela Media</b>")),
         selectInput("año_media","Año", c("2012","2013","2014","2015","2016","2017","2018","2019")),
      plotlyOutput("incidencia_media")),
  column(6,
         h4(HTML("<b>Nivel Superior</b>")),
         selectInput("año_superior","Año", c("2012","2013","2014","2015","2016","2017","2018","2019")),
         plotlyOutput("incidencia_superior"))
  )),
fluidRow(
  box(title = "Ingreso Esperado Beneficiarios", status = "info", solidHeader=TRUE,collapsible = TRUE,collapsed=TRUE,width=12,
      plotlyOutput("Ingresos"))),

fluidRow(
  box(title = "Desempeño Esperado Beneficiarios 2015-2017", status = "info", solidHeader=TRUE,collapsible = TRUE,collapsed=TRUE,width=12,
      selectInput('indicador','Métrica', c("empleabilidad", "continuidad"), selected="empleabilidad", multiple=FALSE, selectize=FALSE),
      plotlyOutput("DESEMPEÑO")))

)

ui<-  dashboardPage(header, sidebar,body)

server <- function(input, output) {
    z <- reactive({
    Pob_Multi %>% filter(comuna %in% input$comuna) %>% select("2015")
  })
  
    k <- reactive({
    Pob_Multi %>% filter(comuna %in% input$comuna) %>% select("2017")
  })
  
  output$plot <- renderPlotly(
    fig <- plot_ly(
      type = 'scatterpolar',
      fill = 'toself',
      mode='lines'
    ) %>%
      add_trace(
        r = as.numeric(unlist(z())),
        theta = input$comuna, 
        fillcolor='#1DC9A1',
        line=list(color='rgba(29, 201, 161, 1)'),
        opacity=0.8,
        name = '2015')  %>%
      add_trace(
        r = as.numeric(unlist(k())),
        theta = input$comuna, 
        name = '2017',
        fillcolor='#F1CB31',
        opacity=0.8,
        line=list(color='rgba(241, 203, 49, 1)')
      )
  )
  
  
  output$Pob_Multi <- DT::renderDataTable(DT::datatable({
          data <- Pob_Multi
          if (input$comuna != "Elija Comuna") {
            data <- data %>% filter (comuna %in% input$comuna)
          }
          data 
          },
                rownames=FALSE,
                escape = FALSE,
                class = "nowrap stripe",
                extensions = list('FixedHeader','Scroller'),
                options = list(
                  scrollX = TRUE,
                  scroller = TRUE,
                 paging=FALSE,
                 searching = FALSE,
                  info=0,
                  lengthChange=FALSE,
                  initComplete = JS("
                  function(settings, json) {
                    $(this.api().table().header()).css({
                      'background-color': '#2797C9',
                      'color': '#fff',
                    });
                  }")
                )
                ))

  
  a <- reactive({
    Pov_Ing %>% filter(comuna %in% input$comuna_ing) %>% select("2015")
  })
  
  b <- reactive({
    Pov_Ing %>% filter(comuna %in% input$comuna_ing) %>% select("2017")
  })
  
  output$Pov_Ing_radar <- renderPlotly(
    fig <- plot_ly(
      type = 'scatterpolar',
      mode='lines'
    ) %>%
      add_trace(
        r = as.numeric(unlist(a())),
        theta = input$comuna_ing, 
        name = '2015',
        fill='toself',
        opacity=0.8,
        fillcolor='#5BCBFD',
        line=list(color='rgba(91, 203, 253, 1)'))  %>%
      add_trace(
        r = as.numeric(unlist(b())),
        theta = input$comuna_ing, 
        name = '2017',
        fill='toself',
        opacity=0.8,
        fillcolor='#EFC000',
        line=list(color='rgba(239, 192, 0, 1)'))
    
  )
    
    output$Pov_Ing <- renderTable({
    Pov_Ing[, c("comuna", input$variable), drop = FALSE]
  },
  rownames=FALSE,
  escape = FALSE,
  class = "nowrap stripe",
  extensions = list('FixedHeader','Scroller'),
  options = list(
    scrollX = TRUE,
    scroller = TRUE,
    DOM='t',
    #searching = FALSE,
    pageLength = 10,
    lengthMenu = list(c(10, 50, 100, 200), c('10', '50', '100', '200')),
    initComplete = JS("
    function(settings, json) {
      $(this.api().table().header()).css({
        'background-color': '#2797C9',
        'color': '#fff',
      });
    }")
  )
  )
  
  

        output$Desempleo <-renderDT(Desempleo, # reactive data
                                rownames=FALSE,
                                escape = FALSE,
                                class = "nowrap stripe",
                                extensions = list('FixedHeader','Scroller'),
                                options = list(
                                  scrollX = TRUE,
                                  scroller = TRUE,
                                  DOM='t',
                                  #searching = FALSE,
                                  pageLength = 10,
                                  lengthMenu = list(c(10, 50, 100, 200), c('10', '50', '100', '200')),
                                  initComplete = JS("
    function(settings, json) {
      $(this.api().table().header()).css({
        'background-color': '#5BCBFD',
        'color': '#fff',
      });
    }")
                                )
    )
      
       

    output$Mat <-renderDT(Mat_hist, # reactive data
                                rownames=FALSE,
                                escape = FALSE,
                                class = "nowrap stripe",
                                extensions = list('FixedHeader','Scroller'),
                                options = list(
                                  scrollX = TRUE,
                                  scroller = TRUE,
                                  DOM='t',
                                  #searching = FALSE,
                                  pageLength = 10,
                                  lengthMenu = list(c(10, 50, 100, 200), c('10', '50', '100', '200')),
                                  initComplete = JS("
    function(settings, json) {
      $(this.api().table().header()).css({
        'background-color': '#EFC000',
        'color': '#fff',
      });
    }")
                                )
    )

    output$data_Emp <- renderTable({
      RII_Empleo[, c("Comuna", "Remunerado", input$variable), drop = FALSE]
    }, 
    rownames=FALSE,
  #  escape = FALSE,
  #  class = "nowrap stripe",
  #  extensions = list('FixedHeader','Scroller'),
    options = list(
      scrollX = TRUE,
      scroller = TRUE,
      DOM='t',
      #searching = FALSE,
    #  pageLength = 10,
    #  lengthMenu = list(c(10, 50, 100, 200), c('10', '50', '100', '200')),
      initComplete = JS("
    function(settings, json) {
      $(this.api().table().header()).css({
        'background-color': '#5BCBFD',
        'color': '#fff',
      });
    }")
    )
    )

    output$Educ <-renderDT(RII_Educ,  
                           rownames=TRUE,
                           escape = FALSE,
                           class = "nowrap stripe",
                           extensions = list('FixedHeader','Scroller'),
                           options = list(
                             scrollX = TRUE,
                             scroller = TRUE,
                             DOM='t',
                             lengthChange=FALSE,
                             paging=FALSE,
                            searching = FALSE,
                            info=0,
                            initComplete = JS("
    function(settings, json) {
      $(this.api().table().header()).css({
        'background-color': '#EFC000',
        'color': '#fff',
      });
    }")
                           )
    )
    
    output$becas <-renderDT(becas, 
                            rownames=FALSE,
                            escape = FALSE,
                            class = "nowrap stripe",
                            extensions = list('FixedHeader','Scroller'),
                            options = list(
                #              columnDefs = list(list(visible=FALSE, target=1)),
                              scrollX = TRUE,
                              scroller = TRUE,
                              DOM='t',
                              lengthChange=FALSE,
                              searching = FALSE,
                              paging=FALSE,
                              info=0,
                              initComplete = JS("
                                  function(settings, json) {
                                    $(this.api().table().header()).css({
                                      'background-color': '#5BCBFD',
                                      'color': '#fff',
                                    });
                                  }")
                            ))
                            
    output$BECAS <-renderPlotly({
      plot_ly(becas, x = ~Año, name='Cantidad Becas') %>% 
        add_trace(y = ~Total, name = 'Total Becas', type = 'scatter', mode = 'lines', line=list(color='rgba(29, 201, 161, 1)')) %>%
        add_trace(y = ~Media, name = 'Escuela Media', type = 'scatter', mode = 'lines', line=list(color='rgba(239, 192, 0, 1)')) %>%
        add_trace(y = ~Superior, name = 'Nivel Superior', type = 'scatter', mode = 'lines', line = list(color = 'rgba(91, 203, 253, 1)')) 
      })
  
  f <- reactive({
      nomina %>% filter(comuna %in% input$comunasPer) %>% select(empleabilidad)
    })
    
  g <- reactive({
      nomina %>% filter(comuna %in% input$comunasPer) %>% select(continuidad)
    })
  
  output$DESEMPEÑO<-renderPlotly(
    fig <- plot_ly(x = ~as.numeric(unlist(aggcomunas %>% filter(comuna=="Baquedano") %>% select(Año))), 
                   y = ~as.numeric(unlist(aggcomunas %>% filter(comuna=="Baquedano") %>% select(input$indicador))), 
                   name = 'Baquedano', type = 'scatter', mode = 'lines', fill = 'tonexty',fillcolor = '#5BCBFD') %>% 
      add_trace(x = ~as.numeric(unlist(aggcomunas %>% filter(comuna=="Sierra Gorda") %>% select(Año))), 
                y = ~as.numeric(unlist(aggcomunas %>% filter(comuna=="Sierra Gorda") %>% select(input$indicador))), 
                name = 'Sierra Gorda', fillcolor = '#EFC000') %>% 
      layout(title = 'Desempeño esperado beneficiarios',
             xaxis = list(title = "Año",
                          showgrid = FALSE),
             yaxis = list(title = input$indicador,
                          showgrid = FALSE,
                          ticksuffix = ''))
    )
  
  output$Matriculas <- renderPlotly(
    fig<-plot_ly(Mat_hist, x = ~ Mat_hist %>% filter(año %in% input$año) %>% select("Comuna"), y = ~ as.numeric(unlist(Mat_hist %>% filter(año %in% input$año) %>% select("Basica"))), type = 'bar', name = 'Básica', marker = list(color = 'rgba(91, 203, 253)')) %>%
      add_trace(y = ~ as.numeric(unlist(Mat_hist %>% filter(año %in% input$año) %>% select("Media.HC"))), name = 'Media Humanista-Científico', marker = list(color = 'rgb(239, 192, 0)')) %>%
      add_trace(y = ~ as.numeric(unlist(Mat_hist %>% filter(año %in% input$año) %>% select("Media.TP"))), name = 'Media Tecnico-Profesional', marker = list(color = 'rgb(29, 201, 161)')) %>% 
      add_trace(y = ~ as.numeric(unlist(Mat_hist %>% filter(año %in% input$año) %>% select("Otros"))), name = 'Otros', marker = list(color = 'rgb(112, 112, 112)'))%>% 
      layout(yaxis = list(title = 'Count'),
             xaxis = list(title = ""),
             yaxis = list(title = ""),
             legend = list(orientation = 'h'),
             barmode = 'stack'))
  
  filtro2<-reactive({
    becados_tincidencia %>% filter(Año == input$año_media)%>% select("tasa_Media")
  })
  
  output$incidencia_media<-renderPlotly(
    figuras<-plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = filtro2()$tasa_Media[1]*100,
      gauge = list(
        axis =list(range = list(NULL, 100)),
        bar = list(color =  '#5BCBFD')),
      type = "indicator",
      mode = "gauge+number")%>%
      layout(margin = list(l=20,r=30))
  )
  filtro3<-reactive({
    becados_tincidencia %>% filter(Año == input$año_superior)%>% select("tasa_Superior")
  })
  
  output$incidencia_superior<-renderPlotly(
    figuras<-plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = filtro3()$tasa_Superior[1]*100,
      gauge = list(
        axis =list(range = list(NULL, 100)),
        bar = list(color =  '#EFC000')),
      type = "indicator",
      mode = "gauge+number")%>%
      layout(margin = list(l=20,r=30))
  )
  
  filtro<-reactive({
    pob_hm %>% filter(Nom_Com == input$comuna_pobhm)
  })
  
  long <-reactive({
    filtro() %>% gather(Nom_Com,Proporcion, Hombre:Mujeres)
  })
  
  colors <- c('rgb(29, 201, 161)', 'rgb(239, 192, 0)')
  
  output$pob_hm <- renderPlotly(
    figura<-plot_ly(
      long(), labels = ~Nom_Com, values = ~Proporcion, type = 'pie',
      textposition = 'inside',
      textinfo = 'label+percent',
      insidetextfont = list(color = '#FFFFFF'),
      marker = list(colors = colors,
                    line = list(color = '#FFFFFF', width = 1)),
      showlegend = FALSE)
  )

  filtro_hm<-reactive({
    pob_tipoeduc %>% filter(Comuna == input$comuna_tipoeduc)
  })
  
  long_hm <-reactive({
    filtro_hm() %>% gather(Comuna,Proporcion, Parvulo:Superior)
  })
  
  colors_educ <- c('rgb(29, 201, 161)', 'rgb(239, 192, 0)', 'rgb(91, 203, 253)', 'rgb(112, 112,112)')
  
  output$pob_tipoeduc <- renderPlotly(
    figura<-plot_ly(
      long_hm(), labels = ~Comuna, values = ~Proporcion, type = 'pie',
      textposition = 'inside',
      textinfo = 'label+percent',
      insidetextfont = list(color = '#FFFFFF'),
      marker = list(colors = colors_educ,
                    line = list(color = '#FFFFFF', width = 1)),
      showlegend = FALSE)
  )
  
  output$Ingresos<-renderPlotly(
    fig <- plot_ly(ingresosAño, x = ~Año, y = ~ingmax, type = 'scatter', mode = 'lines',
                   line = list(color = 'rgba(29, 201, 161,1)'),
                   showlegend = FALSE, name = 'Máximo') %>% 
      add_trace(y = ~ingmin, type = 'scatter', mode = 'lines',
                fill = 'tonexty', fillcolor='rgba(29, 201, 161,0.2)', line = list(color = 'rgba(29, 201, 161,1)'),
                showlegend = FALSE, name = 'Mínimo') %>% 
      layout(title = "Ingresos mínimos y máximos esperados de beneficiarios",
             paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
             xaxis = list(title = "Año",
                          gridcolor = 'rgb(255,255,255)',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'rgb(127,127,127)',
                          ticks = 'outside',
                          zeroline = FALSE),
             yaxis = list(title = "Ingresos (Miles de pesos)",
                          gridcolor = 'rgb(255,255,255)',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'rgb(127,127,127)',
                          ticks = 'outside',
                          zeroline = FALSE))
  )
  
}

shinyApp(ui, server)
  
  