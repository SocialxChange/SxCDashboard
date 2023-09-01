#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom dplyr select filter group_by summarise
#' @importFrom rlang sym
#' @importFrom leaflet renderLeaflet leaflet addTiles setView
#' @importFrom DT renderDT
#' @importFrom colorspace sequential_hcl
#' @importFrom bubbles renderBubbles bubbles
#' @noRd
app_server <- function(input, output, session) {
  # Definimos valores de los botones con total donantes y total donaciones ----
  output$cuantas <- renderValueBox({
    valueBox(
      value = sum(sxcdashboard::colunga$aporte_convenio, na.rm = TRUE) / 1000000,
      subtitle = "Inversión Social Total",
      icon = icon("fas fa-igloo")
    )
  })

  # Valor del total de donaciones
  output$cuanto <- renderValueBox({
    valueBox(
      value = round(sum(sxcdashboard::proyectos_2016$total), digits = 0),
      subtitle = "Total donaciones (Millones)",
      icon = icon("hand-holding-usd", lib = "font-awesome")
    )
  })

  # Definimos el grafico de burbujas Colunga ----

  output$burbujasC <- renderBubbles({
    dfefec <- sxcdashboard::colunga %>%
      select(!!sym("region"), !!sym("anio_asignacion"), !!sym("efectividad02017")) %>%
      filter(!!sym("region") %in% input$location)

    dfefec_ag <- dfefec %>%
      group_by(!!sym("region")) %>%
      summarise(efectividad02017 = sum(!!sym("efectividad02017"), na.rm = TRUE))

    bubbles(dfefec_ag$efectividad02017, dfefec_ag$region,
      color = sequential_hcl(nrow(dfefec_ag), h = col2rgb("#efc000")),
      textColor = "#FFFFFF"
    )
  })

  # Definimos el grafico de burbujas Pobreza ----

  output$burbujas <- renderBubbles({
    df <- sxcdashboard::metricas_comuna %>%
      select(!!sym("comuna"), !!sym("pobreza_ing"), !!sym("pobreza_mult")) %>%
      filter(!!sym("comuna") %in% input$comunas)

    bubbles(df$pobreza_mult, df$comuna,
      color = sequential_hcl(nrow(df), h = col2rgb("#efc000")),
      textColor = "#FFFFFF"
    )
  })

  # Definimos la tabla del home ----
  output$tabla <- renderDataTable(
    sxcdashboard::proyectos_2016 %>%
      select(
        !!sym("proyecto"), !!sym("p_total"), !!sym("pobreza_ing"), !!sym("pobreza_mult"), !!sym("gtrend"),
        !!sym("socialrank")
      ) %>%
      filter(!!sym("comuna") %in% input$comunas & !!sym("categoria") %in% input$categorias)
  )

  # Definimos la tabla del comparador que es llamada igual que el tab donde está ----

  output$comparador <- renderDataTable(sxcdashboard::proyectos_2016)

  # Tabla 1 Reporte ----
  # output$tbl <- renderDT(
  #   ranking_mallplaza,
  #   options = list(lengthChange = FALSE), rownames = FALSE,
  #   caption = "Tabla 1: Donaciones por Institución Beneficiaria, MM$"
  # )

  output$progressBox <- renderInfoBox({
    dfyear <- sxcdashboard::colunga %>%
      select(!!sym("organizacion"), !!sym("anio_asignacion"), !!sym("aporte_convenio")) %>%
      filter(!!sym("anio_asignacion") %in% input$year)

    infoBox(
      "Inversión Social Total",
      paste(
        round(sum(dfyear$aporte_convenio, na.rm = TRUE) / 1000000, digits = 0),
        "Millones",
        sep = " "
      ),
      icon = icon("fas fa-money-bill-wave-alt"),
      color = "purple"
    )
  })
  output$approvalBox <- renderInfoBox({
    infoBox(
      "Beneficiarios", "832",
      icon = icon("fas fa-users"),
      color = "yellow"
    )
  })

  # Same as above, but with fill=TRUE

  output$progressBox2 <- renderInfoBox({
    infoBox(
      "Crecimiento", paste(4, "%", sep = " "),
      icon = icon("fas fa-chart-line"),
      color = "purple", fill = TRUE
    )
  })

  output$approvalBox2 <- renderInfoBox({
    infoBox(
      "Cumplimiento hitos", "87%",
      icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow", fill = TRUE
    )
  })

  # MAPA seccion FIS proyectos ----

  output$mymap <- renderLeaflet({
    m <- leaflet() %>%
      addTiles() %>%
      setView(lng = -71.5429688, lat = -35.675148, zoom = 5)
    m
  })
}
