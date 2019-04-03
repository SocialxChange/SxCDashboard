function(input, output) {
# Definimos valores de los botones con total donantes y total donaciones  
  output$cuantas <- renderValueBox({
    valueBox(
      value = sum(colunga$`MONTO APORTE (CONVENIO)`[!is.na(colunga$`MONTO APORTE (CONVENIO)`)])/1000000,
      subtitle = "Inversión Social Total",
      icon = icon("fas fa-igloo")
    )
  })
# Valor del total de donaciones  
  output$cuanto <- renderValueBox({
    valueBox(
      value = round(sum(Proyectos2016$total),digits=0),
      subtitle = "Total donaciones (Millones)",
      icon = icon("hand-holding-usd", lib = "font-awesome")
    )
  })
# Definimos el grafico de burbujas  
  output$burbujas <- renderBubbles({
    df<-filter(MetricasComuna[,c(1,3)], MetricasComuna$comuna %in% input$comunas)
    
    bubbles(df$pobrezaMult, df$comuna, 
            color=sequential_hcl(nrow(df), h = col2rgb("#efc000")), #rainbow(nrow(df), alpha = NULL)
            textColor = "#FFFFFF")
  })
# Definimos la tabla del home  
  output$tabla <- renderDataTable(filter(Proyectos2016[,c(1,4,7:10)], Proyectos2016$Comuna %in% input$comunas & Proyectos2016$categoria %in% input$categorias))
# Definimos la tabla del comparador que es llamada igual que el tab donde está
  output$comparador<-renderDataTable(Proyectos2016)
# Tabla 1 Reporte
  output$tbl = renderDT(
    RankingMallPlaza, options = list(lengthChange = FALSE), rownames=FALSE, caption = "Tabla 1: Donaciones por Institución Beneficiaria, MM$") 
  
  output$progressBox <- renderInfoBox({
    dfyear<-filter(colunga[,c(4,15,18)], colunga$`AÑO ASIGNACION` %in% input$year)
    infoBox(
      "Inversión Social Total", paste(round(sum(dfyear$`MONTO APORTE (CONVENIO)`[!is.na(dfyear$`MONTO APORTE (CONVENIO)`)])/1000000, digits=0), "Millones", sep=" "), icon = icon("fas fa-money-bill-wave-alt"),
      color = "purple"
    )
  })
  output$approvalBox <- renderInfoBox({
    infoBox(
      "Beneficiarios", "800 Millones", icon = icon("fas fa-users"),
      color = "yellow"
    )
  })
  
  # Same as above, but with fill=TRUE
  output$progressBox2 <- renderInfoBox({
    infoBox(
      "Crecimiento", paste(4, "%", sep=" "), icon = icon("fas fa-chart-line"),
      color = "purple", fill = TRUE
    )
  })
  output$approvalBox2 <- renderInfoBox({
    infoBox(
      "Cumplimiento hitos", "87%", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow", fill = TRUE
    )
  })
  
  }