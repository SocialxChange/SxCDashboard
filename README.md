# SxCReportDemo

Este repositorio tiene los scripts de la primera version del dashboard de SocialxChange

La aplicación muestra datos de la base Prototipo.RData que fue generada con Prototipo.R (ver carpeta datos)

La aplicación se puede ver en [sebacea.shinyapps.io/SxCReportDemo/](https://sebacea.shinyapps.io/SxCReportDemo/)

## Estructura de la App

*   global.R carga las librerías necesarias y la base de datos
*   ui.R define la estructura de la página web y los inputs con los que se calculan las tablas y gráficos en server.R
*   server.R toma los inputs de ui.R y calcula las tablas y gráficos

## Uso de la aplicación en computador personal

basta abrir cualquiera de los archivos.R y apretar el boton Run App en RStudio. Puede requerirse loa instalación de los paquetes shinydashboard y dplyr
