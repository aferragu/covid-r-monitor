library(shiny)
library(shinydashboard)
library(plotly)

shinyUI(

    dashboardPage(
      dashboardHeader(title = "COVID R Monitor"),
      dashboardSidebar(disable=TRUE),
      dashboardBody(
        fluidRow(
          infoBoxOutput("uruguay"),
          infoBoxOutput("uruguay_ci_lower"),
          infoBoxOutput("uruguay_ci_upper")
        ),
        fluidRow(
          box(title = "Casos registrados", plotlyOutput("plot_incidence", height = 300),status="primary",solidHeader = TRUE,
    collapsible = TRUE),
          box(title = "Estimacion de tasa R",plotlyOutput("plot_estimR", height = 300),status="primary",solidHeader = TRUE,
    collapsible = TRUE)
        ),
        fluidRow(
          box(title = "Sobre la estimacion:", HTML("<strong>Metodologia: </strong> Documento realizado por E. Mordecki explicando la metodologia : <a href='http://www.cmat.edu.uy/~mordecki/EpiEstim_reporte.pdf'>Reporte</a>.<br />"),HTML("<strong>Origen de los datos:</strong> Los datos de Uruguay y otros países son extraídos de <a href='https://ourworldindata.org/coronavirus-source-data'>ourworldindata</a>. Esto puede presentar discrepanacias con datos oficiales de cada pais."),status="primary",solidHeader = TRUE,
    collapsible = TRUE),
          box(title = "Descargar los resultados", downloadLink("downloadData", "Resultados de estimacion"),status="primary",solidHeader = TRUE,
    collapsible = TRUE)
        ),
        fluidRow(
          box(title= "Elegir Pais:", uiOutput("choose_country"),status="primary",solidHeader = TRUE,
    collapsible = TRUE)
        ),
        fluidRow(
          box(title = "Casos registrados", plotlyOutput("plot_incidence_country", height = 300),status="primary",solidHeader = TRUE,
    collapsible = TRUE),
          box(title = "Estimacion de tasa R",plotlyOutput("plot_estimR_country", height = 300),status="primary",solidHeader = TRUE,
    collapsible = TRUE)
        ),
        tags$footer(HTML("Desarrollado por Andres Ferragut (Univ. ORT Uruguay) y Ernesto Mordecki (CMAT, UdelaR) en base a la biblioteca EpiEstim. Contacto: <a href='mailto:ferragut@ort.edu.uy'>ferragut@ort.edu.uy</a>. <a href='https://github.com/aferragu/covid-r-monitor'>Codigo</a>"))
      )
    )
)
