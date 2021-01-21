library(shiny)
library(shinydashboard)
library(plotly)

status1 <- "primary"
status2 <- "info"

shinyUI(

    dashboardPage(
      dashboardHeader(title = "COVID R Monitor",
                      tags$li(tags$img(src="logogach.png",height="50px"),class="dropdown")
      ),

      dashboardSidebar(collapsed=FALSE,

          sidebarMenu(
              menuItem("Uruguay", tabName = "uruguay", icon = icon("dashboard")),
              menuItem("Comparativas", tabName = "comparativas", icon = icon("dashboard")),
              menuItem("Parámetros de Estimación",
                      tags$div(title="Tamaño de la ventana (días) para estimar el R",
                        sliderInput("window_R", "Ventana estimacion R", 1, 14, 7, step = 1)
                      ),
                      tags$div(title="Tamaño de la ventana (días) para promediar los casos diarios",
                        sliderInput("window_ma", "Ventana suavizado", 2, 14, 7, step = 1)
                      ),
                      tags$div(title="Tamaño de la ventana (días) para suavizar ratio de test",
                        sliderInput("window_ratio", "Ventana acumulacion ratio", 1, 7, 3, step = 1)
                      ),
                      tags$div(title="Duración media del intervalo de contagio",
                        numericInput("mean_covid_si", "Media SI", 3.95, min = 0, max = 15, step = 0.2)
                      ),
                      tags$div(title="Desvío estándar del intervalo de contagio",
                        numericInput("sd_covid_si", "Desvio SI", 4.75, min = 0, max = 15, step = 0.25)
                      )
                    )
          )
      ),

      dashboardBody(

          tabItems(
              tabItem(tabName = "uruguay",

                    fluidRow(
                      infoBoxOutput("uruguay"),
                      infoBoxOutput("uruguay_ci_lower"),
                      infoBoxOutput("uruguay_ci_upper")
                    ),
                    fluidRow(
                      div(style = "margin: auto; width: 80%",
                      sliderInput("CommonDatesUY",
                          label="Rango temporal:",
                          min = as.Date("2020-03-13","%Y-%m-%d"),
                          max = Sys.Date(),
                          value=c(as.Date("2019-12-01","%Y-%m-%d"),Sys.Date()),
                          timeFormat="%Y-%m-%d",
                          width="100%",
                          step = 1)
                        )
                    ),
                    fluidRow(
                      box(title = "Casos registrados", plotlyOutput("plot_incidence", height = 300),status="primary",solidHeader = TRUE,
                collapsible = TRUE),
                      box(title = "Estimación de tasa R",plotlyOutput("plot_estimR", height = 300),status="primary",solidHeader = TRUE,
                collapsible = TRUE)
                    ),
                    fluidRow(
                      box(title = "Casos activos", plotlyOutput("plot_active", height = 300),status="primary",solidHeader = TRUE,
                collapsible = TRUE),
                      box(title = "Ratio de test positivos",plotlyOutput("plot_test_ratio", height = 300),status="primary",solidHeader = TRUE,
                collapsible = TRUE)
                    ),
                    fluidRow(
                      box(title = "Sobre la estimación:", HTML("<strong>Metodologia: </strong> Documento realizado por E. Mordecki explicando la metodologia : <a href='http://www.cmat.edu.uy/~mordecki/EpiEstim_reporte.pdf'>Reporte</a>.<br />"),HTML("<strong>Origen de los datos:</strong> Los datos de incidencia de Uruguay y otros países son extraídos de <a href='https://ourworldindata.org/coronavirus-source-data'>ourworldindata</a>. Esto puede presentar discrepanacias con datos oficiales de cada pais. Los datos de casos activos de Uruguay y ratio de test positivos son tomados del <a href='https://guiad-covid.github.io/data/'>repositorio de datos del GUIAD</a>, que publica como datos abiertos los datos del SINAE-UY."),status="primary",solidHeader = TRUE,
                collapsible = TRUE),
                      box(title = "Descargar los resultados", downloadLink("downloadData", "Resultados de estimacion del R"),status="primary",solidHeader = TRUE,
                collapsible = TRUE)
                    ),
                ),
                tabItem(tabName="comparativas",
                    fluidRow(
                      box(title= "Elegir Pais 1:", uiOutput("choose_country_1"),status=status1,solidHeader = TRUE,
                      collapsible = TRUE),
                      box(title= "Elegir Pais 2:", uiOutput("choose_country_2"),status=status2,solidHeader = TRUE,
                      collapsible = TRUE)
                    ),
                    fluidRow(
                      div(style = "margin: auto; width: 80%",
                      sliderInput("CommonDates",
                          label="Rango temporal:",
                          min = as.Date("2019-12-01","%Y-%m-%d"),
                          max = Sys.Date(),
                          value=c(as.Date("2019-12-01","%Y-%m-%d"),Sys.Date()),
                          timeFormat="%Y-%m-%d",
                          width="100%",
                          step = 1)
                        )
                    ),
                    fluidRow(
                      box(title = "Casos registrados", plotlyOutput("plot_incidence_country_1", height = 300),status=status1,solidHeader = TRUE,
                      collapsible = TRUE),
                      box(title = "Casos registrados", plotlyOutput("plot_incidence_country_2", height = 300),status=status2,solidHeader = TRUE,
                      collapsible = TRUE)
                    ),
                    fluidRow(
                      box(title = "Estimacion de tasa R",plotlyOutput("plot_estimR_country_1", height = 300),status=status1,solidHeader = TRUE,
                      collapsible = TRUE),
                      box(title = "Estimacion de tasa R",plotlyOutput("plot_estimR_country_2", height = 300),status=status2,solidHeader = TRUE,
                      collapsible = TRUE)
                    ),
                    fluidRow(
                      box(title = "Oxford Stringency Index",plotlyOutput("plot_stringency_country_1", height = 300),status=status1,solidHeader = TRUE,
                      collapsible = TRUE),
                      box(title = "Oxford Stringency Index",plotlyOutput("plot_stringency_country_2", height = 300),status=status2,solidHeader = TRUE,
                      collapsible = TRUE)
                    ),

                )
            ),

        tags$footer(HTML("Desarrollado por Andres Ferragut (Univ. ORT Uruguay) y Ernesto Mordecki (CMAT, UdelaR) en base a la biblioteca EpiEstim. Contacto: <a href='mailto:ferragut@ort.edu.uy'>ferragut@ort.edu.uy</a>. <a href='https://github.com/aferragu/covid-r-monitor'>Codigo</a>"))
      )
    )
)
