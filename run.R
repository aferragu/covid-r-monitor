## app.R ##
library(shiny)
library(shinydashboard)
library(xts)
library(plotly)
library(EpiEstim)

options(shiny.host = '0.0.0.0')
options(shiny.port = as.numeric(Sys.getenv('PORT')))

ui <- dashboardPage(
  dashboardHeader(title = "COVID R Monitor"),
  dashboardSidebar(collapsed=TRUE),
  dashboardBody(
    fluidRow(
      infoBoxOutput("uruguay"),
      infoBoxOutput("uruguay_ci_lower"),
      infoBoxOutput("uruguay_ci_upper")
    ),
    fluidRow(
      box(title = "Casos registrados", plotOutput("plot_incidence", height = 250)),
      box(title = "Estimacion de tasa R",plotOutput("plot_estimR", height = 250))
    ),
    fluidRow(
      box(title = "Sobre la estimación:", HTML("<strong>Metodología: </strong> Documento realizado por E. Mordecki explicando la metodología : <a href='EpiEstim_desarrollo_v1.pdf'>Reporte</a>.<br />"),HTML("<strong>Origen de los datos:</strong> Los datos de Uruguay y otros países son extraídos de <a href='https://covid.ourworldindata.org/'>ourworldindata</a>. Esto puede presentar discrepanacias con datos oficiales de cada país."),status="primary",solidHeader = TRUE,
collapsible = TRUE),
      box(title = "Descargar los resultados", downloadLink("downloadData", "Resultados de Estimacion"),status="primary",solidHeader = TRUE,
collapsible = TRUE),
    ),
    fluidRow(
      box(title= "Elegir Pais:", uiOutput("choose_country"))
    ),
    fluidRow(
      box(title = "Casos registrados", plotOutput("plot_incidence_country", height = 250)),
      box(title = "Estimacion de tasa R",plotOutput("plot_estimR_country", height = 250))
    )
  )
)



server <- function(input, output,session) {

  download.file("https://covid.ourworldindata.org/data/ecdc/full_data.csv", "full_data.csv")
  data <- read.csv(file = 'full_data.csv')
  datos_uy <- data[data$location == 'Uruguay',]
  times <- as.Date(datos_uy[,"date"])
  serie <- xts(datos_uy[,"new_cases"],order.by=times)

  output$plot_incidence <- renderPlot({
    barplot(serie,main="Incidencia", ylab="Casos")
  })

  #estimo el R
  mean_covid_si<-3.95
  sd_covid_si <- 4.75
  delta_si<-30
  discrete_si_distr <- discr_si(seq(0, delta_si), mean_covid_si, sd_covid_si)

  res <- estimate_R(incid = pmax(datos_uy[,"new_cases"],0),
                  method = "non_parametric_si",
                  config = make_config(list(si_distr = discrete_si_distr)))

  times2=tail(times,-7)
  serieR <- xts(res$R[,c("Median(R)")],order.by=times2)
  serieRl <- xts(res$R[,c("Quantile.0.025(R)")],order.by=times2)
  serieRu <- xts(res$R[,c("Quantile.0.975(R)")],order.by=times2)

  output$plot_estimR <- renderPlot({
    plot(serieR,ylim=c(0,3),main="R estimado", xlab="R", ylab="Tiempo" )
    lines(serieRl)
    lines(serieRu)
    ref <- xts(rep(1.0,length(serieR)),order.by=time(serieR))
    lines(ref,col="red")
  })

    output$downloadData <- downloadHandler(
        filename = function() {
          paste("estimacion_R_Uruguay-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          write.csv(res$R, file)
        }
    )

  output$uruguay <- renderInfoBox({
      infoBox(
        "R actual Uruguay",
        tail(res$R[,c("Median(R)")], n=1)
      )
  })

  output$uruguay_ci_lower <- renderInfoBox({
      infoBox(
        "Cuantil 0.025",
        tail(res$R[,c("Quantile.0.025(R)")], n=1)
      )
  })

  output$uruguay_ci_upper <- renderInfoBox({
      infoBox(
        "Cuantil 0.975",
        tail(res$R[,c("Quantile.0.975(R)")], n=1)
      )
  })

  output$choose_country <- renderUI({
      selectInput("pais", "Pais", unique(data[,"location"]))
  })

  actualizar_serie_pais <- reactive( {
      datos_country <- data[data$location == input$pais,]
      times_country <- as.Date(datos_country[,"date"])
      serie_country <- xts(datos_country[,c("new_cases")],order.by=times_country)
      serie_country
      }
    )

  output$plot_incidence_country <- renderPlot({
      barplot(actualizar_serie_pais(), main="Incidencia", ylab="Casos")
  })

  actualizar_calculo_pais <- reactive( {
        datos_country <- data[data$location == input$pais,]
        times_country <- as.Date(datos_country[,"date"])
        serie_country <- xts(datos_country[,c("new_cases")],order.by=times_country)

        res_country <- estimate_R(incid = pmax(datos_country[,"new_cases"],0),
                        method = "non_parametric_si",
                        config = make_config(list(si_distr = discrete_si_distr)))

        times2_country <- tail(times_country,-7)
        serieR_country <- xts(res_country$R[,c("Median(R)")],order.by=times2_country)
        serieRl_country <- xts(res_country$R[,c("Quantile.0.025(R)")],order.by=times2_country)
        serieRu_country <- xts(res_country$R[,c("Quantile.0.975(R)")],order.by=times2_country)

        list(R=serieR_country,Rl=serieRl_country,Ru=serieRu_country)
      }
    )

  output$plot_estimR_country <- renderPlot({
    data <- actualizar_calculo_pais()
    plot(data$R,ylim=c(0,3),,main="R estimado", xlab="R", ylab="Tiempo")
    lines(data$Rl)
    lines(data$Ru)
    ref <- xts(rep(1.0,length(data$R)),order.by=time(data$R))
    lines(ref,col="red")
  })

  output$result <- renderText({
      paste("Pais: ", input$choose_country)
    })

}

shinyApp(ui, server)
