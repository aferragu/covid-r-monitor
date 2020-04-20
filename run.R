## app.R ##
library(shiny)
library(shinydashboard)
library(xts)
library(EpiEstim)

options(shiny.host = '0.0.0.0')
options(shiny.port = as.numeric(Sys.getenv('PORT')))

ui <- dashboardPage(
  dashboardHeader(title = "COVID R Monitor"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      infoBox("Uruguay")
    ),
    fluidRow(
      box(title = "Casos registrados", plotOutput("plot_incidence", height = 250)),
      box(title = "Estimacion de tasa R",plotOutput("plot_estimR", height = 250))
    ),
    fluidRow(
      uiOutput("choose_country")
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
  serie <- xts(datos_uy[,c("new_cases")],order.by=times)

  output$plot_incidence <- renderPlot({
    plot(serie)
  })

  #estimo el R
  mean_covid_si<-3.95
  sd_covid_si <- 4.75
  delta_si<-30
  discrete_si_distr <- discr_si(seq(0, delta_si), mean_covid_si, sd_covid_si)

  res <- estimate_R(incid = datos_uy[,"new_cases"],
                  method = "non_parametric_si",
                  config = make_config(list(si_distr = discrete_si_distr)))

  times2=tail(times,-7)
  serieR <- xts(res$R[,c("Median(R)")],order.by=times2)
  serieRl <- xts(res$R[,c("Quantile.0.025(R)")],order.by=times2)
  serieRu <- xts(res$R[,c("Quantile.0.975(R)")],order.by=times2)

  output$plot_estimR <- renderPlot({
    plot(serieR)
#    lines(serieRl)
#    lines(serieRu)
#    abline(h=1,col="red",lwd=2)
#    abline(h=0,lwd=2)

  })


  output$choose_country <- renderUI({
      selectInput("pais", "Pais", unique(data[,"location"], selected="Uruguay"))
  })

  actualizar_pais <- reactive( {
        datos_country <- data[data$location == input$pais,]
        times_country <- as.Date(datos_country[,"date"])
        serie_x|country <- xts(datos_country[,c("new_cases")],order.by=times_country)

        res_country <- estimate_R(incid = datos_country[,"new_cases"],
                        method = "non_parametric_si",
                        config = make_config(list(si_distr = discrete_si_distr)))

        times2_country <- tail(times_country,-7)
        serieR_country <- xts(res_country$R[,c("Median(R)")],order.by=times2_country)
        serieRl_country <- xts(res_country$R[,c("Quantile.0.025(R)")],order.by=times2_country)
        serieRu_country <- xts(res_country$R[,c("Quantile.0.975(R)")],order.by=times2_country)

        serieR_country
      }
    )

  output$plot_incidence_country <- renderPlot({
    datos <- actualizar_pais()
    plot(datos)
  })

  output$plot_estimR_country <- renderPlot({
    datos <- actualizar_pais()
    plot(datos)
#    lines(serieRl)
#    lines(serieRu)
#    abline(h=1,col="red",lwd=2)
#    abline(h=0,lwd=2)

  })

  output$result <- renderText({
      paste("Pais: ", input$choose_country)
    })

}

shinyApp(ui, server)
