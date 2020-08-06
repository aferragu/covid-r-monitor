library(shiny)
library(shinydashboard)
library(xts)
library(EpiEstim)
library(plotly)

shinyServer(function(input, output, session) {

      download.file("https://covid.ourworldindata.org/data/ecdc/full_data.csv", "full_data.csv")
      data <- read.csv(file = 'full_data.csv')
      datos_uy <- data[data$location == 'Uruguay',]
      times <- as.Date(datos_uy[,"date"])
      incidencia <- datos_uy[,"new_cases"]

      datos_incidencia_uy <- data.frame(Tiempo=times,Incidencia=incidencia)

      output$plot_incidence <- renderPlotly({
        plot_ly(
            data = datos_incidencia_uy,
            x = ~Tiempo,
            y = ~Incidencia,
            name = "Nuevos casos diarios",
            type = "bar"
          )
      })

      #estimo el R
      mean_covid_si<-3.95
      sd_covid_si <- 4.75
      delta_si<-30
      discrete_si_distr <- discr_si(seq(0, delta_si), mean_covid_si, sd_covid_si)

      res <- estimate_R(incid = pmax(incidencia,0),
                      method = "non_parametric_si",
                      config = make_config(list(si_distr = discrete_si_distr)))

      times2=tail(times,-7)
      datos_R_uy <- data.frame(Tiempo=times2,R=res$R[,c("Median(R)")],Rl=res$R[,"Quantile.0.025(R)"],Ru=res$R[,"Quantile.0.975(R)"])


      output$plot_estimR <- renderPlotly({

        fig <- plot_ly(
                data=datos_R_uy,
                x = ~Tiempo,
                y = ~R,
                type = 'scatter',
                mode = 'lines',
                line = list(color='rgb(31, 119, 180)'),
                name = 'R estimado')
        fig <- fig %>% add_trace(
                    x = ~Tiempo,
                    y = ~Ru,
                    type = 'scatter',
                    mode = 'lines',
                    line = list(color = 'transparent'),
                    showlegend = FALSE,
                    hoverinfo='skip',
                    name = 'Limite superior')
        fig <- fig %>% add_trace(
            y = ~Rl,
            type = 'scatter',
            mode = 'lines',
            fill = 'tonexty',
            fillcolor='rgba(31, 119, 180,0.2)',
            line = list(color = 'transparent'),
            showlegend = FALSE,
            hoverinfo='skip',
            name = 'Limite inferior')



        fig <- fig %>% add_trace(
                y = rep(1,length(times2)),
                type = 'scatter',
                mode = 'lines',
                showlegend = FALSE,
                line = list(color='rgb(127,0,0)',width=1,dash="dash"),
                hoverinfo='skip',
                name = 'Referencia')

        fig <- fig %>% layout(
            title = "R estimado",
            yaxis = list(title="R",range=c(0,3.5),hoverformat = '.2f')
        )
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
          times <- as.Date(datos_country[,"date"])
          incidencia <- datos_country[,c("new_cases")]
          datos_incidencia_country <- data.frame(Tiempo=times,Incidencia=incidencia)
          datos_incidencia_country
          }
        )

      output$plot_incidence_country <- renderPlotly({
          plot_ly(
              data = actualizar_serie_pais(),
              x = ~Tiempo,
              y = ~Incidencia,
              name = "Nuevos casos diarios",
              type = "bar"
            )

      })

      actualizar_calculo_pais <- reactive( {
            datos_country <- data[data$location == input$pais,]
            times <- as.Date(datos_country[,"date"])

            #cambio los NA por 0
            datos_country[is.na(datos_country[,"new_cases"]),"new_cases"] <- 0

            incidencia <- datos_country[,c("new_cases")]

            res <- estimate_R(incid = pmax(incidencia,0),
                            method = "non_parametric_si",
                            config = make_config(list(si_distr = discrete_si_distr)))

            times2 <- tail(times,-7)
            datos_R_country <- data.frame(Tiempo=times2,R=res$R[,c("Median(R)")],Rl=res$R[,"Quantile.0.025(R)"],Ru=res$R[,"Quantile.0.975(R)"])
          }
        )

      output$plot_estimR_country <- renderPlotly({

          data = actualizar_calculo_pais()
          n=nrow(data)

          fig <- plot_ly(
                  data=data,
                  x = ~Tiempo,
                  y = ~R,
                  type = 'scatter',
                  mode = 'lines',
                  line = list(color='rgb(31, 119, 180)'),
                  name = 'R estimado')
          fig <- fig %>% add_trace(
                      x = ~Tiempo,
                      y = ~Ru,
                      type = 'scatter',
                      mode = 'lines',
                      line = list(color = 'transparent'),
                      showlegend = FALSE,
                      hoverinfo='skip',
                      name = 'Limite superior')
          fig <- fig %>% add_trace(
              y = ~Rl,
              type = 'scatter',
              mode = 'lines',
              fill = 'tonexty',
              fillcolor='rgba(31, 119, 180,0.2)',
              line = list(color = 'transparent'),
              showlegend = FALSE,
              hoverinfo='skip',
              name = 'Limite inferior')



          fig <- fig %>% add_trace(
                  y = rep(1,n),
                  type = 'scatter',
                  mode = 'lines',
                  showlegend = FALSE,
                  line = list(color='rgb(127,0,0)',width=1,dash="dash"),
                  hoverinfo='skip',
                  name = 'Referencia')

          fig <- fig %>% layout(
              title = "R estimado",
              yaxis = list(title="R",range=c(0,3.5),hoverformat = '.2f')
          )
      })

      output$result <- renderText({
          paste("Pais: ", input$choose_country)
        })

})
