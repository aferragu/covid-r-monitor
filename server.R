shinyServer(function(input, output, session) {

    #####Auxiliary functions

    #Filter Time and Incidence data for a given country. Also provides a moving average of window W
    filter_data <- function(data,country, W=7) {
        datos_country <- data[data$location == country,]
        times <- as.Date(datos_country[,"date"])
        incidencia <- datos_country[,c("new_cases")]
        incidencia_ma <- stats::filter(incidencia, rep(1/W,W),sides=1)
        datos_incidencia_country <- data.frame(Tiempo=times,Incidencia=incidencia,MediaMovil=incidencia_ma)
        return(datos_incidencia_country)
    }

    #Filter stringency data
    filter_stringency_data <- function(data,country) {
        datos_country <- data[data$CountryName == country & data$Jurisdiction=="NAT_TOTAL",]
        times <- as.Date(as.character(datos_country[,"Date"]),"%Y%m%d")
        stringency <- datos_country[,"StringencyIndex"]
        datos_stringency_country <- data.frame(Tiempo=times,StringencyIndex=stringency)
        return(datos_stringency_country)
    }

    #Estimate R from incidence and parameters
    estimate_R_country <- function(datos,window=7,mean_covid_si=3.95,sd_covid_si=4.75,delta_si=30) {

        discrete_si_distr <- discr_si(seq(0, delta_si), mean_covid_si, sd_covid_si)
        datos[is.na(datos[,"Incidencia"]),"Incidencia"] <- 0

        #vectores auxiliares para cambiar la ventana
        tt<-nrow(datos)
        t_start <- seq(2, tt-window+1)
        t_end <- t_start + window-1

        res <- estimate_R(incid = pmax(datos$Incidencia,0),
                        method = "non_parametric_si",
                        config = make_config(list(si_distr = discrete_si_distr,t_start=t_start,t_end=t_end)))

        shortened_times <- tail(datos$Tiempo,-window)
        datos_R_country <- data.frame(Tiempo=shortened_times,R=res$R[,c("Median(R)")],Rl=res$R[,"Quantile.0.025(R)"],Ru=res$R[,"Quantile.0.975(R)"])
        return(datos_R_country)
    }

    #Process GUIAD data to get active cases and positive test ratio
    process_data_guiad <- function(datos, W=7, W2=3) {

        fecha <- datos[,"fecha"]
        incidencia <- datos[,"cantCasosNuevos"]
        incidencia_ma <- stats::filter(as.numeric(incidencia), rep(1,W)/W, sides=1)
        activos <- datos[,"cantPersonasConInfeccionEnCurso"]
        ratio_test <- as.numeric(datos[,"cantCasosNuevos"])/as.numeric(datos[,"cantTest"])*100
        ratio_test_suavizado_num <- stats::filter(as.numeric(datos[,"cantCasosNuevos"]), rep(1,W2), sides=1)
        ratio_test_suavizado_den <- stats::filter(as.numeric(datos[,"cantTest"]), rep(1,W2), sides=1)
        ratio_test_suavizado <- ratio_test_suavizado_num/ratio_test_suavizado_den*100
        return(data.frame(Tiempo = fecha, Incidencia = incidencia, MediaMovil = incidencia_ma, Activos = activos, RatioTest = ratio_test, RatioTestSuavizado = ratio_test_suavizado))

    }

    #Create Plotly Barplot for incidence
    plotly_incidence <- function(datos) {
        fig<- plot_ly(
            data = datos,
            x = ~Tiempo,
            y = ~Incidencia,
            name = "Nuevos casos diarios",
            type = "bar"
          )
          fig <- fig %>% add_trace(
              x = ~Tiempo,
              y = ~MediaMovil,
              type = 'scatter',
              mode = 'lines',
              line = list(color = 'rgba(119, 31, 180,0.5)'),
              showlegend = FALSE,
              name = 'Media móvil')
          return(fig)
    }

    #Create Plotly Barplot for StringencyIndex
    plotly_stringency <- function(datos) {
        fig <- plot_ly(
            data = datos,
            x = ~Tiempo,
            y = ~StringencyIndex,
            name = "Oxford Stringency Index",
            type = "scatter",
            mode = "lines"
          )
          fig <- fig %>% layout(yaxis = list(range = c(0, 100)))
          return(fig)
    }

    #Create Plotly Barplot for R
    plotly_R <- function(datos_R_country){
        n=nrow(datos_R_country)

        fig <- plot_ly(
                data=datos_R_country,
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
        return(fig)
    }

    #Create Plotly for active cases
    plotly_active <- function(datos) {
        fig<- plot_ly(
            data = datos,
            x = ~Tiempo,
            y = ~Activos,
            name = "Cant. de casos activos",
            type = "scatter",
            mode = "lines"
        )
        fig <- fig %>% layout(title = "Casos activos")
          return(fig)
    }

    #Create Plotly for test ratio
    plotly_test_ratio <- function(datos) {
        fig<- plot_ly(
            data = datos,
            x = ~Tiempo,
            y = ~RatioTest,
            name = "Ratio de test positivos",
            type = "scatter",
            mode = "lines"
          )
          fig <- fig %>% add_trace(
              x = ~Tiempo,
              y = ~RatioTestSuavizado,
              type = 'scatter',
              mode = 'lines',
              line = list(color = 'rgba(119, 31, 180,0.5)'),
              showlegend = FALSE,
              name = 'Media móvil')
          fig <- fig %>% layout(title = "Porcentaje de test positivos")
          return(fig)
    }


    ######Main logic and rendering functions

    #Download data
    data <- get_data()
    stringency_data <- get_stringency_data()
    guiad<- get_data_guiad()

    data_guiad <- reactive(process_data_guiad(guiad,W=input$window_ma,W2=input$window_ratio))
    
    #Filter Uruguay
    datos_incidencia_uy <- reactive(data_guiad())
    datos_R_uy <- reactive(estimate_R_country(datos_incidencia_uy(), window=input$window_R,mean_covid_si=input$mean_covid_si,sd_covid_si=input$sd_covid_si))

    output$plot_incidence <- renderPlotly({
      plotly_incidence(datos_incidencia_uy()) %>% layout(
          title = "Incidencia",
          xaxis = list(range = input$CommonDatesUY)
      )
    })

    output$plot_estimR <- renderPlotly({
        plotly_R(datos_R_uy()) %>% layout(
            xaxis = list(range = input$CommonDatesUY)
        )
    })

    output$plot_active <- renderPlotly({
      plotly_active(data_guiad()) %>% layout(
          xaxis = list(range = input$CommonDatesUY)
      )
    })

    output$plot_test_ratio <- renderPlotly({
        plotly_test_ratio(data_guiad()) %>% layout(
            xaxis = list(range = input$CommonDatesUY)
        )
    })

    output$downloadData <- downloadHandler(
        filename = function() {
          paste("estimacion_R_Uruguay-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          write.csv(datos_R_uy(), file)
        }
    )

    output$uruguay <- renderInfoBox({
        infoBox(
          "R actual Uruguay",
          tail(datos_R_uy()$R, n=1)
        )
    })

    output$uruguay_ci_lower <- renderInfoBox({
        infoBox(
          "Cuantil 0.025",
          tail(datos_R_uy()$Rl, n=1)
        )
    })

    output$uruguay_ci_upper <- renderInfoBox({
        infoBox(
          "Cuantil 0.975",
          tail(datos_R_uy()$Ru, n=1)
        )
    })

    ##Get country data

    output$choose_country_1 <- renderUI({
        selectInput("pais1", NULL, unique(data[,"location"]), selected="Uruguay")
    })

    output$choose_country_2 <- renderUI({
        selectInput("pais2", NULL, unique(data[,"location"]))
    })

    output$plot_incidence_country_1 <- renderPlotly({
        plotly_incidence(filter_data(data,input$pais1,input$window_ma))  %>% layout(
            xaxis = list(range = input$CommonDates)
        )
    })

    output$plot_estimR_country_1 <- renderPlotly({
        plotly_R(estimate_R_country(filter_data(data,input$pais1),window=input$window_R,mean_covid_si=input$mean_covid_si,sd_covid_si=input$sd_covid_si)) %>% layout(
            xaxis = list(range = input$CommonDates)
        )
    })

    output$plot_incidence_country_2 <- renderPlotly({
        plotly_incidence(filter_data(data,input$pais2,input$window_ma)) %>% layout(
            xaxis = list(range = input$CommonDates)
        )
    })

    output$plot_estimR_country_2 <- renderPlotly({
        plotly_R(estimate_R_country(filter_data(data,input$pais2),window=input$window_R,mean_covid_si=input$mean_covid_si,sd_covid_si=input$sd_covid_si)) %>% layout(
            xaxis = list(range = input$CommonDates)
        )
    })

    output$plot_stringency_country_1 <- renderPlotly({
        plotly_stringency(filter_stringency_data(stringency_data,input$pais1))  %>% layout(
            xaxis = list(range = input$CommonDates)
        )
    })

    output$plot_stringency_country_2 <- renderPlotly({
        plotly_stringency(filter_stringency_data(stringency_data,input$pais2))  %>% layout(
            xaxis = list(range = input$CommonDates)
        )
    })

})
