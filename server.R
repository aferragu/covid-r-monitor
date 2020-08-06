library(shiny)
library(shinydashboard)
library(EpiEstim)
library(plotly)

shinyServer(function(input, output, session) {

    #####Auxiliary functions

    #Get complete data_frame from server
    get_data <- function(location="https://covid.ourworldindata.org/data/ecdc/full_data.csv") {
        download.file(location, "full_data.csv")
        data <- read.csv(file = 'full_data.csv')
        return(data)
    }

    #Filter Time and Incidence data for a given country
    filter_data <- function(data,country) {
        datos_country <- data[data$location == country,]
        times <- as.Date(datos_country[,"date"])
        incidencia <- datos_country[,c("new_cases")]
        datos_incidencia_country <- data.frame(Tiempo=times,Incidencia=incidencia)
        return(datos_incidencia_country)
    }

    #Estimate R from incidence and parameters
    estimate_R_country <- function(datos,window=7,mean_covid_si=3.95,sd_covid_si=4.75,delta_si=30) {

        discrete_si_distr <- discr_si(seq(0, delta_si), mean_covid_si, sd_covid_si)
        datos[is.na(datos[,"Incidencia"]),"Incidencia"] <- 0
        res <- estimate_R(incid = pmax(datos$Incidencia,0),
                        method = "non_parametric_si",
                        config = make_config(list(si_distr = discrete_si_distr)))

        shortened_times <- tail(datos$Tiempo,-window)
        datos_R_country <- data.frame(Tiempo=shortened_times,R=res$R[,c("Median(R)")],Rl=res$R[,"Quantile.0.025(R)"],Ru=res$R[,"Quantile.0.975(R)"])
        return(datos_R_country)
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


    ######Main logic and rendering functions

    #Download data
    data <- get_data()

    #Filter Uruguay
    datos_incidencia_uy <- filter_data(data,"Uruguay")

    output$plot_incidence <- renderPlotly({
      plotly_incidence(datos_incidencia_uy)
    })

    datos_R_uy <- estimate_R_country(datos_incidencia_uy)

    output$plot_estimR <- renderPlotly({
        plotly_R(datos_R_uy)
    })

    output$downloadData <- downloadHandler(
        filename = function() {
          paste("estimacion_R_Uruguay-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          write.csv(datos_R_uy, file)
        }
    )

    output$uruguay <- renderInfoBox({
        infoBox(
          "R actual Uruguay",
          tail(datos_R_uy$R, n=1)
        )
    })

    output$uruguay_ci_lower <- renderInfoBox({
        infoBox(
          "Cuantil 0.025",
          tail(datos_R_uy$Rl, n=1)
        )
    })

    output$uruguay_ci_upper <- renderInfoBox({
        infoBox(
          "Cuantil 0.975",
          tail(datos_R_uy$R, n=1)
        )
    })

    output$choose_country <- renderUI({
        selectInput("pais", "Pais", unique(data[,"location"]))
    })

    output$plot_incidence_country <- renderPlotly({
        plotly_incidence(filter_data(data,input$pais))
    })

    output$plot_estimR_country <- renderPlotly({
        plotly_R(estimate_R_country(filter_data(data,input$pais)))
    })

})
