## app.R ##
library(shiny)
library(shinydashboard)
library(xts)

options(shiny.host = '0.0.0.0')
options(shiny.port = as.numeric(Sys.getenv('PORT')))

ui <- dashboardPage(
  dashboardHeader(title = "COVID R Monitor"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
     box(plotOutput("plot1", height = 250)),

     box(
       title = "Controls",
       sliderInput("slider", "Number of observations:", 1, 100, 50)
     )
   )
 )
)



server <- function(input, output) {

  download.file("https://covid.ourworldindata.org/data/ecdc/full_data.csv", "full_data.csv")
  data <- read.csv(file = 'full_data.csv')
  datos_uy <- data[data$location == 'Uruguay',]
  times <- as.Date(datos_uy[,"date"])
  serie <- xts(datos_uy[,c("new_cases")],order.by=times)

  output$plot1 <- renderPlot({
    plot(serie)
  })

}

shinyApp(ui, server)
