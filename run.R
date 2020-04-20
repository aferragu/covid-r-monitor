## app.R ##
library(shiny)
library(shinydashboard)

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
  set.seed(122)
  histdata <- rnorm(500)

  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)
