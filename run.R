## app.R ##
library(shiny)
library(shinydashboard)

options(shiny.host = '0.0.0.0')
options(shiny.port = as.numeric(Sys.getenv('PORT')))

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)

server <- function(input, output) { }

shinyApp(ui, server)

