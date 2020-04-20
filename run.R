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
          box(
            width = 8, status = "info", solidHeader = TRUE,
            title = "Popularity by package (last 5 min)",
            
          ),
        )

  )
)

server <- function(input, output) { }

shinyApp(ui, server)
