library(shiny)
library(shinydashboard)
library(memoise)
library(plotly)
library(EpiEstim)

#port <- Sys.getenv('PORT')
if( Sys.getenv('PORT') != ""){
    port <- Sys.getenv('PORT')
} else {
    port = 8000
}

cache_timeout <- 1800 #half an hour

## Functions to get the data
#Get complete data_frame from server
get_data <- memoise(function(location="https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/full_data.csv") {
    data <- read.csv(location)
    return(data)
}, cache = cachem::cache_mem(max_age = cache_timeout))

#Get complete data_frame from GUIAD
get_data_guiad <- memoise(function(location="https://raw.githubusercontent.com/GUIAD-COVID/datos-y-visualizaciones-GUIAD/master/datos/estadisticasUY.csv") {
    data <- read.csv(location,na="N/A")
    data[,"fecha"] <- as.Date(data[,"fecha"],format="%d/%m/%Y")
    data <- data[order(data[,"fecha"]),]
    return(data)
}, cache = cachem::cache_mem(max_age = cache_timeout))

get_stringency_data <- memoise(function(location="https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_nat_latest.csv") {
    data <- read.csv(location)
    return(data)
}, cache = cachem::cache_mem(max_age = cache_timeout))

#retrieve data at startup

data <- get_data()
guiad <- get_data_guiad()
stringency_data <- get_stringency_data()

shiny::runApp(
  appDir = getwd(),
  host = '0.0.0.0',
  port = as.numeric(port)
)
