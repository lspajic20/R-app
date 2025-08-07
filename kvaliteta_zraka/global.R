library(shiny)
library(shinydashboard)
library(httr)
library(jsonlite)
library(DT)
library(shinyjs)
library(readxl)
library(plotly)
library(zoo)
library(janitor)
library(tidyr)
library(dplyr)

api_key <- "44606ae1c46c6a7a05d05c0be8f154d79d1ee219"
city_country <- read_excel("gradovi_drzave.xlsx")


viz_data <- read_excel("podaciv2.xlsx") %>%
  janitor::clean_names()


viz_data$grad <- as.character(viz_data$grad) 

aqi_color <- function(aqi) {
  if (is.na(aqi)) return("#999999")
  if (aqi <= 50) return("#aedcae")
  if (aqi <= 100) return("#ffea61")
  if (aqi <= 150) return("#ff9100")
  if (aqi <= 200) return("#ff4500")
  if (aqi <= 300) return("#9f5ea5")
  return("#7e0023")
}

# Dohvaćanje podataka putem API-ja
get_city_data <- function(city, token) {
  url <- paste0("https://api.waqi.info/feed/", URLencode(city), "/?token=", token)
  res <- GET(url)
  if (status_code(res) != 200) return(NULL)
  data <- fromJSON(content(res, "text"), flatten = TRUE)
  if (data$status != "ok") return(NULL)
  return(data$data)
}

# Pomocna funkcija za city dropdown
render_city_dropdown <- function(country_input, input_id) {
  renderUI({
    req(country_input())
    cities <- city_country %>%
      filter(Country == country_input()) %>%
      pull(City) %>%
      unique()
    
    selectInput(inputId = input_id, label = "Odaberi grad", choices = cities)
  })
}

# Pomoćna funkcija za year dropdown
render_year_dropdown <- function(input_id) {
  renderUI({
    req(viz_data)
    years <- sort(unique(lubridate::year(viz_data$datum)))
    selectInput(inputId = input_id, label = "Odaberi godinu", choices = years, selected = max(years))
  })
}

