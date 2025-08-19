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
library(forecast)
library(lubridate)
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

# Pomoćne za RF
make_rf_df <- function(hist_df, lags = 1:3) {
  df <- hist_df %>%
    dplyr::arrange(month) %>%
    dplyr::mutate(m = lubridate::month(month),
                  sin12 = sin(2*pi*m/12),
                  cos12 = cos(2*pi*m/12))
  for (k in lags) df[[paste0("lag", k)]] <- dplyr::lag(df$value, k)
  tidyr::drop_na(df)
}

fit_rf <- function(df) {
  ranger::ranger(value ~ lag1 + lag2 + lag3 + sin12 + cos12,
                 data = df, num.trees = 500, mtry = 3, min.node.size = 5, seed = 123)
}

forecast_rf <- function(fit, hist_df, h = 6) {
  last_date <- max(hist_df$month)
  vals <- hist_df$value
  if (length(vals) < 3) return(numeric(0))
  lag1 <- tail(vals, 1); lag2 <- tail(vals, 2)[1]; lag3 <- tail(vals, 3)[1]
  
  dates <- seq(last_date %m+% months(1), by = "month", length.out = h)
  preds <- numeric(h)
  m <- lubridate::month(last_date)
  
  for (i in seq_len(h)) {
    m <- (m %% 12) + 1
    newx <- data.frame(
      lag1 = lag1, lag2 = lag2, lag3 = lag3,
      sin12 = sin(2*pi*m/12), cos12 = cos(2*pi*m/12)
    )
    yhat <- predict(fit, data = newx)$predictions
    preds[i] <- pmax(yhat, 0)
    lag3 <- lag2; lag2 <- lag1; lag1 <- preds[i]
  }
  preds
}
