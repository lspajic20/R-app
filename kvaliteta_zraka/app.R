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

# --- UI ---
ui <- dashboardPage(
  dashboardHeader(
    title = tags$a(
      href = "#",
      tags$img(src = "logoo.png", height = "40px",
               style = "margin-top: -10px; cursor:pointer;", id = "logo_click")
    ),
    titleWidth = 70
  ),
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      menuItem("Informacije", tabName = "informacije", icon=icon("table")),
      menuItem("Vizualizacije", tabName="vizualizacije", icon = icon("chart-bar")),
      menuItem("Prikaz na karti", tabName = "karta", icon = icon("map")),
      menuItem("Predviđanja", tabName = "predvidjanja", icon = icon("chart-line")),
      menuItem("O aplikaciji", tabName = "oaplikaciji", icon=icon("info-circle"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
        document.getElementById('logo_click').addEventListener('click', function() {
          Shiny.setInputValue('logo_click', new Date().getTime());
        });
      });
    ")),
    tags$head(
      tags$style("
        .sidebar-toggle { display: none }
        .skin-blue .main-header .logo { background-color: #B4C4D9; color: white; font-weight: 700;}
        .skin-blue .main-header .logo:hover { background-color: #B4C4D9; }
        .skin-blue .main-sidebar { background-color: #B4C4D9; }
        .skin-blue .main-sidebar .sidebar a { color: #2D4473; font-weight: 600;}
        .skin-blue .main-sidebar .sidebar a:hover { background-color: #7E94BF;}
        .skin-blue .main-sidebar .sidebar-menu > li.active > a { background-color: #7E94BF; color: white; }
        .skin-blue .main-header .navbar { background-color: #B4C4D9; }
        .aqi-box { padding: 15px; border-radius: 8px; color: white; }
        .aqi-value { font-size: 28px; font-weight: bold; }
      ")
    ),
    tabItems(
      # INFORMACIJE
      tabItem(tabName = "informacije",
              fluidRow(
                box(width = 12, solidHeader = TRUE, status = "primary",
                    fluidRow(
                      column(6, selectInput("country", "Odaberi državu", choices = unique(city_country$Country), selected="Croatia")),
                      column(6, uiOutput("city_ui"))
                    )
                )
              ),
              fluidRow(
                column(
                  width = 8,
                  box(title = "Trenutna kvaliteta zraka",
                      width = 12, solidHeader = TRUE,
                      uiOutput("city_info"),
                      style = "text-align:center; font-size:18px; padding:20px;"
                  )
                ),
                column(
                  width = 4,
                  box(title = "AQI Legenda",
                      width = 12, solidHeader = TRUE,
                      HTML("
                        <div style='padding:5px;'>
                          <div style='background-color:#aedcae; padding:5px;'>0–50: Dobro</div>
                          <div style='background-color:#ffea61; padding:5px;'>51–100: Umjereno</div>
                          <div style='background-color:#ff9100; padding:5px;'>101–150: Nezdravo za osjetljive</div>
                          <div style='background-color:#ff4500; padding:5px; color:white;'>151–200: Nezdravo</div>
                          <div style='background-color:#9f5ea5; padding:5px; color:white;'>201–300: Vrlo nezdravo</div>
                          <div style='background-color:#7e0023; padding:5px; color:white;'>300+: Opasno</div>
                        </div>
                      ")
                  )
                )
              ),
              fluidRow(
                column(
                  width = 6,
                  box(
                    title = "Pregled parametara",
                    width = 12, solidHeader = TRUE, 
                    DTOutput("pollutants_table")
                  )
                ),
                column(
                  width = 6,
                  box(
                    title = "Objašnjenje parametara koji utječu na kvalitetu zraka",
                    width = 12,
                    status = "warning",
                    div(
                      style = "padding:10px; font-size:14px; line-height:1.6;",
                      HTML("
                        <b>CO</b>: Ugljikov monoksid – bezbojan, otrovan plin koji nastaje izgaranjem.<br>
                        <b>NO₂</b>: Dušikov dioksid – iritant za dišni sustav, potječe od prometa i industrije.<br>
                        <b>O₃</b>: Ozon – štetan pri razini tla, povezan s respiratornim problemima.<br>
                        <b>PM10 / PM2.5</b>: Čestice prašine – sitne čestice koje mogu prodrijeti duboko u pluća.<br>
                        <b>SO₂</b>: Sumporni dioksid – potječe od sagorijevanja fosilnih goriva.<br>
                        <b>Dew</b>: Točka rosišta – pokazuje vlagu u zraku.<br>
                        <b>p</b>: Atmosferski tlak – mjera pritiska zraka.<br>
                        <b>t</b>: Temperatura zraka.<br>
                        <b>w</b>: Brzina vjetra.
                      ")
                    )
                  )
                )
              )
        
      ),
      # VIZUALIZACIJE
      tabItem(tabName = "vizualizacije",
              fluidRow(
                box(width = 12, solidHeader = TRUE, status = "primary",
                    fluidRow(
                      column(6, selectInput("viz_country", "Odaberi državu", choices = unique(city_country$Country), selected = "Croatia")),
                      column(6, uiOutput("viz_city_ui"))
                    ),
                    br(),
                    fluidRow(
                      column(12, selectInput("viz_param", "Odaberi parametar", 
                                             choices = names(viz_data)[!names(viz_data) %in% c("datum", "grad")]))
                    )
                )
              ),
              fluidRow(
                box(title = "Trend parametra kroz vrijeme", width = 12, solidHeader = TRUE,
                    plotlyOutput("viz_plot"))
              )
      ),
      
      #PRIKAZ NA KARTI
      tabItem(tabName = "karta",
              h2("Ovdje će biti prikaz na karti...")
      ),
      
      #PREDVIĐANJA
      tabItem(tabName = "predvidjanja",
              h2("Ovdje će biti predviđanja...")
      ),
      
      # O APLIKACIJI
      tabItem(tabName = "oaplikaciji",
              h2("Ovdje će biti informacije o aplikaciji...")
    )
  )
)
)

# --- SERVER ---
server <- function(input, output, session) {
  # Prikaz/skrivanje bočne trake s izbornikom
  observeEvent(input$logo_click, {
    toggleClass(selector="body", class = "sidebar-collapse")
  })
  
  #Dropdown za izbor grada
  output$city_ui <- renderUI({
    req(input$country)
    cities <- city_country$City[city_country$Country == input$country]
    selectInput("city", "Odaberi grad", choices = cities,
                selected = if (input$country == "Croatia") "Zagreb" else cities[1])
  })
  
  city_data <- reactiveVal(NULL)
  observeEvent(input$city, {
    req(input$city)
    data <- get_city_data(input$city, api_key)
    if (is.null(data)) {
      showModal(modalDialog(title = "Greška", "Nije moguće dohvatiti podatke za odabrani grad."))
    } else {
      city_data(data)
    }
  }, ignoreInit = TRUE)
  
  # Odlomak za prikaz info o AQI
  output$city_info <- renderUI({
    data <- city_data()
    req(data)
    city_name <- as.character(ifelse(!is.null(data$city$name), data$city$name, "Nepoznato"))
    aqi_value <- as.numeric(ifelse(!is.null(data$aqi), data$aqi, NA))
    dom_pol <- as.character(ifelse(!is.null(data$dominentpol), data$dominentpol, "N/A"))
    update_time <- as.character(ifelse(!is.null(data$time$s), data$time$s, "Nepoznato"))
    color <- aqi_color(aqi_value)
    
    HTML(paste0(
      "<div class='aqi-box' style='background-color:", color, ";'>",
      "<h4>", city_name, "</h4>",
      "<div class='aqi-value'>AQI: ", aqi_value, "</div>",
      "<p><strong>Dominantni polutant:</strong> ", dom_pol, "</p>",
      "<p><strong>Zadnje ažuriranje:</strong> ", update_time, "</p>",
      "</div>"
    ))
  })
  
  # Tablica za prikaz onečišćivača
  pollutant_order <- c("pm25", "pm10", "no2", "o3", "so2", "co", "dew", "p", "t", "w", "wg", "h")
  output$pollutants_table <- renderDT({
    data <- city_data()
    req(data)
    if (is.null(data$iaqi)) return(data.frame())
    
    df <- data.frame(
      Onečišćivač = names(data$iaqi),
      Vrijednost = sapply(data$iaqi, function(x) ifelse(!is.null(x$v), x$v, NA)),
      stringsAsFactors = FALSE
    )
    
    df$Onečišćivač <- tolower(df$Onečišćivač)
    df <- df[match(pollutant_order, df$Onečišćivač, nomatch = 0), ]
    
    datatable(df, rownames = FALSE, options = list(dom = 't', paging = FALSE))
  })
  
  # Dropdown za državu
  output$viz_city_ui <- renderUI({
    req(input$viz_country)
    cities <- city_country$City[city_country$Country == input$viz_country]
    selectInput("viz_city", "Odaberi grad", choices = cities, selected = cities[1])
  })
  
  # Filtriranje dataseta
  filtered_viz_data <- reactive({
    req(input$viz_country, input$viz_city, input$viz_param)
    
    df <- viz_data %>%
      filter(grad == input$viz_city) %>%
      select(datum, value = all_of(input$viz_param)) %>%
      mutate(
        value = as.numeric(value)
      ) %>%
      drop_na()
  })
  
  #Generiranje grafa
  output$viz_plot <- renderPlotly({
    req(filtered_viz_data(), input$viz_param)
    df <- filtered_viz_data()
    
    plot_ly(
      data = df,
      x = ~datum,
      y = ~value,
      type = "scatter",
      mode = "lines+markers"
    ) %>%
      layout(
        title = paste("Trend parametra:", input$viz_param),
        xaxis = list(
          title = "Vrijeme",
          type = "date",
          tickformat = "%b %Y",
          tickangle = -45
        ),
        yaxis = list(title = input$viz_param)
      )
  })
  
  
  
}


shinyApp(ui, server)
