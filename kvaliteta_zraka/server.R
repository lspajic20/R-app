library(dplyr)
source("global.R")

# --- SERVER ---
server <- function(input, output, session) {
  # Prikaz/skrivanje bočne trake s izbornikom
  observeEvent(input$logo_click, {
    toggleClass(selector="body", class = "sidebar-collapse")
  })
  
  # City dropdown
  output$city_ui <- render_city_dropdown(reactive(input$country), "city")
  output$viz_city_ui <- render_city_dropdown(reactive(input$viz_country), "viz_city")
  output$avg_city_ui <- render_city_dropdown(reactive(input$avg_country), "avg_city")
  
  # Year dropdown
  output$time_filter_ui <- render_year_dropdown("selected_year")
  output$avg_year_ui <- render_year_dropdown("avg_year")
  
  #Učitavanje podataka o gradu nakon što je odabran
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
  
  #Filtriranje dataseta
  filtered_viz_data <- reactive({
    req(input$viz_country, input$viz_city, input$viz_param, input$selected_year)
    
    df <- viz_data %>%
      filter(grad == input$viz_city) %>%
      select(datum, value = all_of(input$viz_param)) %>%
      mutate(
        value = as.numeric(value),
        year = lubridate::year(datum),
        month = lubridate::floor_date(datum, "month")
      ) %>%
      filter(year == input$selected_year) %>%
      group_by(month) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      rename(datum = month)
  })
  
  param_inputs <- reactiveVal(c("param_1")) #Pohranjuje ID trenutno aktivnog dropdowna parametara
  
  # KLikom na 'dodaj' dodaju se novi dropdown izbornici za odabir max 5 parametara
  observeEvent(input$add_param, {
    current_ids <- param_inputs()
    
    if (length(current_ids) >= 5) return()
    
    next_index <- 1
    while (paste0("param_", next_index) %in% current_ids) {
      next_index <- next_index + 1
    }
    
    new_id <- paste0("param_", next_index)
    new_row_id <- paste0(new_id, "_row")
    
    insertUI(
      selector = "#param_ui_container",
      where = "beforeEnd",
      ui = tags$div(id = new_row_id,
                    fluidRow(
                      column(11,
                             selectInput(new_id, paste("Parametar", next_index), 
                                         choices = names(viz_data)[!names(viz_data) %in% c("datum", "grad")])
                      ),
                      column(1,
                             actionButton(paste0("remove_", new_id), "−", style = "margin-top:25px;")
                      )
                    )
      )
    )
    
    param_inputs(c(current_ids, new_id))
  })
  
  # Prati ako je kliknut gumb za uklanjanje i briše UI za taj dropdown
  observe({
    lapply(param_inputs(), function(id) {
      remove_id <- paste0("remove_", id)
      observeEvent(input[[remove_id]], {
        removeUI(selector = paste0("#", id, "_row"))
        param_inputs(setdiff(param_inputs(), id))
      }, ignoreInit = TRUE)
    })
  })
  
  # Reaktivni skup podataka koji kombinira odabrane parametre za prikaz
  filtered_multi_data <- reactive({
    req(input$viz_city, input$selected_year)
    
    selected_params <- unlist(lapply(param_inputs(), function(id) input[[id]]))
    selected_params <- selected_params[!is.na(selected_params) & selected_params != ""]
    
    #PRiprema podataka za prikaz
    df <- viz_data %>%
      filter(grad == input$viz_city) %>%
      mutate(
        year = lubridate::year(datum),
        month = lubridate::floor_date(datum, "month")
      ) %>%
      filter(year == input$selected_year) %>%
      select(month, all_of(selected_params)) %>%
      pivot_longer(-month, names_to = "parameter", values_to = "value") %>%
      drop_na() %>%
      group_by(month, parameter) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
    
    return(df)
  })
  
  #Višelinijski grafikon za usporedbu više odabranih parametara
  output$multi_param_plot <- renderPlotly({
    req(filtered_multi_data())
    df <- filtered_multi_data()
    
    plot_ly(df, x = ~month, y = ~value, color = ~parameter, type = 'scatter', mode = 'lines+markers') %>%
      layout(
        title = "Usporedba parametara kroz vrijeme",
        xaxis = list(title = "Vrijeme", tickformat = "%b %Y"),
        yaxis = list(title = "Vrijednost")
      )
  })
  
  output$avg_city_ui <- renderUI({
    req(input$avg_country)
    cities <- city_country %>%
      filter(Country == input$avg_country) %>%
      pull(City) %>%
      unique()
    
    selectInput("avg_city", "Odaberi grad", choices = cities)
  })
  
  filtered_avg_data <- reactive({
    req(input$avg_city, input$avg_year)
    
    viz_data %>%
      filter(grad == input$avg_city) %>%
      mutate(
        year = lubridate::year(datum),
        month = lubridate::floor_date(datum, "month")
      ) %>%
      filter(year == input$avg_year) %>%
      group_by(month) %>%
      summarise(avg_value = mean(.data[[input$avg_parameter]], na.rm = TRUE), .groups = "drop")
  })
  
  output$avg_year_ui <- renderUI({
    req(viz_data)
    years <- sort(unique(lubridate::year(viz_data$datum)))
    selectInput("avg_year", "Odaberi godinu", choices = years, selected = max(years))
  })
  
  # Novi reaktivni skup za prosjeke svih parametara
  avg_pollutants_data <- reactive({
    req(input$avg_city, input$avg_year)
    
    df <- viz_data %>%
      filter(grad == input$avg_city) %>%
      mutate(year = lubridate::year(datum)) %>%
      filter(year == input$avg_year)
    
    param_cols <- c("pm25", "pm10", "no2", "o3", "so2", "co")
    
    summary_df <- df %>%
      summarise(across(all_of(param_cols), ~mean(.x, na.rm = TRUE))) %>%
      pivot_longer(cols = everything(), names_to = "parameter", values_to = "avg_value") %>%
      arrange(desc(avg_value))
    
    return(summary_df)
  })
  
  # Graf horizontal bar chart za sve parametre
  output$avg_pollutants_plot <- renderPlotly({
    df <- avg_pollutants_data()
    
    plot_ly(df, 
            x = ~avg_value, 
            y = ~reorder(parameter, avg_value),
            type = 'bar',
            orientation = 'h',
            marker = list(color = '#5F85A0')) %>%
      layout(
        title = paste("Top onečišćivači za", input$avg_city, "-", input$avg_year),
        xaxis = list(title = "Prosječna vrijednost"),
        yaxis = list(title = "Parametar")
      )
  })
  
  # Dropdown za gradove - sezonski prikaz
  output$season_city_ui <- render_city_dropdown(reactive(input$season_country), "season_city")
  
  # Priprema podataka za sezonski prikaz
  seasonal_data <- reactive({
    req(input$season_city, input$season_param)
    
    viz_data %>%
      filter(grad == input$season_city) %>%
      mutate(month = lubridate::month(datum, label = TRUE, abbr = TRUE)) %>%
      group_by(month) %>%
      summarise(avg_value = mean(.data[[input$season_param]], na.rm = TRUE), .groups = "drop")
  })
  
  # Graf za sezonski prikaz
  output$seasonal_plot <- renderPlotly({
    df <- seasonal_data()
    
    plot_ly(df, x = ~month, y = ~avg_value, type = 'scatter', mode = 'lines+markers',
            line = list(color = "#7E94BF", width = 3),
            marker = list(size = 8, color = "#7E94BF")) %>%
      layout(
        title = paste("Sezonski profil -", input$season_city, "-", input$season_param),
        xaxis = list(title = "Mjesec"),
        yaxis = list(title = "Prosječna vrijednost")
      )
  })
  
  # City dropdown za godišnji trend
  output$year_trend_city_ui <- render_city_dropdown(reactive(input$year_trend_country), "year_trend_city")
  
  # Godišnji prosjeci za odabrani parametar
  year_trend_data <- reactive({
    req(input$year_trend_city, input$year_trend_parameter)
    
    viz_data %>%
      dplyr::filter(grad == input$year_trend_city) %>%
      dplyr::mutate(year = lubridate::year(datum)) %>%
      dplyr::filter(!is.na(.data[[input$year_trend_parameter]])) %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(avg_value = mean(.data[[input$year_trend_parameter]], na.rm = TRUE),
                       .groups = "drop") %>%
      dplyr::arrange(year)
  })
  
  
  # Grf za godišnji prikaz
  output$year_trend_plot <- renderPlotly({
    df <- year_trend_data()
    
    # Ako ne postoje podaci il imaju za mnje od 2 god
    if (is.null(df) || !is.data.frame(df) || nrow(df) < 2) {
      return(
        plotly::plotly_empty(type = "scatter", mode = "lines") %>%
          layout(
            title = list(text = "Nema dostupnih podataka za odabrane postavke"),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE)
          )
      )
    }
    
    df$year <- as.integer(df$year)
    
    # Prilagođavanje linearne linije trenda
    fit <- lm(avg_value ~ year, data = df)
    slope <- unname(coef(fit)[["year"]])
    
    #Računanje postotne promjene rijekom razdovblja
    pct_change <- 100 *
      (predict(fit, data.frame(year = max(df$year))) -
         predict(fit, data.frame(year = min(df$year)))) /
      mean(df$avg_value, na.rm = TRUE)
    
    subtitle <- sprintf("Trend: %.2f / god (%.1f%% kroz raspon)", slope, pct_change)
    
    #Generiranje točaka za liniju trenda
    xgrid <- seq(min(df$year), max(df$year), by = 1)
    yhat  <- predict(fit, newdata = data.frame(year = xgrid))
    
    # Kreiranje grafa
    p <- plot_ly(df, x = ~year, y = ~avg_value,
                 type = "scatter", mode = "lines+markers",
                 name = "Godišnji prosjek",
                 line = list(color = "#1f77b4"))
    
    # dodavanje linije trenda
    p <- add_trace(p, x = xgrid, y = yhat, type = "scatter", mode = "lines",
                   name = "Trend", line = list(color = "#ff7f0e", dash = "dash"))
    
    #Izgled grafa
    p %>% layout(
      title = list(
        text = paste0(
          "Godišnji trend – ", input$year_trend_city, " – ", toupper(input$year_trend_parameter),
          "<br><sup>", subtitle, "</sup>"
        )
      ),
      xaxis = list(title = "Godina", tickmode = "linear", dtick = 1, tickformat = "d"),
      yaxis = list(title = "Prosječna vrijednost"),
      hovermode = "x unified"
    )
  })
  
  
}
