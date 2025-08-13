library(dplyr)
source("global.R")
library(leaflet)
library(memoise)

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
  
  # Dropdown za grad
  output$corr_city_ui <- render_city_dropdown(reactive(input$corr_country), "corr_city")
  
  # Dropdown za godinu kojeg uvjetu odabir grada - npr ako nema barem 3 zapisa za neku godinu, ta godina se neće pojaviti u dropdownu
  output$corr_year_ui <- renderUI({
    req(input$corr_city)
    yrs <- viz_data %>%
      dplyr::filter(grad == input$corr_city) %>%
      dplyr::mutate(y = lubridate::year(datum)) %>%
      dplyr::count(y, name = "n") %>%
      dplyr::filter(n >= 3) %>%     
      dplyr::pull(y) %>% sort()
    if (!length(yrs)) return(tags$p("Nema dostupnih godina za izabrani grad."))
    selectInput("corr_year", "Odaberi godinu", choices = yrs, selected = max(yrs))
  })
  
  #Prioprema podataka za corr matrix
  corr_data <- reactive({
    req(input$corr_city, input$corr_year)
    
    vars <- c("co","dew","humidity","no2","o3","pm10","pm25",
              "precipitation","pressure","so2","temperature",
              "wind_gust","wind_speed")
    
    df <- viz_data %>%
      dplyr::filter(grad == input$corr_city,
                    lubridate::year(datum) == input$corr_year) %>%
      dplyr::select(dplyr::any_of(vars)) %>%
      dplyr::mutate(dplyr::across(
        dplyr::everything(),
        ~ suppressWarnings(as.numeric(gsub(",", ".", as.character(.))))
      ))
    
    if (nrow(df) == 0) return(NULL)
    
    # ostavlja one stupce koji nemaju više od 4 nedostajuća podatka 
    min_n <- 4
    enough_data <- vapply(df, function(x) sum(!is.na(x)) >= min_n, logical(1))
    df <- df[, enough_data, drop = FALSE]
    
    # ostavlja samo one stupce koji imaju smisla za prikaz korelacija
    non_zero_var <- vapply(df, function(x) {
      x <- x[!is.na(x)]
      length(unique(x)) > 1
    }, logical(1))
    df <- df[, non_zero_var, drop = FALSE]
    
    if (ncol(df) < 2) return(NULL)
    
    df
  })
  
  
  # Matrica korelacija
  output$corr_heatmap <- renderPlotly({
    df <- corr_data()
    if (is.null(df)) {
      return(plotly::plotly_empty(type = "heatmap") %>%
               layout(title = "Nema dovoljno podataka"))
    }
    
    corr_mat <- round(stats::cor(df, use = "pairwise.complete.obs", method = "pearson"), 2)
    
    plot_ly(
      x = colnames(corr_mat),
      y = rownames(corr_mat),
      z = corr_mat,
      type = "heatmap",
      colors = colorRampPalette(c("blue", "white", "red"))(100),
      reversescale = TRUE,
      zmin = -1, zmax = 1,
      text = corr_mat,
      texttemplate = "%{text}",
      textfont = list(size = 12),
      hovertemplate = "<b>%{x}</b> vs <b>%{y}</b><br>Korelacija: %{z}<extra></extra>"
    ) %>% layout(
      title = paste("Korelacijska matrica –", input$corr_city, "-", input$corr_year),
      xaxis = list(title = "", tickangle = -45),
      yaxis = list(title = "", autorange = "reversed")
    )
  })
  
  
  
  # --- KARTA ---
  
  # Predmemorija kako se ne bi api pozivao svaki put u manje od 5 min
  map_cache <- reactiveVal(list(ts = as.POSIXct(0), country = NULL, data = NULL))
  
  # Dohvaća trenutne podatke za sve gradove u odabranoj državi
  fetch_country_aqi <- function(country) {
    cities <- city_country %>%
      dplyr::filter(Country == country) %>%
      dplyr::pull(City) %>%
      unique()
    
    if (length(cities) == 0) return(data.frame())
    
    results <- lapply(cities, function(city) {
      dat <- get_city_data(city, api_key)
      if (is.null(dat)) return(NULL)
      
      data.frame(
        city = city,
        aqi  = suppressWarnings(as.numeric(dat$aqi)),
        dom  = if (!is.null(dat$dominentpol)) dat$dominentpol else NA_character_,
        lat  = if (!is.null(dat$city$geo[[1]])) dat$city$geo[[1]] else NA_real_,
        lon  = if (!is.null(dat$city$geo[[2]])) dat$city$geo[[2]] else NA_real_,
        upd  = if (!is.null(dat$time$s)) dat$time$s else NA_character_,
        stringsAsFactors = FALSE
      )
    })
    
    df <- do.call(rbind, results)
    if (is.null(df)) return(data.frame())
    df %>% dplyr::filter(!is.na(lat), !is.na(lon), !is.na(aqi))
  }
  
  # Provjera je li predmem manje od 5 min stara i ista zemlja
  load_map_data <- function(force = FALSE) {
    cc <- map_cache()
    fresh <- difftime(Sys.time(), cc$ts, units = "mins") < 5 &&
      identical(cc$country, input$map_country) &&
      !is.null(cc$data)
    
    if (!force && fresh) return(cc$data)
    
    withProgress(message = "Dohvaćam podatke...", value = 0, {
      df <- fetch_country_aqi(input$map_country)
    })
    map_cache(list(ts = Sys.time(), country = input$map_country, data = df))
    df
  }
  
  # Početni prikaz karte - hrv
  output$aqi_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 15, lat = 45.5, zoom = 5)
  })
  
  # Ažurira prikaz karte kad se pormijeni država ili klkikne 'osvježi'
  observeEvent(list(input$map_country, input$map_refresh), {
    df <- load_map_data(force = TRUE)
    
    pal <- colorNumeric(
      palette = c("#aedcae", "#ffea61", "#ff9100", "#ff4500", "#9f5ea5", "#7e0023"),
      domain  = c(0, 400), na.color = "#cccccc"
    )
    
    if (nrow(df) == 0) {
      leafletProxy("aqi_map") %>%
        clearMarkers() %>%
        clearControls()
      return(invisible(NULL))
    }
    
    leafletProxy("aqi_map", data = df) %>%
      clearMarkers() %>%
      clearControls() %>%
      fitBounds(lng1 = min(df$lon), lat1 = min(df$lat),
                lng2 = max(df$lon), lat2 = max(df$lat)) %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        radius = ~pmin(18, pmax(6, aqi / 20)),
        fillColor = ~pal(aqi), fillOpacity = 0.9,
        color = "#ffffff", weight = 1,
        popup = ~paste0(
          "<b>", city, "</b><br>",
          "AQI: <b>", aqi, "</b><br>",
          "Dominantni polutant: ", ifelse(is.na(dom), "—", dom), "<br>",
          "Ažurirano: ", ifelse(is.na(upd), "—", upd)
        )
      ) %>%
      addLegend(
        "bottomright", pal = pal, values = ~aqi, title = "AQI",
        labFormat = labelFormat(), opacity = 0.9
      )
  }, ignoreInit = FALSE)
  
  
  
  # --- PREDVIĐANJA---
  
  # Dopdown za grad
  output$fc_city_ui <- render_city_dropdown(reactive(input$fc_country), "fc_city")
  
  #Peiprema mjesečnih vremesnkih serija za grad+oneciscivac
  fc_series <- reactive({
    req(input$fc_city, input$fc_param)
    
    df <- viz_data %>%
      dplyr::filter(grad == input$fc_city) %>%
      dplyr::select(datum, value = dplyr::all_of(input$fc_param)) %>%
      dplyr::mutate(
        datum = as.Date(datum),
        value = suppressWarnings(as.numeric(gsub(",", ".", as.character(value))))
      )
    
    df <- df %>%
      dplyr::mutate(month = lubridate::floor_date(datum, "month")) %>%
      dplyr::group_by(month) %>%
      dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(month)
    
    if (nrow(df) < 6) return(NULL)
    
    full_idx <- tibble::tibble(month = seq(min(df$month), max(df$month), by = "month"))
    dfc <- full_idx %>% dplyr::left_join(df, by = "month")
    
    opts <- if (is.null(input$fc_opts)) character() else input$fc_opts
    
    # outlieri
    if ("deout" %in% opts) {
      x <- dfc$value
      qs <- stats::quantile(x, c(.25, .75), na.rm = TRUE)
      iqr <- qs[2] - qs[1]
      lo <- qs[1] - 1.5 * iqr
      hi <- qs[2] + 1.5 * iqr
      x[x < lo] <- lo; x[x > hi] <- hi
      dfc$value <- x
    }
    
    dfc$value <- forecast::na.interp(dfc$value)
    
    use_log <- "log" %in% opts
    y <- ts(dfc$value,
            start = c(lubridate::year(min(dfc$month)), lubridate::month(min(dfc$month))),
            frequency = 12)
    if (use_log) y <- log(pmax(y, .Machine$double.eps))
    
    list(y = y, hist = dfc, use_log = use_log)
  })
  
  
  
  # Prilagođavanje i prognoza
  fc_result <- reactive({
    req(fc_series(), input$fc_model, input$fc_h, input$fc_level)
    ser <- fc_series(); y <- ser$y; h <- input$fc_h
    lvl <- as.numeric(input$fc_level)
    levels <- sort(unique(c(80, lvl)))
    
    # funkcije predviđanja točaka za tsCV (h = 1)
    pf_arima <- function(y, h) forecast::forecast(forecast::auto.arima(y), h = h)$mean
    pf_ets   <- function(y, h) forecast::forecast(forecast::ets(y), h = h)$mean
    pf_sn    <- function(y, h) forecast::snaive(y, h = h)$mean
    
    # potpuno predviđanje za konačni grafikon/tablicu (s intervalima)
    f_arima <- function(y, h) forecast::forecast(forecast::auto.arima(y), h = h, level = levels)
    f_ets   <- function(y, h) forecast::forecast(forecast::ets(y), h = h, level = levels)
    f_sn    <- function(y, h) forecast::snaive(y,h = h, level = levels)
    
    # Backtesting
    metrics <- NULL
    if (isTRUE(input$fc_compare)) {
      mae  <- function(e) mean(abs(e), na.rm = TRUE)
      rmse <- function(e) sqrt(mean(e^2, na.rm = TRUE))
      
      safe_tsCV <- function(fun) {
        tryCatch(forecast::tsCV(y, forecastfunction = function(y, h) fun(y, h), h = 1),
                 error = function(e) rep(NA_real_, length(y)))
      }
      e_arima <- safe_tsCV(pf_arima)
      e_ets   <- safe_tsCV(pf_ets)
      e_sn    <- safe_tsCV(pf_sn)
      
      metrics <- dplyr::tibble(
        Model = c("Auto ARIMA", "ETS", "Sezonski naivni"),
        Key   = c("arima", "ets", "snaive"),
        MAE   = c(mae(e_arima), mae(e_ets), mae(e_sn)),
        RMSE  = c(rmse(e_arima), rmse(e_ets), rmse(e_sn))
      ) %>% dplyr::arrange(MAE)
    }
    
    # Odabir modela
    selected_key <- input$fc_model
    if (isTRUE(input$fc_compare) && isTRUE(input$fc_auto) &&
        !is.null(metrics) && is.data.frame(metrics) && nrow(metrics) > 0 &&
        "Key" %in% names(metrics)) {
      selected_key <- metrics$Key[1]
    }
    
    chosen <- switch(selected_key,
                     "arima"  = f_arima(y, h),
                     "ets"    = f_ets(y, h),
                     "snaive" = f_sn(y, h),
                     f_arima(y, h))  
    
    list(f = chosen, levels = levels, metrics = metrics, selected_key = selected_key)
  })
  
  
  
  output$fc_metrics <- DT::renderDT({
    res <- fc_result()
    if (is.null(res$metrics) || !is.data.frame(res$metrics) || nrow(res$metrics) == 0) return(NULL)
    DT::datatable(res$metrics[, c("Model","MAE","RMSE")], rownames = FALSE,
                  options = list(dom = 't', paging = FALSE))
  })
  
  
  
  # Graf
  output$fc_plot <- renderPlotly({
    ser <- fc_series(); res <- fc_result(); req(ser, res)
    hist_df <- ser$hist; f <- res$f
    last_m <- max(hist_df$month)
    fut_months <- seq(last_m %m+% months(1), by = "month", length.out = length(f$mean))
    inv <- function(x) if (isTRUE(ser$use_log)) exp(x) else x
    
    cols <- colnames(f$lower)
    has80 <- "80%" %in% cols
    mainLvl <- setdiff(cols, "80%")          
    mainLvl <- if (length(mainLvl)) mainLvl[1] else NULL
    
    df <- data.frame(
      month = fut_months,
      mean  = inv(as.numeric(f$mean)),
      lo80  = if (has80) inv(as.numeric(f$lower[, "80%"])) else NA,
      hi80  = if (has80) inv(as.numeric(f$upper[, "80%"])) else NA,
      loML  = if (!is.null(mainLvl)) inv(as.numeric(f$lower[, mainLvl])) else NA,
      hiML  = if (!is.null(mainLvl)) inv(as.numeric(f$upper[, mainLvl])) else NA
    )
    
    p <- plot_ly()
    p <- add_trace(p, data = hist_df, x = ~month, y = ~value,
                   type = "scatter", mode = "lines+markers", name = "Povijest")
    
    if (!is.null(mainLvl)) {
      p <- add_ribbons(p, data = df, x = ~month, ymin = ~loML, ymax = ~hiML,
                       name = paste0(mainLvl, " interval"), opacity = 0.25, showlegend = TRUE)
    }
    if (has80) {
      p <- add_ribbons(p, data = df, x = ~month, ymin = ~lo80, ymax = ~hi80,
                       name = "80% interval", opacity = 0.35, showlegend = TRUE)
    }
    
    p <- add_trace(p, data = df, x = ~month, y = ~mean,
                   type = "scatter", mode = "lines+markers",
                   name = "Prognoza", line = list(dash = "dash"))
    
    ttl_model <- switch(input$fc_model, arima="Auto ARIMA", ets="ETS", snaive="Sezonski naivni")
    p %>% layout(
      title = paste("Predviđanje –", input$fc_city, "–", toupper(input$fc_param), "–", ttl_model),
      xaxis = list(title = "Mjesec"),
      yaxis = list(title = "Vrijednost"),
      hovermode = "x unified"
    )
    
    
    model_name <- switch(res$selected_key, arima = "Auto ARIMA", ets = "ETS", snaive = "Sezonski naivni")
    p %>% layout(
      title = paste("Predviđanje –", input$fc_city, "–", toupper(input$fc_param), "–", model_name),
      xaxis = list(title = "Mjesec"),
      yaxis = list(title = "Vrijednost"),
      hovermode = "x unified"
    )
    
  })
  
  # Tablica sa prognoziranim vrijendostima
  output$fc_table <- renderDT({
    res <- fc_result()
    ser <- fc_series()
    f <- res$f
    
    last_m <- max(ser$hist$month)
    fut_months <- seq(last_m %m+% months(1), by = "month", length.out = length(f$mean))
    
    out <- data.frame(
      Datum = fut_months,
      Prognoza = round(as.numeric(f$mean), 2),
      Lo80 = round(as.numeric(f$lower[,"80%"]), 2),
      Hi80 = round(as.numeric(f$upper[,"80%"]), 2),
      Lo95 = round(as.numeric(f$lower[,"95%"]), 2),
      Hi95 = round(as.numeric(f$upper[,"95%"]), 2)
    )
    datatable(out, rownames = FALSE, options = list(pageLength = 10, dom = 'tip'))
  })
  
}
