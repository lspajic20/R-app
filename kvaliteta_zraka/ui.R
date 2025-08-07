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
                    h4("Usporedba parametara kroz vrijeme"),
                    
                    fluidRow(
                      column(4, selectInput("viz_country", "Odaberi državu", 
                                            choices = unique(city_country$Country), selected = "Croatia")),
                      column(4, uiOutput("viz_city_ui")),
                      column(4, uiOutput("time_filter_ui"))
                    ),
                    
                    tags$hr(),
                    fluidRow(
                      column(12,
                             tags$div(
                               id = "param_ui_container", 
                               style = "max-height: 300px",
                               
                               div(id = "param_1_row",
                                   fluidRow(
                                     column(11,
                                            selectInput("param_1", "Parametar 1", 
                                                        choices = names(viz_data)[!names(viz_data) %in% c("datum", "grad")], 
                                                        selected = "pm25")
                                     ),
                                     column(1,
                                            actionButton("remove_param_1", "−", style = "margin-top:25px;")
                                     )
                                   )
                               )
                             ),
                             actionButton("add_param", "Dodaj", style= " background-color:#B4C4D9; border-color:#7E94BF; font-weight:bold;",
                                          style = "margin-top:10px;")
                      )
                    )
                    
                )
              ),
              fluidRow(
                box(width = 12, solidHeader = TRUE,
                    plotlyOutput("multi_param_plot"))
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