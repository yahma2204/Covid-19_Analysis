ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "Covid-19 Analysis"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem(text = "Overview", tabName = "overview", icon = icon("viruses")),
                        menuItem(text = "More Details", tabName = "data", icon = icon("hospital")),
                        menuItem(text = "Data", tabName = "table", icon = icon("table"))
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "data",
                                fluidRow(
                                  column(
                                    width = 3,
                                    dateRangeInput(inputId = "date", 
                                                        label = "Input date range: ", 
                                                        start = min(covid_clean$date),
                                                        end = max(covid_clean$date), 
                                                        format = "yyyy-mm-dd",
                                                        weekstart = 0,
                                                        language = "en",
                                                        separator = " to ",
                                                        width = NULL,
                                                        autoclose = TRUE
                                         ),
                                  box(width = NULL,
                                         selectInput(
                                           inputId = "country",
                                           label = "Select country : ",
                                           choices = unique(covid_clean$location),
                                           selected = "Indonesia"
                                         )),
                                  box(width = NULL,
                                         checkboxGroupInput(
                                           inputId = "variant",
                                           label = "Select variant : ",
                                           choices = unique(covid_clean$variant),
                                           selected = unique(covid_clean$variant)
                                           
                                         ))
                                      ),
                              column(
                                width=9,
                                
                                fluidRow(
                                    box(
                                      width = 12,
                                      plotlyOutput(outputId = "plot_overview"))),
                                fluidRow(
                                  tabsetPanel(
                                    tabPanel("Top 5 Country",
                                      plotlyOutput(outputId = "plot_5_location")
                                    ),
                                    tabPanel(
                                      "Top 5 Variant",
                                      plotlyOutput("plot_5_variant")
                                    )
                                    ))
                                ))),
                        tabItem(tabName = "overview",
                                fluidRow(
                                  valueBox(width = 4, 
                                           value = comma(sum(covid_clean$jumlah_kasus)),
                                           subtitle = "Total Case",
                                           icon = icon("head-side-mask"),
                                           color = "green"),
                                  valueBox(width = 4,
                                           value = n_distinct(covid_clean$location),
                                           subtitle = "Total Country",
                                           icon = icon("globe-africa"),
                                           color = "green"),
                                  valueBox(width = 4,
                                           value = n_distinct(covid_clean$variant),
                                           subtitle = "Total Variant",
                                           icon = icon("atom"),
                                           color = "green")
                                ),
                              box(width = NULL,
                                  title = "Covid-19 Spread Map",
                                  leafletOutput(outputId = "map_data")
                                  ),
                              fluidRow(
                                
                                  box(plotlyOutput(outputId = "plot_5_top_country")
                                  ),
                                  box(plotlyOutput("plot_5_top_variant")
                                  )
                    )),
                        tabItem(tabName = "table",
                            box(width = NULL,
                            dataTableOutput(outputId = "tabel_data"))
                    )
)))