library(shiny)
library(tidycensus)
library(tidyverse)
library(purrr)
library(sf)
library(DT)
library(leaflet)


nice_label <- c(
  "B25077_001" = "Median home value",
  "B19013_001" = "Median household income",
  "B01003_001" = "Total population",
  "B25002_003" = "Vacant housing units",
  "B25002_002" = "Occupied housing units"
)

get_age_data <- function(year) {
  age_vars <- c(
    male_0_9   = "B01001_003",
    male_10_19= "B01001_004",
    male_20_29 = "B01001_005",
    male_30_39= "B01001_006",
    male_40_49 = "B01001_007",
    male_50_59 = "B01001_008",
    male_60_69= "B01001_009",
    male_70p   = "B01001_010",
    
    female_0_9   = "B01001_027",
    female_10_19= "B01001_028",
    female_20_29 = "B01001_029",
    female_30_39 = "B01001_030",
    female_40_49= "B01001_031",
    female_50_59 = "B01001_032",
    female_60_69= "B01001_033",
    female_70p   = "B01001_034"
  )
  
  get_acs(
    geography = "county",
    variables = age_vars,
    state= "WA",
    county = "San Juan",
    year= year
  ) |>
    mutate(
      sex = ifelse(grepl("^male_", variable), "Male", "Female"),
      age_group= gsub("^male_|^female_", "", variable),
      value = ifelse(sex == "Male", -estimate, estimate)
    )
}

ui <- fluidPage(
  titlePanel("San Juan County Explorer (ACS)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "var",
        "Select ACS variable:",
        choices = c(
          "Median home value" = "B25077_001",
          "Median household income" = "B19013_001",
          "Total population" = "B01003_001",
          "Occupied housing units" = "B25002_002",
          "Vacant housing units" = "B25002_003"
        ),
        selected = "B25077_001"
      ),
      
      sliderInput("year", "Year (ACS 5-year):", min= 2013, max = 2023, value= 2023, sep = ""),
      
      checkboxInput("show_moe", "Show margin of error (MOE) in table", value = TRUE),
      
      hr(),
      
      h4("Compare years (difference map)"),
      sliderInput("year_a", "Year A:", min= 2013, max = 2020, value = 2013, sep= ""),
      sliderInput("year_b", "Year B:", min = 2013, max= 2020, value= 2018, sep = ""),
      
      hr(),
      
      radioButtons(
        "map_mode",
        "Map mode:",
        choices = c("Leaflet (interactive)" = "leaflet", "ggplot (static)" = "ggplot"),
        selected = "leaflet"
      ),
      
      downloadButton("download_csv", "Download tract data (CSV)"),
      downloadButton("download_png", "Download map (PNG)"),
      
      hr(),
      helpText("ACS 5-year estimates include sampling uncertainty. Focus on broad patterns.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Map", br(),
          uiOutput("summary_cards"),
          br(),
          
          wellPanel(
            h4("Transportation context"),
            p("San Juan County has no road connection to the mainland."),
            p("Most residents and visitors rely on the Washington State Ferry system."),
            p("Transportation constraints influence housing, employment, and tourism seasonality.")
          ),
          
          conditionalPanel("input.map_mode == 'leaflet'",
                           leafletOutput("leaflet_map", height = "520px")),
          conditionalPanel("input.map_mode == 'ggplot'",
                           plotOutput("gg_map", height = "520px"))
        ),
        
        tabPanel("Trend", br(),
                 plotOutput("trend", height = "420px")),
        
        tabPanel("Compare years", br(),
                 plotOutput("diff_map", height = "520px"),
                 p("This map shows (Year B − Year A) by tract. Interpret carefully: tract estimates have sampling uncertainty.")
        ),
        
        tabPanel("Age structure", br(),
                 plotOutput("age_pyramid", height = "520px"),
                 p("Negative values represent males; positive values represent females.")
        ),
        
        tabPanel("Data", br(),
                 DTOutput("datatable")),
        
        tabPanel("About", br(),
                 h4("What this app does"),
                 tags$ul(
                   tags$li("Maps ACS 5-year estimates at the census tract level for San Juan County."),
                   tags$li("Shows a county-level trend for the same variable."),
                   tags$li("Allows a difference-map comparison between two years."),
                   tags$li("Includes ACS margin of error (MOE) to emphasize uncertainty."),
                   tags$li("Adds an age-structure view (age pyramid) for demographic context.")
                 ),
                 h4("Notes"),
                 p("Tract shapes are large in rural island counties. Treat maps as broad patterns rather than street-level detail."),
                 p("MOE is provided by ACS and reflects sampling uncertainty.")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  tract_data <- reactive({
    get_acs(
      geography = "tract",
      variables = input$var,
      state= "WA",
      county = "San Juan",
      geometry= TRUE,
      year = input$year
    ) %>%
      mutate(var_label= nice_label[input$var])
  })
  
  county_trend <- reactive({
    yrs <- 2013:input$year
    map_df(yrs, \(yr) {
      get_acs(
        geography= "county",
        variables = input$var,
        state= "WA",
        county = "San Juan",
        year= yr
      ) %>% mutate(year= yr)
    })
  })
  
  #seasonal housing share
  seasonal_share <- reactive({
    dat <- get_acs(
      geography= "county",
      variables = c(
        total_units = "B25001_001",
        seasonal_vacant= "B25004_006"
      ),
      state= "WA",
      county = "San Juan",
      year= input$year
    )
    
    total <- dat$estimate[dat$variable == "total_units"]
    seasonal <- dat$estimate[dat$variable == "seasonal_vacant"]

    if (length(total)== 0 || length(seasonal)== 0) return(NA_real_)
    if (is.na(total)|| total== 0) return(NA_real_)
    
    round(100 * seasonal / total, 1)
  })
  
  
  #compare years 
  tract_diff <- reactive({
    a <- get_acs(
      geography = "tract",
      variables = input$var,
      state = "WA",
      county = "San Juan",
      geometry = TRUE,
      year = input$year_a
    ) %>% select(GEOID, NAME, geometry, estimate_a = estimate, moe_a = moe)
    
    b <- get_acs(
      geography = "tract",
      variables = input$var,
      state = "WA",
      county = "San Juan",
      geometry = FALSE,
      year = input$year_b
    ) %>% select(GEOID, estimate_b = estimate, moe_b = moe)
    
    left_join(a, b, by = "GEOID") %>%
      mutate(diff = estimate_b - estimate_a)
  })
  

  output$summary_cards <- renderUI({
    dat <- tract_data()
    vals <- dat$estimate
    
    sshare <- seasonal_share()
    sshare_txt <- ifelse(is.na(sshare), "NA", paste0(sshare, "%"))
    
    div(
      style = "display:flex; gap:12px; flex-wrap:wrap;",
      div(style="padding:10px 14px; border:1px solid #ddd; border-radius:12px;",
          strong("Variable:"), " ", nice_label[input$var]),
      div(style="padding:10px 14px; border:1px solid #ddd; border-radius:12px;",
          strong("Year:"), " ", input$year),
      div(style="padding:10px 14px; border:1px solid #ddd; border-radius:12px;",
          strong("Tract min:"), " ", round(min(vals, na.rm = TRUE), 2)),
      div(style="padding:10px 14px; border:1px solid #ddd; border-radius:12px;",
          strong("Tract median:"), " ", round(median(vals, na.rm = TRUE), 2)),
      div(style="padding:10px 14px; border:1px solid #ddd; border-radius:12px;",
          strong("Tract max:"), " ", round(max(vals, na.rm = TRUE), 2)),
      div(style="padding:10px 14px; border:1px solid #ddd; border-radius:12px;",
          strong("Seasonal housing share:"), " ", sshare_txt)
    )
  })
  
  output$leaflet_map <- renderLeaflet({
    dat <- tract_data()
    pal <- colorNumeric("viridis", domain = dat$estimate, na.color = "#dddddd")
    
    leaflet(dat) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(estimate),
        fillOpacity = 0.75,
        weight = 1,
        color = "white",
        popup = ~paste0(
          "<strong>", NAME, "</strong><br/>",
          nice_label[input$var], ": ", formatC(estimate, format = "f", digits = 0), "<br/>",
          "MOE: ", formatC(moe, format = "f", digits = 0)
        )
      ) %>%
      addLegend("bottomright", pal = pal, values = ~estimate,
                title = paste(nice_label[input$var], input$year))
  })
  
  # ---- ggplot static map ----
  output$gg_map <- renderPlot({
    dat <- tract_data()
    ggplot(dat) +
      geom_sf(aes(fill = estimate), color = "white", linewidth = 0.25) +
      labs(
        title = paste(nice_label[input$var], "by Census Tract"),
        subtitle = paste("San Juan County —", input$year, "(ACS 5-year)"),
        fill = "Estimate"
      ) +
      theme_minimal(base_size = 13)
  })
  
  #trend plot
  output$trend <- renderPlot({
    dat <- county_trend()
    ggplot(dat, aes(x = year, y = estimate)) +
      geom_line() +
      geom_point() +
      labs(
        title = "County trend (ACS 5-year)",
        subtitle = paste("Variable:", nice_label[input$var]),
        x = NULL,
        y = "Estimate"
      ) +
      theme_minimal(base_size = 13)
  })
  
  #difference map plot
  output$diff_map <- renderPlot({
    dat <- tract_diff()
    ggplot(dat) +
      geom_sf(aes(fill = diff), color = "white", linewidth = 0.25) +
      labs(
        title = "Difference map by tract",
        subtitle= paste(nice_label[input$var], ":", input$year_b, "−", input$year_a),
        fill = "Diff"
      ) +
      theme_minimal(base_size = 13)
  })
  
  output$age_pyramid <- renderPlot({
    age_dat <- get_age_data(input$year)
    
    ggplot(age_dat, aes(x = age_group, y = value, fill = sex)) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = abs) +
      labs(
        title= "Age Distribution of San Juan County",
        subtitle = paste("ACS", input$year, "5-year estimates"),
        x = "Age group",
        y = "Population",
        fill = "Sex"
      ) +
      theme_minimal(base_size = 13)
  })
  
  output$datatable <- renderDT({
    dat <- tract_data() %>%
      st_drop_geometry() %>%
      transmute(
        GEOID, NAME,
        estimate,
        moe,
        estimate_minus_moe = estimate - moe,
        estimate_plus_moe = estimate + moe
      )
    
    if (!input$show_moe) {
      dat <- dat %>% select(GEOID, NAME, estimate)
    }
    
    datatable(dat, options = list(pageLength = 10))
  })
  
  output$download_csv <- downloadHandler(
    filename = function() paste0("sanjuan_tract_", input$var, "_", input$year, ".csv"),
    content= function(file) {
      dat <- tract_data() %>% st_drop_geometry()
      write.csv(dat, file, row.names = FALSE)
    }
  )
  
  output$download_png <- downloadHandler(
    filename = function() paste0("sanjuan_map_", input$var, "_", input$year, ".png"),
    content = function(file) {
      dat <- tract_data()
      p <- ggplot(dat) +
        geom_sf(aes(fill = estimate), color = "white", linewidth = 0.25) +
        labs(
          title= paste(nice_label[input$var], "by Census Tract"),
          subtitle = paste("San Juan County —", input$year, "(ACS 5-year)"),
          fill= "Estimate"
        ) +
        theme_minimal(base_size = 13)
      
      ggsave(file, p, width = 9, height = 6, dpi = 200)
    }
  )
}

shinyApp(ui, server)
