library(shiny)
library(dplyr)
library(tidyverse)  
library(DT)

prizes <- read_csv("prizes_clean.csv", show_col_types = FALSE)

ui <- navbarPage(
  "British Literary Prizes Dashboard",
  tabPanel("Authors Explorer",
           sidebarLayout(
             sidebarPanel(
               h4("Filter Authors"),
               selectInput(
                 "filter_var", "Choose a filter variable:",
                 choices = c("Ethnicity" = "ethnicity_macro",
                             "Gender"   = "gender",
                             "Genre"    = "prize_genre")
               ),
               uiOutput("filter_value_ui"),

               radioButtons(
                 "role", "Select role:",
                 choices = c("Winners" = "winner",
                             "Shortlisted" = "shortlisted"),
                 inline = TRUE
               )
             ),
             mainPanel(
               h4("Filtered Authors"),
               DTOutput("authors_table"),
               br(),
               h4("Number of Prizes for Selected Group"),
               plotOutput("authors_count_plot")
             )
           )
  ),
  
# Representation by Ethnicity & Gender
  tabPanel("Representation",
           sidebarLayout(
             sidebarPanel(
               h4("Filters"),
               
               # Prize genre filter
               selectInput(
                 "rep_genre", "Prize genre:",
                 choices = c("All", sort(unique(prizes$prize_genre))),
                 selected = "All"
               ),

               checkboxGroupInput(
                 "rep_roles", "Person role:",
                 choices = c("Winner" = "winner",
                             "Shortlisted" = "shortlisted"),
                 selected = c("winner", "shortlisted")
               ),

               sliderInput(
                 "rep_year_range", "Prize year range:",
                 min  = min(prizes$prize_year, na.rm = TRUE),
                 max  = max(prizes$prize_year, na.rm = TRUE),
                 value = c(min(prizes$prize_year, na.rm = TRUE),
                           max(prizes$prize_year, na.rm = TRUE)),
                 step = 1,
                 sep = ""
               )
             ),
             
             mainPanel(
               h3("Are Some Groups More Represented?"),
               p("This page explores representation across ethnicity and gender among prize 
                 winners and shortlisted authors. Use the filters on the left to focus on 
                 specific prize genres, roles, and years."),
               
               h4("Counts by Ethnicity"),
               plotOutput("ethnicity_bar_plot"),
               br(),
               h4("Counts by Gender"),
               plotOutput("gender_bar_plot"),
               br(),
               h4("Summary Table"),
               DTOutput("rep_summary_table")
             )
           )
  ),
  
#Institutions 
  tabPanel("Institutions",
           sidebarLayout(
             sidebarPanel(
               h4("Filters"),

               checkboxGroupInput(
                 "inst_roles", "Person role:",
                 choices = c("Winner" = "winner",
                             "Shortlisted" = "shortlisted"),
                 selected = c("winner", "shortlisted")
               ),
               
               selectInput(
                 "inst_genre", "Prize genre:",
                 choices = c("All", sort(unique(prizes$prize_genre))),
                 selected = "All"
               ),

               sliderInput(
                 "inst_top_n",
                 "Show top N institutions:",
                 min = 5, max = 30, value = 10, step = 1
               )
             ),
             
             mainPanel(
               h3("Which Institutions Appear Most Often?"),
               p("This page looks at which degree institutions appear most frequently 
                 among prize recipients. A high count could reflect the institution’s 
                 size, prestige, or simply that it trains many authors who later become 
                 prominent."),
               
               h4("Top Institutions"),
               plotOutput("inst_bar_plot"),
               br(),
               h4("Institution Summary Table"),
               DTOutput("inst_table")
             )
           )
  )
)

server <- function(input, output, session) {

  output$filter_value_ui <- renderUI({
    req(input$filter_var)
    
    values <- prizes %>%
      filter(!is.na(.data[[input$filter_var]])) %>%
      distinct(.data[[input$filter_var]]) %>%
      pull()
    
    selectInput("filter_value", "Select value:", choices = sort(values))
  })

  filtered_data <- reactive({
    req(input$filter_var, input$filter_value, input$role)
    
    prizes %>%
      filter(
        .data[[input$filter_var]] == input$filter_value,
        person_role == input$role
      ) %>%
      select(
        name, prize_name, prize_year, prize_genre, person_role,
        gender, ethnicity_macro, degree_institution, degree_field
      )
  })

  output$authors_table <- renderDT({
    req(filtered_data())
    datatable(
      filtered_data(),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })

  output$authors_count_plot <- renderPlot({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    df %>%
      count(prize_year) %>%
      ggplot(aes(x = prize_year, y = n)) +
      geom_col() +
      labs(
        x = "Prize year",
        y = "Count",
        title = "Number of prizes for selected group by year"
      ) +
      theme_minimal()
  })
  
#Representation

  rep_data <- reactive({
    df <- prizes

    if (input$rep_genre != "All") {
      df <- df %>% filter(prize_genre == input$rep_genre)
    }

    req(input$rep_roles)
    df <- df %>% filter(person_role %in% input$rep_roles)

    df <- df %>%
      filter(
        !is.na(prize_year),
        prize_year >= input$rep_year_range[1],
        prize_year <= input$rep_year_range[2]
      )
    
    df
  })

  output$ethnicity_bar_plot <- renderPlot({
    df <- rep_data()
    req(nrow(df) > 0)
    
    df %>%
      filter(!is.na(ethnicity_macro)) %>%
      count(ethnicity_macro, person_role) %>%
      ggplot(aes(x = ethnicity_macro, y = n, fill = person_role)) +
      geom_col(position = "dodge") +
      labs(
        x = "Ethnicity (macro)",
        y = "Count",
        fill = "Role",
        title = "Counts by ethnicity and role"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  output$gender_bar_plot <- renderPlot({
    df <- rep_data()
    req(nrow(df) > 0)
    
    df %>%
      filter(!is.na(gender)) %>%
      count(gender, person_role) %>%
      ggplot(aes(x = gender, y = n, fill = person_role)) +
      geom_col(position = "dodge") +
      labs(
        x = "Gender",
        y = "Count",
        fill = "Role",
        title = "Counts by gender and role"
      ) +
      theme_minimal()
  })

  output$rep_summary_table <- renderDT({
    df <- rep_data()
    req(nrow(df) > 0)
    
    df %>%
      count(ethnicity_macro, gender, person_role, sort = TRUE) %>%
      datatable(
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
  })
  
  inst_data <- reactive({
    df <- prizes

    req(input$inst_roles)
    df <- df %>% filter(person_role %in% input$inst_roles)

    if (input$inst_genre != "All") {
      df <- df %>% filter(prize_genre == input$inst_genre)
    }
    
    df %>%
      filter(!is.na(degree_institution), degree_institution != "")
  })

  output$inst_bar_plot <- renderPlot({
    df <- inst_data()
    req(nrow(df) > 0)
    
    top_inst <- df %>%
      count(degree_institution, sort = TRUE) %>%
      slice_head(n = input$inst_top_n) %>%
      mutate(degree_institution = reorder(degree_institution, n))
    
    ggplot(top_inst, aes(x = degree_institution, y = n)) +
      geom_col() +
      coord_flip() +
      labs(
        x = "Degree institution",
        y = "Count of prize recipients",
        title = paste("Top", input$inst_top_n, "degree institutions")
      ) +
      theme_minimal()
  })

  output$inst_table <- renderDT({
    df <- inst_data()
    req(nrow(df) > 0)
    
    df %>%
      count(degree_institution, sort = TRUE) %>%
      datatable(
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
  })
}

shinyApp(ui, server)
