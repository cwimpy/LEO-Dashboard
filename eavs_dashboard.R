# EAVS Shiny Dashboard
# Election Administration and Voting Survey Data Visualization

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)
library(leaflet)
library(readr)
library(scales)

# Load the cleaned data (assumes data preparation script has been run)
load_dashboard_data <- function() {
  data_dir <- "dashboard_data"
  
  if (!dir.exists(data_dir)) {
    stop("Dashboard data directory not found. Please run the data preparation script first.")
  }
  
  cat("Loading dashboard data...\n")
  
  # Load with error handling
  tryCatch({
    jurisdiction_data <- read_csv(file.path(data_dir, "jurisdiction_data.csv"), 
                                  show_col_types = FALSE)
    
    # Debug: Check column names
    cat("Jurisdiction data columns (first 10):\n")
    print(head(names(jurisdiction_data), 10))
    
    # Check for empty column names
    empty_cols <- which(names(jurisdiction_data) == "" | 
                          is.na(names(jurisdiction_data)) | 
                          nchar(names(jurisdiction_data)) == 0)
    
    if (length(empty_cols) > 0) {
      cat("Warning: Found empty column names, removing them.\n")
      jurisdiction_data <- jurisdiction_data[, -empty_cols]
    }
    
    # Fix any remaining column name issues
    names(jurisdiction_data) <- make.names(names(jurisdiction_data), unique = TRUE)
    
    result <- list(
      jurisdiction = jurisdiction_data,
      state_summary = read_csv(file.path(data_dir, "state_summary.csv"), show_col_types = FALSE),
      national_summary = read_csv(file.path(data_dir, "national_summary.csv"), show_col_types = FALSE),
      overview = read_csv(file.path(data_dir, "overview_data.csv"), show_col_types = FALSE),
      registration = read_csv(file.path(data_dir, "registration_data.csv"), show_col_types = FALSE),
      mail_voting = read_csv(file.path(data_dir, "mail_voting_data.csv"), show_col_types = FALSE),
      provisional = read_csv(file.path(data_dir, "provisional_data.csv"), show_col_types = FALSE),
      polling = read_csv(file.path(data_dir, "polling_data.csv"), show_col_types = FALSE),
      uocava = read_csv(file.path(data_dir, "uocava_data.csv"), show_col_types = FALSE)
    )
    
    cat("Data loaded successfully!\n")
    return(result)
    
  }, error = function(e) {
    cat("Error loading data:", e$message, "\n")
    stop(e)
  })
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Rural LEO Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("chart-line")),
      menuItem("Registration", tabName = "registration", icon = icon("user-plus")),
      menuItem("Turnout Analysis", tabName = "turnout", icon = icon("vote-yea")),
      menuItem("Mail Voting", tabName = "mail_voting", icon = icon("envelope")),
      menuItem("Provisional Ballots", tabName = "provisional", icon = icon("question-circle")),
      menuItem("Polling Operations", tabName = "polling", icon = icon("building")),
      menuItem("UOCAVA", tabName = "uocava", icon = icon("globe")),
      menuItem("Jurisdiction Profile", tabName = "jurisdiction_profile", icon = icon("map-marker-alt")),
      menuItem("Data Explorer", tabName = "explorer", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .small-box {
          border-radius: 5px;
        }
        .nav-tabs-custom > .nav-tabs > li.active {
          border-top-color: #3c8dbc;
        }
      "))
    ),
    
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_jurisdictions"),
                valueBoxOutput("total_registered"),
                valueBoxOutput("national_turnout")
              ),
              
              fluidRow(
                box(
                  title = "Turnout Rate by State", status = "primary", solidHeader = TRUE,
                  width = 6, height = 450,
                  plotlyOutput("state_turnout_plot")
                ),
                box(
                  title = "Jurisdiction Size Distribution", status = "primary", solidHeader = TRUE,
                  width = 6, height = 450,
                  plotlyOutput("jurisdiction_size_plot")
                )
              ),
              
              fluidRow(
                box(
                  title = "Regional Comparison", status = "primary", solidHeader = TRUE,
                  width = 12, height = 400,
                  plotlyOutput("regional_comparison")
                )
              )
      ),
      
      # Registration Tab
      tabItem(tabName = "registration",
              fluidRow(
                box(
                  title = "Filters", status = "primary", solidHeader = TRUE,
                  width = 3, height = 200,
                  selectInput("reg_state", "Select State:", 
                              choices = NULL, multiple = TRUE),
                  selectInput("reg_size", "Jurisdiction Size:", 
                              choices = NULL, multiple = TRUE)
                ),
                box(
                  title = "Registration Summary", status = "info", solidHeader = TRUE,
                  width = 9, height = 200,
                  DT::dataTableOutput("reg_summary_table")
                )
              ),
              
              fluidRow(
                box(
                  title = "Registration Methods", status = "primary", solidHeader = TRUE,
                  width = 6, height = 400,
                  plotlyOutput("registration_methods_plot")
                ),
                box(
                  title = "Active vs Inactive Registrations", status = "primary", solidHeader = TRUE,
                  width = 6, height = 400,
                  plotlyOutput("active_inactive_plot")
                )
              )
      ),
      
      # Turnout Analysis Tab
      tabItem(tabName = "turnout",
              fluidRow(
                box(
                  title = "Turnout Rate Distribution", status = "primary", solidHeader = TRUE,
                  width = 6, height = 400,
                  plotlyOutput("turnout_distribution")
                ),
                box(
                  title = "Voting Methods", status = "primary", solidHeader = TRUE,
                  width = 6, height = 400,
                  plotlyOutput("voting_methods_plot")
                )
              ),
              
              fluidRow(
                box(
                  title = "Turnout vs Registration Scatterplot", status = "primary", solidHeader = TRUE,
                  width = 12, height = 450,
                  plotlyOutput("turnout_scatter")
                )
              )
      ),
      
      # Mail Voting Tab
      tabItem(tabName = "mail_voting",
              fluidRow(
                valueBoxOutput("mail_sent_total"),
                valueBoxOutput("mail_return_rate"),
                valueBoxOutput("mail_acceptance_rate")
              ),
              
              fluidRow(
                box(
                  title = "Mail Ballot Return Rates by State", status = "primary", solidHeader = TRUE,
                  width = 8, height = 450,
                  plotlyOutput("mail_return_by_state")
                ),
                box(
                  title = "Mail Voting Statistics", status = "info", solidHeader = TRUE,
                  width = 4, height = 450,
                  DT::dataTableOutput("mail_stats_table")
                )
              )
      ),
      
      # Provisional Ballots Tab
      tabItem(tabName = "provisional",
              fluidRow(
                box(
                  title = "Provisional Ballot Outcomes", status = "primary", solidHeader = TRUE,
                  width = 6, height = 400,
                  plotlyOutput("provisional_outcomes")
                ),
                box(
                  title = "Provisional Rates by State", status = "primary", solidHeader = TRUE,
                  width = 6, height = 400,
                  plotlyOutput("provisional_by_state")
                )
              )
      ),
      
      # Polling Operations Tab
      tabItem(tabName = "polling",
              fluidRow(
                box(
                  title = "Poll Worker Ratios", status = "primary", solidHeader = TRUE,
                  width = 6, height = 400,
                  plotlyOutput("poll_worker_ratio")
                ),
                box(
                  title = "Polling Places per Jurisdiction", status = "primary", solidHeader = TRUE,
                  width = 6, height = 400,
                  plotlyOutput("polling_places_plot")
                )
              )
      ),
      
      # UOCAVA Tab
      tabItem(tabName = "uocava",
              fluidRow(
                box(
                  title = "UOCAVA Ballot Return Rates", status = "primary", solidHeader = TRUE,
                  width = 6, height = 400,
                  plotlyOutput("uocava_return_rates")
                ),
                box(
                  title = "UOCAVA by State", status = "primary", solidHeader = TRUE,
                  width = 6, height = 400,
                  plotlyOutput("uocava_by_state")
                )
              )
      ),
      
      # Jurisdiction Profile Tab
      tabItem(tabName = "jurisdiction_profile",
              fluidRow(
                box(
                  title = "Select Jurisdiction", status = "primary", solidHeader = TRUE,
                  width = 4, height = 150,
                  selectInput("profile_state", "Select State:", 
                              choices = NULL, selected = NULL),
                  selectInput("profile_jurisdiction", "Select Jurisdiction:", 
                              choices = NULL, selected = NULL)
                ),
                box(
                  title = "Quick Stats", status = "info", solidHeader = TRUE,
                  width = 8, height = 150,
                  div(id = "quick_stats_content",
                      style = "padding: 10px;",
                      h4("Select a jurisdiction to view detailed statistics")
                  )
                )
              ),
              
              # Main jurisdiction profile content (initially hidden)
              conditionalPanel(
                condition = "input.profile_jurisdiction != ''",
                
                fluidRow(
                  valueBoxOutput("jurisdiction_registered", width = 3),
                  valueBoxOutput("jurisdiction_turnout", width = 3),
                  valueBoxOutput("jurisdiction_mail_rate", width = 3),
                  valueBoxOutput("jurisdiction_size_category", width = 3)
                ),
                
                fluidRow(
                  box(
                    title = "Registration Details", status = "primary", solidHeader = TRUE,
                    width = 6, height = 350,
                    tableOutput("jurisdiction_registration_table")
                  ),
                  box(
                    title = "Voting Methods Breakdown", status = "primary", solidHeader = TRUE,
                    width = 6, height = 350,
                    plotlyOutput("jurisdiction_voting_methods")
                  )
                ),
                
                fluidRow(
                  box(
                    title = "Mail Voting Performance", status = "primary", solidHeader = TRUE,
                    width = 4, height = 300,
                    tableOutput("jurisdiction_mail_table")
                  ),
                  box(
                    title = "Provisional Ballots", status = "primary", solidHeader = TRUE,
                    width = 4, height = 300,
                    tableOutput("jurisdiction_provisional_table")
                  ),
                  box(
                    title = "Similar Jurisdictions", status = "warning", solidHeader = TRUE,
                    width = 4, height = 300,
                    div(
                      style = "text-align: center; padding: 20px;",
                      h5("Find Similar Jurisdictions"),
                      br(),
                      actionButton("find_similar_size", "Similar by Size", 
                                   class = "btn-warning", style = "margin: 5px;"),
                      br(),
                      actionButton("find_similar_demographics", "Similar Demographics", 
                                   class = "btn-warning", style = "margin: 5px;"),
                      br(),
                      actionButton("find_similar_performance", "Similar Performance", 
                                   class = "btn-warning", style = "margin: 5px;"),
                      br(),
                      hr(),
                      div(id = "similar_jurisdictions_list")
                    )
                  )
                ),
                
                fluidRow(
                  box(
                    title = "Complete Jurisdiction Data", status = "info", solidHeader = TRUE,
                    width = 12, height = 400, collapsible = TRUE, collapsed = TRUE,
                    DT::dataTableOutput("jurisdiction_complete_data")
                  )
                )
              )
      ),
      
      # Data Explorer Tab
      tabItem(tabName = "explorer",
              fluidRow(
                box(
                  title = "Data Explorer", status = "primary", solidHeader = TRUE,
                  width = 12,
                  DT::dataTableOutput("data_explorer_table")
                )
              )
      )
    )
  )
)

# Server - Minimal version for debugging
server <- function(input, output, session) {
  
  cat("=== SERVER STARTING ===\n")
  
  # Load data with extensive error checking
  data <- reactive({
    cat("=== ATTEMPTING TO LOAD DATA ===\n")
    tryCatch({
      loaded_data <- load_dashboard_data()
      
      # Detailed data inspection
      cat("=== DETAILED DATA INSPECTION ===\n")
      
      # Check jurisdiction data
      if (!is.null(loaded_data$jurisdiction)) {
        cat("Jurisdiction data rows:", nrow(loaded_data$jurisdiction), "\n")
        cat("Jurisdiction data columns:", ncol(loaded_data$jurisdiction), "\n")
        cat("All column names:\n")
        print(names(loaded_data$jurisdiction))
        
        # Check specific columns we need
        key_cols <- c("a1a", "a1b", "f1a", "state_abbr", "jurisdiction_name")
        for (col in key_cols) {
          if (col %in% names(loaded_data$jurisdiction)) {
            non_na_count <- sum(!is.na(loaded_data$jurisdiction[[col]]))
            cat(col, "- Non-NA values:", non_na_count, "\n")
            
            if (non_na_count > 0) {
              if (is.numeric(loaded_data$jurisdiction[[col]])) {
                cat("  Range:", min(loaded_data$jurisdiction[[col]], na.rm = TRUE), 
                    "to", max(loaded_data$jurisdiction[[col]], na.rm = TRUE), "\n")
              } else {
                unique_vals <- unique(loaded_data$jurisdiction[[col]])
                cat("  Sample values:", paste(head(unique_vals, 3), collapse = ", "), "\n")
              }
            }
          } else {
            cat(col, "- MISSING from data\n")
          }
        }
      }
      
      # Check other datasets
      for (dataset_name in names(loaded_data)) {
        if (dataset_name != "jurisdiction" && !is.null(loaded_data[[dataset_name]])) {
          cat(dataset_name, "data rows:", nrow(loaded_data[[dataset_name]]), "\n")
        } else if (is.null(loaded_data[[dataset_name]])) {
          cat(dataset_name, "- IS NULL\n")
        }
      }
      
      cat("=== DATA INSPECTION COMPLETE ===\n")
      return(loaded_data)
      
    }, error = function(e) {
      cat("ERROR in data reactive:", e$message, "\n")
      cat("Call stack:", paste(capture.output(traceback()), collapse = "\n"), "\n")
      return(NULL)
    })
  })
  
  cat("=== DATA REACTIVE CREATED ===\n")
  
  # Test basic outputs first
  output$total_jurisdictions <- renderValueBox({
    cat("=== RENDERING TOTAL JURISDICTIONS ===\n")
    tryCatch({
      if (is.null(data())) {
        cat("Data is NULL\n")
        return(valueBox(0, "Loading...", icon = icon("hourglass")))
      }
      
      cat("Data is not NULL, checking jurisdiction data\n")
      if (is.null(data()$jurisdiction)) {
        cat("Jurisdiction data is NULL\n")
        return(valueBox(0, "No Data", icon = icon("exclamation")))
      }
      
      row_count <- nrow(data()$jurisdiction)
      cat("Row count:", row_count, "\n")
      
      valueBox(
        value = row_count,
        subtitle = "Total Jurisdictions",
        icon = icon("building"),
        color = "blue"
      )
    }, error = function(e) {
      cat("ERROR in total_jurisdictions:", e$message, "\n")
      valueBox("Error", "Check Console", icon = icon("exclamation"), color = "red")
    })
  })
  
  cat("=== TOTAL JURISDICTIONS OUTPUT CREATED ===\n")
  
  # Add the second output
  output$total_registered <- renderValueBox({
    cat("=== RENDERING TOTAL REGISTERED ===\n")
    tryCatch({
      if (is.null(data())) {
        cat("Data is NULL for total_registered\n")
        return(valueBox(0, "Loading...", icon = icon("hourglass")))
      }
      
      cat("Checking for a1a column\n")
      if (!"a1a" %in% names(data()$jurisdiction)) {
        cat("Column a1a not found\n")
        return(valueBox(0, "No A1A Data", icon = icon("exclamation")))
      }
      
      total_reg <- sum(data()$jurisdiction$a1a, na.rm = TRUE)
      cat("Total registered calculated:", total_reg, "\n")
      
      valueBox(
        value = paste(round(total_reg / 1000000, 1), "M"),
        subtitle = "Total Registered Voters",
        icon = icon("users"),
        color = "green"
      )
    }, error = function(e) {
      cat("ERROR in total_registered:", e$message, "\n")
      valueBox("Error", "Check Console", icon = icon("exclamation"), color = "red")
    })
  })
  
  cat("=== TOTAL REGISTERED OUTPUT CREATED ===\n")
  
  # Add the third output
  output$national_turnout <- renderValueBox({
    cat("=== RENDERING NATIONAL TURNOUT ===\n")
    tryCatch({
      if (is.null(data())) {
        cat("Data is NULL for national_turnout\n")
        return(valueBox(0, "Loading...", icon = icon("hourglass")))
      }
      
      cat("Checking for national_summary\n")
      if (is.null(data()$national_summary)) {
        cat("National summary is NULL\n")
        return(valueBox(0, "No Summary", icon = icon("exclamation")))
      }
      
      nat_summary <- data()$national_summary
      cat("National summary columns:", names(nat_summary), "\n")
      
      if (!"national_turnout_rate" %in% names(nat_summary)) {
        cat("national_turnout_rate column not found\n")
        return(valueBox("N/A", "No Turnout Data", icon = icon("vote-yea"), color = "yellow"))
      }
      
      turnout_rate <- nat_summary$national_turnout_rate
      cat("National turnout rate:", turnout_rate, "\n")
      
      valueBox(
        value = paste0(turnout_rate, "%"),
        subtitle = "National Turnout Rate",
        icon = icon("vote-yea"),
        color = "yellow"
      )
    }, error = function(e) {
      cat("ERROR in national_turnout:", e$message, "\n")
      valueBox("Error", "Check Console", icon = icon("exclamation"), color = "red")
    })
  })
  
  cat("=== NATIONAL TURNOUT OUTPUT CREATED ===\n")
  
  # Add plots one by one
  output$state_turnout_plot <- renderPlotly({
    cat("=== RENDERING STATE TURNOUT PLOT ===\n")
    tryCatch({
      if (is.null(data()) || is.null(data()$state_summary)) {
        cat("No state summary data\n")
        return(plotly_empty())
      }
      
      state_data <- data()$state_summary %>%
        filter(!is.na(state_turnout_rate), state_turnout_rate > 0) %>%
        arrange(desc(state_turnout_rate)) %>%
        head(20)
      
      cat("State data rows for plot:", nrow(state_data), "\n")
      
      if (nrow(state_data) == 0) return(plotly_empty())
      
      p <- ggplot(state_data, aes(x = reorder(state_abbr, state_turnout_rate), y = state_turnout_rate)) +
        geom_col(fill = "steelblue") +
        coord_flip() +
        labs(title = "Top 20 States by Turnout Rate",
             x = "State", y = "Turnout Rate (%)") +
        theme_minimal()
      
      ggplotly(p)
    }, error = function(e) {
      cat("ERROR in state_turnout_plot:", e$message, "\n")
      plotly_empty()
    })
  })
  
  cat("=== STATE TURNOUT PLOT CREATED ===\n")
  
  output$jurisdiction_size_plot <- renderPlotly({
    cat("=== RENDERING JURISDICTION SIZE PLOT ===\n")
    tryCatch({
      if (is.null(data()) || is.null(data()$overview)) {
        cat("No overview data\n")
        return(plotly_empty())
      }
      
      size_data <- data()$overview %>%
        filter(!is.na(jurisdiction_size)) %>%
        count(jurisdiction_size)
      
      cat("Size data rows:", nrow(size_data), "\n")
      
      if (nrow(size_data) == 0) return(plotly_empty())
      
      p <- ggplot(size_data, aes(x = jurisdiction_size, y = n, fill = jurisdiction_size)) +
        geom_col() +
        labs(title = "Jurisdictions by Size",
             x = "Size Category", y = "Number of Jurisdictions") +
        theme_minimal() +
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p)
    }, error = function(e) {
      cat("ERROR in jurisdiction_size_plot:", e$message, "\n")
      plotly_empty()
    })
  })
  
  cat("=== JURISDICTION SIZE PLOT CREATED ===\n")
  
  output$regional_comparison <- renderPlotly({
    cat("=== RENDERING REGIONAL COMPARISON ===\n")
    tryCatch({
      if (is.null(data()) || is.null(data()$jurisdiction)) {
        cat("No jurisdiction data for regional comparison\n")
        return(plotly_empty())
      }
      
      regional_data <- data()$jurisdiction %>%
        filter(!is.na(region), !is.na(turnout_rate), !is.na(mail_return_rate)) %>%
        group_by(region) %>%
        summarise(
          avg_turnout = mean(turnout_rate, na.rm = TRUE),
          avg_mail_return = mean(mail_return_rate, na.rm = TRUE),
          jurisdictions = n(),
          .groups = "drop"
        ) %>%
        pivot_longer(cols = c(avg_turnout, avg_mail_return), 
                     names_to = "metric", values_to = "rate")
      
      cat("Regional data rows:", nrow(regional_data), "\n")
      
      if (nrow(regional_data) == 0) return(plotly_empty())
      
      p <- ggplot(regional_data, aes(x = region, y = rate, fill = metric)) +
        geom_col(position = "dodge") +
        labs(title = "Regional Averages",
             x = "Region", y = "Rate (%)") +
        theme_minimal() +
        scale_fill_discrete(name = "Metric", 
                            labels = c("Turnout Rate", "Mail Return Rate"))
      
      ggplotly(p)
    }, error = function(e) {
      cat("ERROR in regional_comparison:", e$message, "\n")
      plotly_empty()
    })
  })
  
  cat("=== REGIONAL COMPARISON CREATED ===\n")
  
  # Registration Tab Functionality
  observe({
    cat("=== SETTING UP REGISTRATION FILTERS ===\n")
    tryCatch({
      if (!is.null(data()) && !is.null(data()$registration)) {
        states <- sort(unique(data()$registration$state_abbr))
        states <- states[!is.na(states)]
        
        sizes <- sort(unique(data()$registration$jurisdiction_size))
        sizes <- sizes[!is.na(sizes)]
        
        cat("Registration states:", length(states), "\n")
        cat("Registration sizes:", length(sizes), "\n")
        
        updateSelectInput(session, "reg_state", choices = states)
        updateSelectInput(session, "reg_size", choices = sizes)
      }
    }, error = function(e) {
      cat("ERROR setting up registration filters:", e$message, "\n")
    })
  })
  
  output$reg_summary_table <- DT::renderDataTable({
    cat("=== RENDERING REGISTRATION SUMMARY TABLE ===\n")
    tryCatch({
      if (is.null(data()) || is.null(data()$state_summary)) {
        cat("No state summary data for registration table\n")
        return(data.frame())
      }
      
      table_data <- data()$state_summary %>%
        select(state_abbr, jurisdictions, total_registered, total_active) %>%
        arrange(desc(total_registered)) %>%
        head(10)
      
      cat("Registration summary table rows:", nrow(table_data), "\n")
      return(table_data)
      
    }, error = function(e) {
      cat("ERROR in reg_summary_table:", e$message, "\n")
      return(data.frame())
    })
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  # Mail Voting Tab Functionality
  output$mail_sent_total <- renderValueBox({
    cat("=== RENDERING MAIL SENT TOTAL ===\n")
    tryCatch({
      if (is.null(data()) || is.null(data()$mail_voting)) {
        cat("No mail voting data\n")
        return(valueBox(0, "Loading...", icon = icon("hourglass")))
      }
      
      total_sent <- sum(data()$mail_voting$c1a, na.rm = TRUE)
      cat("Total mail sent:", total_sent, "\n")
      
      valueBox(
        value = paste(round(total_sent / 1000000, 1), "M"),
        subtitle = "Mail Ballots Sent",
        icon = icon("envelope"),
        color = "blue"
      )
    }, error = function(e) {
      cat("ERROR in mail_sent_total:", e$message, "\n")
      valueBox("Error", "Check Console", icon = icon("exclamation"), color = "red")
    })
  })
  
  output$mail_return_rate <- renderValueBox({
    cat("=== RENDERING MAIL RETURN RATE ===\n")
    tryCatch({
      if (is.null(data()) || is.null(data()$national_summary)) {
        cat("No national summary for mail return rate\n")
        return(valueBox(0, "Loading...", icon = icon("hourglass")))
      }
      
      nat_summary <- data()$national_summary
      if (!"national_mail_return_rate" %in% names(nat_summary)) {
        cat("national_mail_return_rate column missing\n")
        return(valueBox("N/A", "No Data", icon = icon("reply"), color = "green"))
      }
      
      valueBox(
        value = paste0(round(nat_summary$national_mail_return_rate, 1), "%"),
        subtitle = "National Mail Return Rate",
        icon = icon("reply"),
        color = "green"
      )
    }, error = function(e) {
      cat("ERROR in mail_return_rate:", e$message, "\n")
      valueBox("Error", "Check Console", icon = icon("exclamation"), color = "red")
    })
  })
  
  output$mail_acceptance_rate <- renderValueBox({
    cat("=== RENDERING MAIL ACCEPTANCE RATE ===\n")
    tryCatch({
      if (is.null(data()) || is.null(data()$mail_voting)) {
        cat("No mail voting data for acceptance rate\n")
        return(valueBox(0, "Loading...", icon = icon("hourglass")))
      }
      
      # Calculate acceptance rate from mail voting data
      total_returned <- sum(data()$mail_voting$c1b, na.rm = TRUE)
      total_counted <- sum(data()$mail_voting$c8a, na.rm = TRUE)
      
      acceptance_rate <- if (total_returned > 0) {
        round((total_counted / total_returned) * 100, 1)
      } else {
        0
      }
      
      cat("Mail acceptance rate calculated:", acceptance_rate, "%\n")
      
      valueBox(
        value = paste0(acceptance_rate, "%"),
        subtitle = "Mail Acceptance Rate",
        icon = icon("check"),
        color = "yellow"
      )
    }, error = function(e) {
      cat("ERROR in mail_acceptance_rate:", e$message, "\n")
      valueBox("Error", "Check Console", icon = icon("exclamation"), color = "red")
    })
  })
  
  # Data Explorer Tab
  output$data_explorer_table <- DT::renderDataTable({
    cat("=== RENDERING DATA EXPLORER TABLE ===\n")
    tryCatch({
      if (is.null(data()) || is.null(data()$overview)) {
        cat("No overview data for explorer\n")
        return(data.frame())
      }
      
      explorer_data <- data()$overview %>%
        select(jurisdiction_name, state_abbr, jurisdiction_size, a1a, f1a, turnout_rate, mail_return_rate) %>%
        arrange(desc(f1a))
      
      cat("Data explorer rows:", nrow(explorer_data), "\n")
      return(explorer_data)
      
    }, error = function(e) {
      cat("ERROR in data_explorer_table:", e$message, "\n")
      return(data.frame())
    })
  }, options = list(pageLength = 25, scrollX = TRUE))
  
  cat("=== ADDITIONAL TABS FUNCTIONALITY ADDED ===\n")

  # ============================================================================
  # JURISDICTION PROFILE TAB - Complete Implementation
  # ============================================================================

  # Populate state dropdown for jurisdiction profile
  observe({
    cat("=== SETTING UP JURISDICTION PROFILE STATE FILTER ===\n")
    tryCatch({
      if (!is.null(data()) && !is.null(data()$jurisdiction)) {
        states <- data()$jurisdiction %>%
          select(state_abbr, state_full) %>%
          distinct() %>%
          arrange(state_abbr)

        state_choices <- setNames(states$state_abbr, paste0(states$state_abbr, " - ", states$state_full))

        cat("Profile states available:", length(state_choices), "\n")
        updateSelectInput(session, "profile_state", choices = c("Select a state" = "", state_choices))
      }
    }, error = function(e) {
      cat("ERROR setting up profile state filter:", e$message, "\n")
    })
  })

  # Update jurisdiction dropdown based on selected state
  observe({
    cat("=== UPDATING JURISDICTION DROPDOWN ===\n")
    tryCatch({
      req(input$profile_state)

      if (input$profile_state != "" && !is.null(data()) && !is.null(data()$jurisdiction)) {
        jurisdictions <- data()$jurisdiction %>%
          filter(state_abbr == input$profile_state) %>%
          arrange(jurisdiction_name) %>%
          select(fips_code, jurisdiction_name)

        jurisdiction_choices <- setNames(jurisdictions$fips_code, jurisdictions$jurisdiction_name)

        cat("Jurisdictions for", input$profile_state, ":", length(jurisdiction_choices), "\n")
        updateSelectInput(session, "profile_jurisdiction",
                         choices = c("Select a jurisdiction" = "", jurisdiction_choices))
      } else {
        updateSelectInput(session, "profile_jurisdiction", choices = c("Select a jurisdiction" = ""))
      }
    }, error = function(e) {
      cat("ERROR updating jurisdiction dropdown:", e$message, "\n")
    })
  })

  # Reactive expression for selected jurisdiction data
  selected_jurisdiction <- reactive({
    cat("=== GETTING SELECTED JURISDICTION DATA ===\n")
    req(input$profile_jurisdiction, input$profile_jurisdiction != "")

    tryCatch({
      if (is.null(data()) || is.null(data()$jurisdiction)) {
        cat("No jurisdiction data available\n")
        return(NULL)
      }

      jurisdiction_data <- data()$jurisdiction %>%
        filter(fips_code == input$profile_jurisdiction)

      if (nrow(jurisdiction_data) == 0) {
        cat("No data found for FIPS code:", input$profile_jurisdiction, "\n")
        return(NULL)
      }

      cat("Selected jurisdiction:", jurisdiction_data$jurisdiction_name[1], "\n")
      return(jurisdiction_data)

    }, error = function(e) {
      cat("ERROR getting jurisdiction data:", e$message, "\n")
      return(NULL)
    })
  })

  # Value boxes for jurisdiction profile
  output$jurisdiction_registered <- renderValueBox({
    cat("=== RENDERING JURISDICTION REGISTERED ===\n")
    tryCatch({
      juris <- selected_jurisdiction()
      req(juris)

      registered <- juris$a1a
      if (is.na(registered)) registered <- 0

      valueBox(
        value = format(registered, big.mark = ","),
        subtitle = "Total Registered Voters",
        icon = icon("users"),
        color = "blue"
      )
    }, error = function(e) {
      cat("ERROR in jurisdiction_registered:", e$message, "\n")
      valueBox("N/A", "Registered Voters", icon = icon("exclamation"), color = "red")
    })
  })

  output$jurisdiction_turnout <- renderValueBox({
    cat("=== RENDERING JURISDICTION TURNOUT ===\n")
    tryCatch({
      juris <- selected_jurisdiction()
      req(juris)

      # Calculate turnout rate
      turnout_rate <- if (!is.na(juris$turnout_rate)) {
        paste0(round(juris$turnout_rate, 1), "%")
      } else if (!is.na(juris$f1a) && !is.na(juris$a1a) && juris$a1a > 0) {
        paste0(round((juris$f1a / juris$a1a) * 100, 1), "%")
      } else {
        "N/A"
      }

      valueBox(
        value = turnout_rate,
        subtitle = "Turnout Rate",
        icon = icon("vote-yea"),
        color = "green"
      )
    }, error = function(e) {
      cat("ERROR in jurisdiction_turnout:", e$message, "\n")
      valueBox("N/A", "Turnout Rate", icon = icon("exclamation"), color = "red")
    })
  })

  output$jurisdiction_mail_rate <- renderValueBox({
    cat("=== RENDERING JURISDICTION MAIL RATE ===\n")
    tryCatch({
      juris <- selected_jurisdiction()
      req(juris)

      # Calculate mail return rate
      mail_rate <- if (!is.na(juris$mail_return_rate)) {
        paste0(round(juris$mail_return_rate, 1), "%")
      } else if (!is.na(juris$c1b) && !is.na(juris$c1a) && juris$c1a > 0) {
        paste0(round((juris$c1b / juris$c1a) * 100, 1), "%")
      } else {
        "N/A"
      }

      valueBox(
        value = mail_rate,
        subtitle = "Mail Return Rate",
        icon = icon("envelope"),
        color = "yellow"
      )
    }, error = function(e) {
      cat("ERROR in jurisdiction_mail_rate:", e$message, "\n")
      valueBox("N/A", "Mail Return Rate", icon = icon("exclamation"), color = "red")
    })
  })

  output$jurisdiction_size_category <- renderValueBox({
    cat("=== RENDERING JURISDICTION SIZE CATEGORY ===\n")
    tryCatch({
      juris <- selected_jurisdiction()
      req(juris)

      size_category <- if (!is.na(juris$jurisdiction_size)) {
        juris$jurisdiction_size
      } else {
        "Unknown"
      }

      valueBox(
        value = size_category,
        subtitle = "Jurisdiction Size",
        icon = icon("chart-bar"),
        color = "purple"
      )
    }, error = function(e) {
      cat("ERROR in jurisdiction_size_category:", e$message, "\n")
      valueBox("Unknown", "Jurisdiction Size", icon = icon("exclamation"), color = "red")
    })
  })

  # Registration details table
  output$jurisdiction_registration_table <- renderTable({
    cat("=== RENDERING JURISDICTION REGISTRATION TABLE ===\n")
    tryCatch({
      juris <- selected_jurisdiction()
      req(juris)

      data.frame(
        Metric = c("Total Registered", "Active Voters", "Inactive Voters",
                   "Online Registration", "Mail Registration", "DMV Registration"),
        Value = c(
          format(ifelse(is.na(juris$a1a), 0, juris$a1a), big.mark = ","),
          format(ifelse(is.na(juris$a1b), 0, juris$a1b), big.mark = ","),
          format(ifelse(is.na(juris$a1c), 0, juris$a1c), big.mark = ","),
          format(ifelse(is.na(juris$a4c), 0, juris$a4c), big.mark = ","),
          format(ifelse(is.na(juris$a4a), 0, juris$a4a), big.mark = ","),
          format(ifelse(is.na(juris$a4e), 0, juris$a4e), big.mark = ",")
        ),
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      cat("ERROR in jurisdiction_registration_table:", e$message, "\n")
      data.frame(Metric = "Error", Value = "Unable to load data")
    })
  }, striped = TRUE, hover = TRUE, width = "100%")

  # Voting methods breakdown chart
  output$jurisdiction_voting_methods <- renderPlotly({
    cat("=== RENDERING JURISDICTION VOTING METHODS ===\n")
    tryCatch({
      juris <- selected_jurisdiction()
      req(juris)

      # Create voting methods data
      methods_data <- data.frame(
        Method = c("In-Person on Election Day", "Mail/Absentee", "Early In-Person", "Provisional"),
        Count = c(
          ifelse(is.na(juris$f1b), 0, juris$f1b),
          ifelse(is.na(juris$f1c), 0, juris$f1c),
          ifelse(is.na(juris$f1d), 0, juris$f1d),
          ifelse(is.na(juris$e1a), 0, juris$e1a)
        )
      ) %>%
        filter(Count > 0)

      if (nrow(methods_data) == 0) {
        return(plotly_empty())
      }

      p <- ggplot(methods_data, aes(x = reorder(Method, Count), y = Count, fill = Method)) +
        geom_col() +
        coord_flip() +
        labs(title = "Voting Methods Breakdown", x = "", y = "Number of Votes") +
        theme_minimal() +
        theme(legend.position = "none") +
        scale_y_continuous(labels = scales::comma)

      ggplotly(p)
    }, error = function(e) {
      cat("ERROR in jurisdiction_voting_methods:", e$message, "\n")
      plotly_empty()
    })
  })

  # Mail voting performance table
  output$jurisdiction_mail_table <- renderTable({
    cat("=== RENDERING JURISDICTION MAIL TABLE ===\n")
    tryCatch({
      juris <- selected_jurisdiction()
      req(juris)

      mail_sent <- ifelse(is.na(juris$c1a), 0, juris$c1a)
      mail_returned <- ifelse(is.na(juris$c1b), 0, juris$c1b)
      mail_counted <- ifelse(is.na(juris$c8a), 0, juris$c8a)

      return_rate <- if (mail_sent > 0) paste0(round((mail_returned/mail_sent)*100, 1), "%") else "N/A"
      acceptance_rate <- if (mail_returned > 0) paste0(round((mail_counted/mail_returned)*100, 1), "%") else "N/A"

      data.frame(
        Metric = c("Ballots Sent", "Ballots Returned", "Ballots Counted",
                   "Return Rate", "Acceptance Rate"),
        Value = c(
          format(mail_sent, big.mark = ","),
          format(mail_returned, big.mark = ","),
          format(mail_counted, big.mark = ","),
          return_rate,
          acceptance_rate
        ),
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      cat("ERROR in jurisdiction_mail_table:", e$message, "\n")
      data.frame(Metric = "Error", Value = "Unable to load data")
    })
  }, striped = TRUE, hover = TRUE, width = "100%")

  # Provisional ballots table
  output$jurisdiction_provisional_table <- renderTable({
    cat("=== RENDERING JURISDICTION PROVISIONAL TABLE ===\n")
    tryCatch({
      juris <- selected_jurisdiction()
      req(juris)

      prov_cast <- ifelse(is.na(juris$e1a), 0, juris$e1a)
      prov_counted <- ifelse(is.na(juris$e1b), 0, juris$e1b)
      prov_rejected <- ifelse(is.na(juris$e1c), 0, juris$e1c)

      acceptance_rate <- if (prov_cast > 0) paste0(round((prov_counted/prov_cast)*100, 1), "%") else "N/A"

      data.frame(
        Metric = c("Provisional Cast", "Counted", "Rejected", "Acceptance Rate"),
        Value = c(
          format(prov_cast, big.mark = ","),
          format(prov_counted, big.mark = ","),
          format(prov_rejected, big.mark = ","),
          acceptance_rate
        ),
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      cat("ERROR in jurisdiction_provisional_table:", e$message, "\n")
      data.frame(Metric = "Error", Value = "Unable to load data")
    })
  }, striped = TRUE, hover = TRUE, width = "100%")

  # Complete jurisdiction data table
  output$jurisdiction_complete_data <- DT::renderDataTable({
    cat("=== RENDERING COMPLETE JURISDICTION DATA ===\n")
    tryCatch({
      juris <- selected_jurisdiction()
      req(juris)

      # Transpose the data for better viewing
      juris_t <- data.frame(
        Variable = names(juris),
        Value = as.character(t(juris)[,1]),
        stringsAsFactors = FALSE
      )

      return(juris_t)
    }, error = function(e) {
      cat("ERROR in jurisdiction_complete_data:", e$message, "\n")
      data.frame(Variable = "Error", Value = "Unable to load data")
    })
  }, options = list(pageLength = 25, scrollX = TRUE, scrollY = "300px"))

  cat("=== JURISDICTION PROFILE TAB COMPLETE ===\n")

  # ============================================================================
  # End of Jurisdiction Profile Implementation
  # ============================================================================

  # Placeholder plots with basic functionality
  output$registration_methods_plot <- renderPlotly({
    tryCatch({
      if (is.null(data()) || is.null(data()$registration)) return(plotly_empty())
      
      # Simple registration methods plot
      reg_data <- data()$registration %>%
        filter(!is.na(a4a), !is.na(a4c), !is.na(a4e)) %>%
        summarise(
          Mail = sum(a4a, na.rm = TRUE),
          Online = sum(a4c, na.rm = TRUE),
          DMV = sum(a4e, na.rm = TRUE)
        ) %>%
        pivot_longer(everything(), names_to = "Method", values_to = "Count")
      
      p <- ggplot(reg_data, aes(x = Method, y = Count, fill = Method)) +
        geom_col() +
        labs(title = "Registration Methods", y = "Total Registrations") +
        theme_minimal() +
        theme(legend.position = "none") +
        scale_y_continuous(labels = scales::comma)
      
      ggplotly(p)
    }, error = function(e) {
      plotly_empty()
    })
  })
  
  output$turnout_distribution <- renderPlotly({
    tryCatch({
      if (is.null(data()) || is.null(data()$overview)) return(plotly_empty())
      
      turnout_data <- data()$overview %>%
        filter(!is.na(turnout_rate), turnout_rate > 0, turnout_rate <= 100)
      
      p <- ggplot(turnout_data, aes(x = turnout_rate)) +
        geom_histogram(binwidth = 5, fill = "steelblue", alpha = 0.7) +
        labs(title = "Distribution of Turnout Rates",
             x = "Turnout Rate (%)", y = "Number of Jurisdictions") +
        theme_minimal()
      
      ggplotly(p)
    }, error = function(e) {
      plotly_empty()
    })
  })
  
  output$voting_methods_plot <- renderPlotly({
    tryCatch({
      if (is.null(data()) || is.null(data()$overview)) return(plotly_empty())
      
      # Aggregate voting methods
      voting_data <- data()$overview %>%
        filter(!is.na(polling_place_pct), !is.na(mail_vote_pct)) %>%
        summarise(
          `Polling Place` = mean(polling_place_pct, na.rm = TRUE),
          `Mail Voting` = mean(mail_vote_pct, na.rm = TRUE),
          `Early Voting` = mean(early_vote_pct, na.rm = TRUE),
          `Provisional` = mean(provisional_pct, na.rm = TRUE)
        ) %>%
        pivot_longer(everything(), names_to = "Method", values_to = "Percentage")
      
      p <- ggplot(voting_data, aes(x = Method, y = Percentage, fill = Method)) +
        geom_col() +
        labs(title = "Average Voting Method Usage",
             y = "Average Percentage") +
        theme_minimal() +
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p)
    }, error = function(e) {
      plotly_empty()
    })
  })
  
  # Set remaining outputs to empty for now
  output$active_inactive_plot <- renderPlotly({ plotly_empty() })
  output$turnout_scatter <- renderPlotly({ plotly_empty() })
  output$mail_return_by_state <- renderPlotly({ plotly_empty() })
  output$mail_stats_table <- DT::renderDataTable({ data.frame() })
  output$provisional_outcomes <- renderPlotly({ plotly_empty() })
  output$provisional_by_state <- renderPlotly({ plotly_empty() })
  output$poll_worker_ratio <- renderPlotly({ plotly_empty() })
  output$polling_places_plot <- renderPlotly({ plotly_empty() })
  output$uocava_return_rates <- renderPlotly({ plotly_empty() })
  output$uocava_by_state <- renderPlotly({ plotly_empty() })
  
  cat("=== ALL BASIC OUTPUTS CREATED ===\n")
  
  # Comment out everything else for now
  # We'll add them back one by one
  
  cat("=== SERVER SETUP COMPLETE ===\n")
}

# Helper function for empty plots
plotly_empty <- function() {
  plotly::plot_ly() %>%
    plotly::add_annotations(
      text = "No data available",
      x = 0.5, y = 0.5,
      xref = "paper", yref = "paper",
      showarrow = FALSE
    )
}

# Run the application
shinyApp(ui = ui, server = server)
