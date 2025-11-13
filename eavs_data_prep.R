# EAVS Data Cleaning and Dashboard Preparation Script
# Election Administration and Voting Survey (EAC) Data Processing

# Load required libraries
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)

# Try to load janitor, if not available, create a simple alternative
if (!require(janitor, quietly = TRUE)) {
  cat("janitor package not found. Installing...\n")
  install.packages("janitor")
  library(janitor)
}

# Alternative clean_names function if janitor fails
clean_names_alt <- function(df) {
  names(df) <- names(df) %>%
    tolower() %>%                           # Convert to lowercase
    gsub("[^a-z0-9_]", "_", .) %>%         # Replace non-alphanumeric with underscore
    gsub("_{2,}", "_", .) %>%              # Replace multiple underscores with single
    gsub("^_|_$", "", .) %>%               # Remove leading/trailing underscores
    make.names(., unique = TRUE)            # Ensure valid R names
  return(df)
}

# Function to clean and prepare EAVS data
clean_eavs_data <- function(file_path) {
  
  # Read the raw EAVS data
  cat("Reading EAVS data...\n")
  eavs_raw <- read_csv(file_path, 
                       col_types = cols(.default = "c"),
                       na = c("", "NA", "N/A", "-", "NULL"))
  
  # Clean column names and handle potential issues
  cat("Cleaning column names...\n")
  
  # First, let's see what we're working with
  cat("Original column names (first 10):\n")
  print(head(names(eavs_raw), 10))
  
  # Clean column names - try janitor first, then fallback
  tryCatch({
    eavs_clean <- eavs_raw %>% clean_names()
  }, error = function(e) {
    cat("Using alternative column cleaning method...\n")
    eavs_clean <- clean_names_alt(eavs_raw)
  })
  
  # Check for and fix any remaining problematic column names
  col_names <- names(eavs_clean)
  
  # Remove any empty or problematic column names
  empty_cols <- which(col_names == "" | is.na(col_names) | nchar(col_names) == 0)
  if (length(empty_cols) > 0) {
    cat("Found", length(empty_cols), "empty column names. Removing...\n")
    eavs_clean <- eavs_clean[, -empty_cols]
  }
  
  # Fix any remaining column name issues
  names(eavs_clean) <- make.names(names(eavs_clean), unique = TRUE)
  
  cat("Final column names (first 10):\n")
  print(head(names(eavs_clean), 10))
  
  # Define variable groups for easier analysis (using exact column names from your data)
  geographic_vars <- c("fipscode", "jurisdiction_name", "state_full", "state_abbr")
  
  registration_vars <- c("a1a", "a1b", "a1c", "a3a", "a4a", "a4b", "a4c", 
                         "a4e", "a4f", "a12a", "a12b", "a12c", "a12k")
  
  uocava_vars <- c("b1a", "b1b", "b1c", "b5a", "b5b", "b5c", 
                   "b11a", "b11b", "b24a", "b24b", "b24c")
  
  mail_voting_vars <- c("c1a", "c1b", "c1c", "c1f", "c8a", "c9a")
  
  polling_vars <- c("d1a", "d3a", "d4a", "d5a", "d6a", "d8")
  
  provisional_vars <- c("e1a", "e1b", "e1c", "e1d")
  
  turnout_vars <- c("f1a", "f1b", "f1c", "f1d", "f1e", "f1f", "f1g")
  
  equipment_vars <- c("f3a", "f4d_1", "f8a")
  
  # Check which variables actually exist in the data
  all_expected_vars <- c(geographic_vars, registration_vars, uocava_vars, 
                         mail_voting_vars, polling_vars, provisional_vars, turnout_vars)
  
  existing_vars <- intersect(all_expected_vars, names(eavs_clean))
  missing_vars <- setdiff(all_expected_vars, names(eavs_clean))
  
  cat("Variables found in data:", length(existing_vars), "\n")
  cat("Variables missing from data:", length(missing_vars), "\n")
  if (length(missing_vars) > 0) {
    cat("Missing variables:", paste(missing_vars, collapse = ", "), "\n")
  }
  
  # Update variable lists to only include existing variables
  geographic_vars <- intersect(geographic_vars, names(eavs_clean))
  registration_vars <- intersect(registration_vars, names(eavs_clean))
  uocava_vars <- intersect(uocava_vars, names(eavs_clean))
  mail_voting_vars <- intersect(mail_voting_vars, names(eavs_clean))
  polling_vars <- intersect(polling_vars, names(eavs_clean))
  provisional_vars <- intersect(provisional_vars, names(eavs_clean))
  turnout_vars <- intersect(turnout_vars, names(eavs_clean))
  
  # Convert numeric variables to proper numeric format
  numeric_vars <- c(registration_vars, uocava_vars, mail_voting_vars, 
                    polling_vars, provisional_vars, turnout_vars)
  
  # Only convert variables that actually exist
  numeric_vars <- intersect(numeric_vars, names(eavs_clean))
  
  cat("Converting", length(numeric_vars), "numeric variables...\n")
  
  # Convert numeric columns with error handling
  if (length(numeric_vars) > 0) {
    eavs_clean <- eavs_clean %>%
      mutate(across(all_of(numeric_vars), ~ {
        # Remove commas and convert to numeric
        numeric_val <- as.numeric(gsub("[^0-9.-]", "", as.character(.x)))
        # Convert common missing value codes to NA
        ifelse(numeric_val %in% c(-99, -88, -77, 999999, 9999999), NA, numeric_val)
      }))
  }
  
  cat("Cleaning missing value codes...\n")
  
  # Additional cleaning for obvious data entry errors
  eavs_clean <- eavs_clean %>%
    mutate(
      # Fix negative values that should be positive
      across(all_of(intersect(numeric_vars, names(.))), ~ ifelse(.x < 0, NA, .x)),
      
      # Clean up impossible values (like negative voter counts)
      a1a = ifelse(a1a <= 0, NA, a1a),
      a1b = ifelse(a1b <= 0, NA, a1b),
      f1a = ifelse(f1a <= 0, NA, f1a)
    )
  
  # Clean geographic variables with error handling
  cat("Cleaning geographic variables...\n")
  
  # Handle state abbreviation
  if ("state_abbr" %in% names(eavs_clean)) {
    eavs_clean <- eavs_clean %>%
      mutate(state_abbr = toupper(str_trim(as.character(state_abbr))))
  }
  
  # Handle state full name
  if ("state_full" %in% names(eavs_clean)) {
    eavs_clean <- eavs_clean %>%
      mutate(state_full = str_to_title(str_trim(as.character(state_full))))
  }
  
  # Handle jurisdiction name
  if ("jurisdiction_name" %in% names(eavs_clean)) {
    eavs_clean <- eavs_clean %>%
      mutate(jurisdiction_name = str_trim(as.character(jurisdiction_name)))
  }
  
  # Handle FIPS code (could be fips_code or fipscode)
  fips_col <- NULL
  if ("fips_code" %in% names(eavs_clean)) {
    fips_col <- "fips_code"
  } else if ("fipscode" %in% names(eavs_clean)) {
    fips_col <- "fipscode"
    # Rename to standardize
    eavs_clean <- eavs_clean %>%
      rename(fips_code = fipscode)
  }
  
  cat("Geographic variables processed successfully.\n")
  
  # Create derived variables for dashboard
  cat("Creating derived variables...\n")
  
  eavs_clean <- eavs_clean %>%
    mutate(
      # Registration rates (only calculate if base value > 0)
      inactive_rate = ifelse(!is.na(a1a) & !is.na(a1c) & a1a > 0, 
                             round((a1c / a1a) * 100, 2), NA),
      active_rate = ifelse(!is.na(a1a) & !is.na(a1b) & a1a > 0, 
                           round((a1b / a1a) * 100, 2), NA),
      
      # Mail voting rates
      mail_return_rate = ifelse(!is.na(c1a) & !is.na(c1b) & c1a > 0, 
                                round((c1b / c1a) * 100, 2), NA),
      mail_acceptance_rate = ifelse(!is.na(c1b) & !is.na(c8a) & c1b > 0, 
                                    round((c8a / c1b) * 100, 2), NA),
      mail_rejection_rate = ifelse(!is.na(c1b) & !is.na(c9a) & c1b > 0, 
                                   round((c9a / c1b) * 100, 2), NA),
      
      # Provisional ballot rates
      provisional_acceptance_rate = ifelse(!is.na(e1a) & !is.na(e1b) & !is.na(e1c) & e1a > 0, 
                                           round(((e1b + e1c) / e1a) * 100, 2), NA),
      provisional_rejection_rate = ifelse(!is.na(e1a) & !is.na(e1d) & e1a > 0, 
                                          round((e1d / e1a) * 100, 2), NA),
      
      # Turnout calculations (with reasonable limits)
      turnout_rate = ifelse(!is.na(f1a) & !is.na(a1b) & a1b > 0 & f1a > 0, 
                            pmin(round((f1a / a1b) * 100, 2), 150), NA),  # Cap at 150%
      
      # Voting method percentages
      polling_place_pct = ifelse(!is.na(f1a) & !is.na(f1b) & f1a > 0, 
                                 round((f1b / f1a) * 100, 2), NA),
      mail_vote_pct = ifelse(!is.na(f1a) & !is.na(f1d) & f1a > 0, 
                             round((f1d / f1a) * 100, 2), NA),
      early_vote_pct = ifelse(!is.na(f1a) & !is.na(f1f) & f1a > 0, 
                              round((f1f / f1a) * 100, 2), NA),
      provisional_pct = ifelse(!is.na(f1a) & !is.na(f1e) & f1a > 0, 
                               round((f1e / f1a) * 100, 2), NA),
      
      # Registration method percentages
      mail_reg_pct = ifelse(!is.na(a3a) & !is.na(a4a) & a3a > 0, 
                            round((a4a / a3a) * 100, 2), NA),
      online_reg_pct = ifelse(!is.na(a3a) & !is.na(a4c) & a3a > 0, 
                              round((a4c / a3a) * 100, 2), NA),
      dmv_reg_pct = ifelse(!is.na(a3a) & !is.na(a4e) & a3a > 0, 
                           round((a4e / a3a) * 100, 2), NA),
      
      # UOCAVA rates
      uocava_return_rate = ifelse(!is.na(b5a) & !is.na(b11a) & b5a > 0, 
                                  round((b11a / b5a) * 100, 2), NA),
      uocava_rejection_rate = ifelse(!is.na(b11a) & !is.na(b24a) & b11a > 0, 
                                     round((b24a / b11a) * 100, 2), NA),
      
      # Poll worker ratios
      poll_worker_ratio = ifelse(!is.na(f1a) & !is.na(d5a) & d5a > 0, 
                                 round(f1a / d5a, 0), NA),
      
      # Jurisdiction size categories (based on total turnout)
      jurisdiction_size = case_when(
        is.na(f1a) ~ "Unknown",
        f1a >= 100000 ~ "Large (100K+)",
        f1a >= 25000 ~ "Medium (25K-99K)",
        f1a >= 5000 ~ "Small (5K-24K)",
        f1a > 0 ~ "Very Small (<5K)",
        TRUE ~ "Unknown"
      ),
      
      # State region (simplified US Census regions)
      region = case_when(
        state_abbr %in% c("CT", "ME", "MA", "NH", "RI", "VT", "NJ", "NY", "PA") ~ "Northeast",
        state_abbr %in% c("IL", "IN", "MI", "OH", "WI", "IA", "KS", "MN", "MO", "NE", "ND", "SD") ~ "Midwest",
        state_abbr %in% c("DE", "FL", "GA", "MD", "NC", "SC", "VA", "WV", "AL", "KY", "MS", "TN", "AR", "LA", "OK", "TX") ~ "South",
        state_abbr %in% c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY", "AK", "CA", "HI", "OR", "WA") ~ "West",
        TRUE ~ "Other"
      )
    )
  
  # Remove extreme outliers in derived variables
  eavs_clean <- eavs_clean %>%
    mutate(
      turnout_rate = ifelse(turnout_rate > 150 | turnout_rate < 0, NA, turnout_rate),
      mail_return_rate = ifelse(mail_return_rate > 100 | mail_return_rate < 0, NA, mail_return_rate),
      active_rate = ifelse(active_rate > 100 | active_rate < 0, NA, active_rate),
      inactive_rate = ifelse(inactive_rate > 100 | inactive_rate < 0, NA, inactive_rate)
    )
  
  cat("Derived variables created successfully.\n")
  
  # Create summary statistics by state
  state_summary <- eavs_clean %>%
    group_by(state_abbr, state_full, region) %>%
    summarise(
      jurisdictions = n(),
      total_registered = sum(a1a, na.rm = TRUE),
      total_active = sum(a1b, na.rm = TRUE),
      total_turnout = sum(f1a, na.rm = TRUE),
      total_mail_sent = sum(c1a, na.rm = TRUE),
      total_mail_returned = sum(c1b, na.rm = TRUE),
      total_provisional = sum(e1a, na.rm = TRUE),
      avg_turnout_rate = mean(turnout_rate, na.rm = TRUE),
      avg_mail_return_rate = mean(mail_return_rate, na.rm = TRUE),
      avg_provisional_rate = mean(provisional_acceptance_rate, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      state_turnout_rate = round((total_turnout / total_active) * 100, 2),
      state_mail_return_rate = round((total_mail_returned / total_mail_sent) * 100, 2)
    )
  
  # Create national summary
  national_summary <- eavs_clean %>%
    summarise(
      total_jurisdictions = n(),
      total_registered = sum(a1a, na.rm = TRUE),
      total_active = sum(a1b, na.rm = TRUE),
      total_turnout = sum(f1a, na.rm = TRUE),
      national_turnout_rate = round((sum(f1a, na.rm = TRUE) / sum(a1b, na.rm = TRUE)) * 100, 2),
      total_mail_sent = sum(c1a, na.rm = TRUE),
      total_mail_returned = sum(c1b, na.rm = TRUE),
      national_mail_return_rate = round((sum(c1b, na.rm = TRUE) / sum(c1a, na.rm = TRUE)) * 100, 2),
      .groups = "drop"
    )
  
  # Return list of cleaned datasets
  return(list(
    jurisdiction_data = eavs_clean,
    state_summary = state_summary,
    national_summary = national_summary,
    variable_groups = list(
      geographic = geographic_vars,
      registration = registration_vars,
      uocava = uocava_vars,
      mail_voting = mail_voting_vars,
      polling = polling_vars,
      provisional = provisional_vars,
      turnout = turnout_vars,
      equipment = equipment_vars
    )
  ))
}

# Function to create dashboard-ready subsets
create_dashboard_subsets <- function(eavs_data) {
  
  jurisdiction_data <- eavs_data$jurisdiction_data
  
  # Key metrics for overview dashboard
  overview_data <- jurisdiction_data %>%
    select(
      fips_code, jurisdiction_name, state_abbr, state_full, region, jurisdiction_size,
      # Registration metrics
      a1a, a1b, active_rate, inactive_rate,
      # Turnout metrics
      f1a, turnout_rate,
      # Voting method percentages
      polling_place_pct, mail_vote_pct, early_vote_pct, provisional_pct,
      # Key rates
      mail_return_rate, mail_acceptance_rate, provisional_acceptance_rate
    ) %>%
    filter(!is.na(state_abbr))
  
  # Registration analysis data
  registration_data <- jurisdiction_data %>%
    select(
      fips_code, jurisdiction_name, state_abbr, state_full, region,
      a1a, a1b, a1c, a3a, a4a, a4b, a4c, a4e, a4f,
      mail_reg_pct, online_reg_pct, dmv_reg_pct,
      a12a, a12b, a12c, a12k
    ) %>%
    filter(!is.na(a1a), a1a > 0)
  
  # Mail voting analysis data
  mail_voting_data <- jurisdiction_data %>%
    select(
      fips_code, jurisdiction_name, state_abbr, state_full, region,
      c1a, c1b, c1c, c1f, c8a, c9a,
      mail_return_rate, mail_acceptance_rate, mail_rejection_rate,
      f1d, mail_vote_pct
    ) %>%
    filter(!is.na(c1a), c1a > 0)
  
  # Provisional ballot analysis data
  provisional_data <- jurisdiction_data %>%
    select(
      fips_code, jurisdiction_name, state_abbr, state_full, region,
      e1a, e1b, e1c, e1d,
      provisional_acceptance_rate, provisional_rejection_rate,
      f1e, provisional_pct
    ) %>%
    filter(!is.na(e1a), e1a > 0)
  
  # Polling operations data
  polling_data <- jurisdiction_data %>%
    select(
      fips_code, jurisdiction_name, state_abbr, state_full, region,
      d1a, d3a, d4a, d5a, d6a, d8,
      f1a, poll_worker_ratio
    ) %>%
    filter(!is.na(d1a), d1a > 0)
  
  # UOCAVA data
  uocava_data <- jurisdiction_data %>%
    select(
      fips_code, jurisdiction_name, state_abbr, state_full, region,
      b1a, b1b, b1c, b5a, b5b, b5c, b11a, b11b,
      b24a, b24b, b24c,
      uocava_return_rate, uocava_rejection_rate
    ) %>%
    filter(!is.na(b1a), b1a > 0)
  
  return(list(
    overview = overview_data,
    registration = registration_data,
    mail_voting = mail_voting_data,
    provisional = provisional_data,
    polling = polling_data,
    uocava = uocava_data
  ))
}

# Main execution function
prepare_eavs_dashboard_data <- function(input_file, output_dir = "dashboard_data") {
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  cat("Starting EAVS data preparation...\n")
  
  # Clean the data
  cleaned_data <- clean_eavs_data(input_file)
  
  # Create dashboard subsets
  dashboard_subsets <- create_dashboard_subsets(cleaned_data)
  
  # Save all datasets
  cat("Saving cleaned datasets...\n")
  
  # Save main datasets
  write_csv(cleaned_data$jurisdiction_data, file.path(output_dir, "jurisdiction_data.csv"))
  write_csv(cleaned_data$state_summary, file.path(output_dir, "state_summary.csv"))
  write_csv(cleaned_data$national_summary, file.path(output_dir, "national_summary.csv"))
  
  # Save dashboard subsets
  write_csv(dashboard_subsets$overview, file.path(output_dir, "overview_data.csv"))
  write_csv(dashboard_subsets$registration, file.path(output_dir, "registration_data.csv"))
  write_csv(dashboard_subsets$mail_voting, file.path(output_dir, "mail_voting_data.csv"))
  write_csv(dashboard_subsets$provisional, file.path(output_dir, "provisional_data.csv"))
  write_csv(dashboard_subsets$polling, file.path(output_dir, "polling_data.csv"))
  write_csv(dashboard_subsets$uocava, file.path(output_dir, "uocava_data.csv"))
  
  # Save variable groups as JSON for dashboard reference
  jsonlite::write_json(cleaned_data$variable_groups, 
                       file.path(output_dir, "variable_groups.json"), 
                       pretty = TRUE)
  
  cat("Data preparation complete!\n")
  cat("Files saved to:", output_dir, "\n")
  
  # Return data for immediate use
  return(list(
    cleaned = cleaned_data,
    subsets = dashboard_subsets
  ))
}

# Example usage:
# eavs_data <- prepare_eavs_dashboard_data("path/to/your/eavs_data.csv")

# Quick data validation function
validate_eavs_data <- function(data) {
  
  jurisdiction_data <- data$cleaned$jurisdiction_data
  
  cat("=== EAVS Data Validation Report ===\n")
  cat("Total jurisdictions:", nrow(jurisdiction_data), "\n")
  cat("States represented:", length(unique(jurisdiction_data$state_abbr)), "\n")
  cat("Date range: 2024 Election Data\n\n")
  
  # Check for missing key variables
  key_vars <- c("a1a", "a1b", "f1a", "state_abbr")
  missing_counts <- jurisdiction_data %>%
    summarise(across(all_of(key_vars), ~ sum(is.na(.)))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "missing_count")
  
  cat("Missing values in key variables:\n")
  print(missing_counts)
  
  # Summary statistics
  cat("\nSummary statistics for key metrics:\n")
  summary_stats <- jurisdiction_data %>%
    select(a1a, a1b, f1a, turnout_rate, mail_return_rate) %>%
    summary()
  
  print(summary_stats)
  
  # Top 10 jurisdictions by turnout
  cat("\nTop 10 jurisdictions by total turnout:\n")
  top_turnout <- jurisdiction_data %>%
    arrange(desc(f1a)) %>%
    select(jurisdiction_name, state_abbr, f1a, turnout_rate) %>%
    head(10)
  
  print(top_turnout)
  
  return(invisible(TRUE))
}

# Data quality check function
check_data_quality <- function(data) {
  
  jurisdiction_data <- data$cleaned$jurisdiction_data
  
  # Check for logical inconsistencies
  issues <- jurisdiction_data %>%
    mutate(
      issue_active_gt_total = a1b > a1a,
      issue_inactive_gt_total = a1c > a1a,
      issue_turnout_gt_active = f1a > a1b,
      issue_negative_rates = turnout_rate < 0 | turnout_rate > 200,
      issue_mail_return_gt_sent = c1b > c1a
    ) %>%
    summarise(
      active_gt_total = sum(issue_active_gt_total, na.rm = TRUE),
      inactive_gt_total = sum(issue_inactive_gt_total, na.rm = TRUE),
      turnout_gt_active = sum(issue_turnout_gt_active, na.rm = TRUE),
      negative_rates = sum(issue_negative_rates, na.rm = TRUE),
      mail_return_gt_sent = sum(issue_mail_return_gt_sent, na.rm = TRUE)
    )
  
  cat("=== Data Quality Issues ===\n")
  print(issues)
  
  return(issues)
}