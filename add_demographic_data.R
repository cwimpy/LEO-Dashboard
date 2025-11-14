# Add Demographic and Context Data to LEO Dashboard
# This script enhances jurisdiction data with population, RUCC, and other useful metrics

library(dplyr)
library(readr)

# ============================================================================
# STEP 1: Load existing jurisdiction data
# ============================================================================

cat("Loading existing jurisdiction data...\n")
jurisdiction_data <- read_csv("dashboard_data/jurisdiction_data.csv", show_col_types = FALSE)

cat("Current jurisdiction data has", nrow(jurisdiction_data), "rows and", ncol(jurisdiction_data), "columns\n")

# ============================================================================
# STEP 2: Prepare demographic data sources
# ============================================================================

# You'll need to obtain and add these datasets:

# 1. POPULATION DATA
# Source: US Census Bureau County Population Estimates
# URL: https://www.census.gov/data/datasets/time-series/demo/popest/2020s-counties-total.html
# Download the CSV and create a mapping like this:

# Example structure for population data (you'll need to download and format):
# population_data <- read_csv("your_census_population_file.csv") %>%
#   mutate(fips_code = paste0(STATE, COUNTY)) %>%
#   select(fips_code, population = POPESTIMATE2023) # or latest year

# For demonstration, I'll show you how to create a sample dataset:
# You would replace this with real Census data

cat("\n=== STEP 2A: Add Population Data ===\n")
cat("TO DO: Download county population data from Census Bureau\n")
cat("URL: https://www.census.gov/data/datasets/time-series/demo/popest/2020s-counties-total.html\n")
cat("\nExample code to merge population:\n")
cat('
# After downloading and preparing population data:
population_data <- read_csv("census_population.csv") %>%
  mutate(
    fips_code = sprintf("%05d", as.numeric(fips_code)),  # Ensure 5-digit FIPS
    population = as.numeric(population)
  ) %>%
  select(fips_code, population)

# Merge with jurisdiction data
jurisdiction_data <- jurisdiction_data %>%
  left_join(population_data, by = "fips_code")
')

# ============================================================================
# 2. RURAL-URBAN CONTINUUM CODES (RUCC)
# Source: USDA Economic Research Service
# URL: https://www.ers.usda.gov/data-products/rural-urban-continuum-codes/
# ============================================================================

cat("\n=== STEP 2B: Add RUCC (Rural-Urban Continuum Codes) ===\n")
cat("TO DO: Download RUCC data from USDA\n")
cat("URL: https://www.ers.usda.gov/data-products/rural-urban-continuum-codes/\n")
cat("\nRUCC Codes Explained:\n")
cat("  1 = Counties in metro areas of 1 million+ population\n")
cat("  2 = Counties in metro areas of 250,000 to 1 million population\n")
cat("  3 = Counties in metro areas of fewer than 250,000 population\n")
cat("  4 = Urban population of 20,000+, adjacent to a metro area\n")
cat("  5 = Urban population of 20,000+, not adjacent to a metro area\n")
cat("  6 = Urban population of 2,500-19,999, adjacent to a metro area\n")
cat("  7 = Urban population of 2,500-19,999, not adjacent to a metro area\n")
cat("  8 = Completely rural or less than 2,500 urban population, adjacent to a metro area\n")
cat("  9 = Completely rural or less than 2,500 urban population, not adjacent to a metro area\n")

cat("\nExample code to merge RUCC:\n")
cat('
# After downloading RUCC data:
rucc_data <- read_csv("rucc_2023.csv") %>%
  mutate(
    fips_code = sprintf("%05d", FIPS),
    rucc_code = as.integer(RUCC_2023),
    rucc_description = case_when(
      rucc_code == 1 ~ "Metro: 1M+ population",
      rucc_code == 2 ~ "Metro: 250K-1M population",
      rucc_code == 3 ~ "Metro: <250K population",
      rucc_code == 4 ~ "Urban 20K+, adjacent to metro",
      rucc_code == 5 ~ "Urban 20K+, not adjacent",
      rucc_code == 6 ~ "Urban 2.5K-20K, adjacent to metro",
      rucc_code == 7 ~ "Urban 2.5K-20K, not adjacent",
      rucc_code == 8 ~ "Rural <2.5K, adjacent to metro",
      rucc_code == 9 ~ "Rural <2.5K, not adjacent",
      TRUE ~ "Unknown"
    ),
    rurality_category = case_when(
      rucc_code %in% 1:3 ~ "Metro",
      rucc_code %in% 4:7 ~ "Urban Non-Metro",
      rucc_code %in% 8:9 ~ "Rural",
      TRUE ~ "Unknown"
    )
  ) %>%
  select(fips_code, rucc_code, rucc_description, rurality_category)

# Merge with jurisdiction data
jurisdiction_data <- jurisdiction_data %>%
  left_join(rucc_data, by = "fips_code")
')

# ============================================================================
# 3. MEDIAN HOUSEHOLD INCOME
# Source: US Census Bureau - Small Area Income and Poverty Estimates (SAIPE)
# URL: https://www.census.gov/data/datasets/2022/demo/saipe/2022-state-and-county.html
# ============================================================================

cat("\n=== STEP 2C: Add Median Household Income ===\n")
cat("TO DO: Download SAIPE data from Census Bureau\n")
cat("URL: https://www.census.gov/data/datasets/2022/demo/saipe/2022-state-and-county.html\n")

cat("\nExample code to merge income data:\n")
cat('
# After downloading SAIPE data:
income_data <- read_csv("saipe_income.csv") %>%
  mutate(
    fips_code = sprintf("%05d", as.numeric(paste0(State_FIPS, County_FIPS))),
    median_household_income = as.numeric(Median_Household_Income),
    poverty_rate = as.numeric(Poverty_Percent_All_Ages)
  ) %>%
  select(fips_code, median_household_income, poverty_rate)

# Merge with jurisdiction data
jurisdiction_data <- jurisdiction_data %>%
  left_join(income_data, by = "fips_code")
')

# ============================================================================
# 4. EDUCATIONAL ATTAINMENT
# Source: US Census Bureau - American Community Survey (ACS)
# URL: https://data.census.gov/
# ============================================================================

cat("\n=== STEP 2D: Add Educational Attainment ===\n")
cat("TO DO: Download ACS 5-Year Estimates for Educational Attainment\n")
cat("URL: https://data.census.gov/\n")
cat("Table: S1501 - Educational Attainment\n")

cat("\nExample code to merge education data:\n")
cat('
# After downloading and preparing ACS education data:
education_data <- read_csv("acs_education.csv") %>%
  mutate(
    fips_code = sprintf("%05d", geoid),
    pct_bachelors_or_higher = as.numeric(bachelors_or_higher_pct),
    pct_high_school_or_higher = as.numeric(high_school_or_higher_pct)
  ) %>%
  select(fips_code, pct_bachelors_or_higher, pct_high_school_or_higher)

# Merge with jurisdiction data
jurisdiction_data <- jurisdiction_data %>%
  left_join(education_data, by = "fips_code")
')

# ============================================================================
# 5. AGE DEMOGRAPHICS
# Source: US Census Bureau - Population Estimates
# ============================================================================

cat("\n=== STEP 2E: Add Age Demographics ===\n")
cat("Useful age metrics:\n")
cat("  - Median age\n")
cat("  - % Population 65+\n")
cat("  - % Population 18-29\n")

cat("\nExample code to merge age data:\n")
cat('
age_data <- read_csv("census_age_data.csv") %>%
  mutate(
    fips_code = sprintf("%05d", FIPS),
    median_age = as.numeric(MEDIAN_AGE),
    pct_65_plus = as.numeric(PCT_65_PLUS),
    pct_18_29 = as.numeric(PCT_18_29)
  ) %>%
  select(fips_code, median_age, pct_65_plus, pct_18_29)

# Merge with jurisdiction data
jurisdiction_data <- jurisdiction_data %>%
  left_join(age_data, by = "fips_code")
')

# ============================================================================
# 6. RACE/ETHNICITY DEMOGRAPHICS
# Source: US Census Bureau - Decennial Census or ACS
# ============================================================================

cat("\n=== STEP 2F: Add Race/Ethnicity Demographics ===\n")

cat("\nExample code to merge demographic data:\n")
cat('
race_data <- read_csv("census_race_ethnicity.csv") %>%
  mutate(
    fips_code = sprintf("%05d", FIPS),
    pct_white = as.numeric(PCT_WHITE_ALONE),
    pct_black = as.numeric(PCT_BLACK_ALONE),
    pct_hispanic = as.numeric(PCT_HISPANIC),
    pct_asian = as.numeric(PCT_ASIAN_ALONE),
    pct_other = as.numeric(PCT_OTHER_RACES)
  ) %>%
  select(fips_code, pct_white, pct_black, pct_hispanic, pct_asian, pct_other)

# Merge with jurisdiction data
jurisdiction_data <- jurisdiction_data %>%
  left_join(race_data, by = "fips_code")
')

# ============================================================================
# 7. POLITICAL CONTEXT (Optional but useful for election officials)
# ============================================================================

cat("\n=== STEP 2G: Add Political Context (Optional) ===\n")
cat("Consider adding:\n")
cat("  - 2020 Presidential election results (margin, turnout)\n")
cat("  - Party registration (if available in state)\n")
cat("  - Competitive index\n")
cat("  - Historical turnout trends\n")

# ============================================================================
# STEP 3: Calculate derived metrics
# ============================================================================

cat("\n=== STEP 3: Calculate Derived Metrics ===\n")

cat("\nExample derived metrics you can calculate:\n")
cat('
# After merging demographic data, calculate useful metrics:
jurisdiction_data <- jurisdiction_data %>%
  mutate(
    # Registration rate (of total population)
    registration_rate = if_else(!is.na(population) & population > 0,
                                round((a1a / population) * 100, 1),
                                NA_real_),

    # Voters per capita (votes cast per 1000 residents)
    votes_per_1000 = if_else(!is.na(population) & population > 0,
                             round((f1a / population) * 1000, 1),
                             NA_real_),

    # Rurality simplified
    is_rural = if_else(rucc_code %in% 8:9, TRUE, FALSE),
    is_metro = if_else(rucc_code %in% 1:3, TRUE, FALSE),

    # Income category
    income_category = case_when(
      is.na(median_household_income) ~ "Unknown",
      median_household_income < 50000 ~ "Low Income",
      median_household_income < 75000 ~ "Middle Income",
      TRUE ~ "High Income"
    )
  )
')

# ============================================================================
# STEP 4: Save enhanced data
# ============================================================================

cat("\n=== STEP 4: Save Enhanced Data ===\n")
cat("After merging all data sources, save the enhanced dataset:\n")
cat('
# Save the enhanced jurisdiction data
write_csv(jurisdiction_data, "dashboard_data/jurisdiction_data_enhanced.csv")

# Update the dashboard to use the enhanced data
# In eavs_dashboard.R, update the load function to use jurisdiction_data_enhanced.csv
cat("Enhanced data saved successfully!\\n")
')

# ============================================================================
# QUICK START OPTION: Sample demographic data
# ============================================================================

cat("\n\n" , "=".repeat(70), "\n")
cat("QUICK START OPTION: Create sample demographic data for testing\n")
cat("=".repeat(70), "\n\n")

cat("If you want to test the integration before downloading real data,\n")
cat("here's code to create sample data:\n\n")

cat('
# Create sample demographic data for testing
set.seed(123)

sample_demographics <- jurisdiction_data %>%
  select(fips_code) %>%
  distinct() %>%
  mutate(
    # Sample population (scaled based on registered voters if available)
    population = sample(5000:5000000, n(), replace = TRUE),

    # Sample RUCC code (weighted toward urban)
    rucc_code = sample(1:9, n(), replace = TRUE,
                      prob = c(0.2, 0.15, 0.15, 0.1, 0.1, 0.1, 0.1, 0.05, 0.05)),

    # Derived RUCC fields
    rucc_description = case_when(
      rucc_code == 1 ~ "Metro: 1M+ population",
      rucc_code == 2 ~ "Metro: 250K-1M population",
      rucc_code == 3 ~ "Metro: <250K population",
      rucc_code == 4 ~ "Urban 20K+, adjacent to metro",
      rucc_code == 5 ~ "Urban 20K+, not adjacent",
      rucc_code == 6 ~ "Urban 2.5K-20K, adjacent to metro",
      rucc_code == 7 ~ "Urban 2.5K-20K, not adjacent",
      rucc_code == 8 ~ "Rural <2.5K, adjacent to metro",
      rucc_code == 9 ~ "Rural <2.5K, not adjacent"
    ),

    rurality_category = case_when(
      rucc_code %in% 1:3 ~ "Metro",
      rucc_code %in% 4:7 ~ "Urban Non-Metro",
      rucc_code %in% 8:9 ~ "Rural"
    ),

    # Sample income
    median_household_income = round(rnorm(n(), 65000, 20000)),

    # Sample education
    pct_bachelors_or_higher = round(runif(n(), 15, 60), 1),

    # Sample age
    median_age = round(rnorm(n(), 38, 8), 1),
    pct_65_plus = round(runif(n(), 10, 25), 1)
  )

# Merge sample data
jurisdiction_data_enhanced <- jurisdiction_data %>%
  left_join(sample_demographics, by = "fips_code")

# Save enhanced data
write_csv(jurisdiction_data_enhanced, "dashboard_data/jurisdiction_data_enhanced.csv")

cat("Sample demographic data created and saved!\\n")
cat("You can now update the dashboard to display these new fields.\\n")
')

cat("\n\n=== SUMMARY ===\n")
cat("Recommended data sources:\n")
cat("1. Population: US Census Bureau County Population Estimates\n")
cat("2. RUCC: USDA Rural-Urban Continuum Codes\n")
cat("3. Income: Census Bureau SAIPE\n")
cat("4. Education: American Community Survey (ACS)\n")
cat("5. Age: Census Population Estimates\n")
cat("6. Race/Ethnicity: Census or ACS\n")
cat("7. Political Context: MIT Election Lab or state election offices\n")
cat("\nAll data should be matched using 5-digit FIPS codes.\n")
cat("\nAfter adding this data, you can enhance:\n")
cat("  - Jurisdiction Profile tab with demographic details\n")
cat("  - Similar Jurisdictions to include demographic matching\n")
cat("  - New visualizations comparing performance by rurality, income, etc.\n")
