# Load necessary libraries
library(dplyr)
library(readr)

# --- 1. Load All THREE Raw Datasets ---

# Load your existing local files
schools_raw <- read_csv("schools_district.csv")
population_raw <- read_csv("population_district.csv")

# Load the new socioeconomic dataset from the web
# Make sure you have downloaded hies_state.csv into the same folder
hies_raw <- read_csv("hies_state.csv")


# --- 2. Process School and Population Data (Aggregating to STATE level) ---

# a) Get the most recent school count (2022) by STATE
schools_by_state <- schools_raw %>%
  filter(date == "2022-01-01", stage %in% c("primary", "secondary")) %>%
  group_by(state) %>%
  summarise(Total_Schools = sum(schools, na.rm = TRUE), .groups = 'drop')

# b) Get the most recent school-age population (2024, ages 5-19) by STATE
population_by_state <- population_raw %>%
  filter(
    date == "2024-01-01",
    sex == "both",
    ethnicity == "overall",
    age %in% c("5-9", "10-14", "15-19")
  ) %>%
  group_by(state) %>%
  summarise(School_Age_Population = sum(population, na.rm = TRUE) * 1000, .groups = 'drop')


# --- 3. Clean the Socioeconomic Data ---

# a) Filter HIES data for the most recent year (2022)
socioeconomic_by_state <- hies_raw %>%
  filter(date == "2022-01-01") %>%
  select(
    state,
    Median_Income_RM = income_median,
    Poverty_Rate_Percent = poverty
  )


# --- 4. Merge All Three Datasets and Create the Master File ---

master_data <- schools_by_state %>%
  inner_join(population_by_state, by = "state") %>%
  inner_join(socioeconomic_by_state, by = "state") %>%
  filter(state != "Malaysia") %>% # Remove the national aggregate row
  mutate(
    # The key normalized metric for resource allocation
    Students_per_School = round(School_Age_Population / Total_Schools)
  )

# --- 5. Export the Final, Cleaned CSV ---
write_csv(master_data, "state_edu_socioeconomic_master.csv")

print("SUCCESS: 'state_edu_socioeconomic_master.csv' has been created.")
print("This single file will power all visualizations.")
print(head(master_data))
