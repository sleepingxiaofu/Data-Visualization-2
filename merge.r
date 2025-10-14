# Load necessary libraries
library(dplyr)
library(readr)

# --- 1. Load All Raw Datasets ---

# Load your existing local files
schools_district <- read_csv("schools_district.csv")
population_district <- read_csv("population_district.csv")

# Load the new datasets
# NOTE: You must create 'spm_results_state.csv' manually first.
hies_state <- read_csv("hies_state.csv") 
spm_results <- read_csv("spm_results_state.csv")

# --- 2. Process and Aggregate Existing Data ---

# a) Aggregate total schools by state for the latest year (2022)
schools_by_state <- schools_district %>%
  filter(date == "2022-01-01", state != "Malaysia") %>%
  group_by(state) %>%
  summarise(Total_Schools = sum(schools, na.rm = TRUE))

# b) Aggregate total population by state for the latest year (2024)
population_by_state <- population_district %>%
  filter(date == "2024-01-01", 
         sex == "both", 
         age == "overall", 
         ethnicity == "overall",
         state != "Malaysia") %>%
  group_by(state) %>%
  summarise(Total_Population_K = sum(population, na.rm = TRUE)) %>%
  mutate(Total_Population = Total_Population_K * 1000) # Convert from thousands

# --- 3. Clean and Select from New Datasets ---

# a) Clean HIES data for the latest year (2022) and select key columns
socioeconomic_data <- hies_state %>%
  filter(date == "2022-01-01") %>%
  select(
    State = state,
    Median_Income_RM = income_median,
    Poverty_Rate_Percent = poverty
  )

# b) Clean SPM results data
performance_data <- spm_results %>%
  select(State, SPM_Pass_Rate_Percent = SPM_GPN_2023) # Assuming lower GPN is better, we can invert if needed in visualization

# --- 4. Merge All Datasets into One ---

# Start with the school and population data
merged_data <- schools_by_state %>%
  inner_join(population_by_state, by = c("state" = "state")) %>%
  # Now join the socioeconomic data
  inner_join(socioeconomic_data, by = c("state" = "State")) %>%
  # Finally, join the performance data
  inner_join(performance_data, by = c("state" = "State"))

# --- 5. Calculate Normalized Metrics and Finalize ---

final_data <- merged_data %>%
  mutate(
    # This is our key normalized metric for resource comparison
    Schools_per_100k_Pop = (Total_Schools / Total_Population) * 100000
  ) %>%
  # Select and rename columns for clarity in Vega-Lite
  select(
    State = state,
    Median_Income_RM,
    SPM_Pass_Rate_Percent,
    Schools_per_100k_Pop,
    Poverty_Rate_Percent
  )

# --- 6. Export the Final CSV ---

write_csv(final_data, "merged_education_data_state.csv")

# Print a success message and show the final data structure
print("Successfully created merged_education_data_state.csv")
print(head(final_data))
