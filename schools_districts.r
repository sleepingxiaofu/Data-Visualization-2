library(dplyr)
library(readr)

# Read data
schools <- read_csv("schools_district.csv")

# Filter and aggregate
schools_agg <- schools %>%
  filter(grepl("^2022", date), state != "Malaysia", schools > 0) %>%
  group_by(state, stage) %>%
  summarise(schools = sum(schools), .groups = "drop") %>%
  group_by(state) %>%
  mutate(percentage = round(schools / sum(schools) * 100, 1)) %>%
  ungroup() %>%
  mutate(stage = case_when(
    stage == "primary" ~ "Primary",
    stage == "secondary" ~ "Secondary",
    stage == "tertiary" ~ "Tertiary"
  ))

schools_simplified <- schools_agg %>%
  filter(stage %in% c("Primary", "Secondary")) %>%
  group_by(state) %>%
  mutate(percentage = round(schools / sum(schools) * 100, 1)) %>%
  ungroup() %>%
  arrange(state, desc(stage == "Primary"))

# Export
write_csv(schools_simplified, "schools_by_stage_simplified.csv")

# View results
print(schools_simplified)
cat("\nDone")