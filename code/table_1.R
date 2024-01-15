##################################################
## Project:
## Script purpose:
## Date:
## Author: Jessica Dyer
##################################################

#### WD SETUP ####
current_wd <- getwd()

if (endsWith(current_wd, "gbv-health-provider-study")) {
  gbv_project_wd <- current_wd
} else if (endsWith(current_wd, "/paper")) {
  gbv_project_wd <- str_remove(current_wd, "/paper")
} else {
  print("Got a WD that's not handled in the If-else ladder yet")
}

source(paste(gbv_project_wd, "/code/dependencies.R", sep = ""))
source(paste(gbv_project_wd, "/code/data_cleaning.R", sep = ""))
source(paste(gbv_project_wd, "/code/demographic_data_cleaning.R", sep = ""))

#### Lint current file ####
style_file(paste(gbv_project_wd, "/code/table_1.R", sep = ""))

# Load cleaned data
path_to_clean_rds <- paste(gbv_project_wd, "/data/clean/demographic_data_clean.RDS", sep = "")
clean_data <- readRDS(path_to_clean_rds)

# Create table
filtered_data <-
  clean_data %>%
  filter(status == "All three") %>%
  mutate(position_groups = droplevels(position_groups))

demographic_table <- filtered_data %>%
  select(c("sex_factored", "age_groups", "position_groups", "position_years_clean", "municipality")) %>%
  tbl_summary(by = municipality, label = list(
    sex_factored ~ "Sex",
    age_groups ~ "Age (years)",
    position_groups ~ "Position",
    position_years_clean ~ "Years of practice"
  ), type = list(position_years_clean ~ "continuous")) %>%
  add_overall() %>%
  add_n()

demographic_table
