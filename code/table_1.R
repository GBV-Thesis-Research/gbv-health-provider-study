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
source(paste(gbv_project_wd, "/code/attendance_data_cleaning.R", sep = ""))

#### Lint current file ####
style_file(paste(gbv_project_wd, "/code/table_1.R", sep = ""))

# Load cleaned data
path_to_clean_rds <- paste(gbv_project_wd, "/data/clean/demographic_data_clean.RDS", sep = "")
clean_data <- readRDS(path_to_clean_rds)

# Create table 1 - for publication
filtered_data <-
  clean_data %>%
  filter(status == "All three") %>%
  mutate(position_groups = droplevels(position_groups))

# Join attendance data to filtered data
filtered_data <- left_join(attendance_data, filtered_data, by = c("participant_id_3")) %>%
  select(-starts_with("FUAT"), -starts_with("GBV"), -"attendance_score")

# Create table 1
demographic_table <- filtered_data %>%
  select(c(
    "sex_factored", "age_collapsed", "position_groups", "position_years_clean",
    "municipality", "previous_training_factored", "attendance_score_FUAT"
  )) %>%
  tbl_summary(by = municipality, label = list(
    sex_factored ~ "Sex",
    age_collapsed ~ "Age (years)",
    position_groups ~ "Position",
    position_years_clean ~ "Years of practice",
    previous_training_factored ~ "Previous GBV Training",
    attendance_score_FUAT ~ "Attendance at Learning Labs (out of 8)"
  ), type = list(position_years_clean ~ "continuous", attendance_score_FUAT ~ "continuous")) %>%
  add_overall() %>%
  add_n()

demographic_table

# Create table 1 - for comparison to <2 timepoints
filtered_data_comparison <-
  clean_data %>%
  mutate(position_groups = droplevels(position_groups))

demographic_table_comparison <- filtered_data_comparison %>%
  select(c(
    "sex_factored", "age_collapsed", "position_groups", "position_years_clean",
    "municipality", "status_binary", "previous_training_factored"
  )) %>%
  tbl_summary(by = status_binary, label = list(
    municipality ~ "Municipality",
    sex_factored ~ "Sex",
    age_collapsed ~ "Age (years)",
    position_groups ~ "Position",
    position_years_clean ~ "Years of practice",
    previous_training_factored ~ "Previous GBV Training"
  ), type = list(position_years_clean ~ "continuous")) %>%
  add_overall() %>%
  add_p() %>%
  add_n()

demographic_table_comparison
