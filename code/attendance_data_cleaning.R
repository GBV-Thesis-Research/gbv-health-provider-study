##################################################
## Project:
## Script purpose:
## Date: 
## Author: Jessica Dyer
##################################################

current_wd <- getwd()

if (endsWith(current_wd, "gbv-health-provider-study")) {
  gbv_project_wd <- current_wd
} else if (endsWith(current_wd, "/paper")) {
  gbv_project_wd <- str_remove(current_wd, "/paper")
} else {
  print("Got a WD that's not handled in the If-else ladder yet")
}

source(paste(gbv_project_wd, "/code/dependencies.R", sep = ""))

style_file(paste(gbv_project_wd, "/code/participant_id_cleaning.R", sep = ""))
path_to_attendance_data <- paste(gbv_project_wd, "/extra_data/attendance_data.xlsx", sep = "")

cols <- c("FUAT 1", "FUAT 4-5", "FUAT 2", "FUAT 6-8", "FUAT 10", "FUAT 14", "FUAT 9", "GBV Training", "FUAT 11-13")
attendance_data <- read_excel(path_to_attendance_data) %>%
  select(participant_id_3, training_session, attendance) %>%
  filter(!is.na(participant_id_3)) %>%
  distinct(participant_id_3, training_session) %>%
  pivot_wider(
    id_cols = participant_id_3,
    names_from = training_session,
    values_from = training_session,
    values_fn = list(training_session = ~ifelse(!is.na(.), 1, NA))
  ) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  rowwise() %>%
  mutate(
    attendance_score = (sum(c_across(cols)))
  ) %>%
  ungroup() %>%
  arrange(participant_id_3)

# Write data to folder
path_to_clean_attendance <- paste(gbv_project_wd, "/data/clean/attendance_data_clean.RDS", sep = "")
saveRDS(attendance_data, file = path_to_clean_attendance)