library(haven)

# SETUP ------------------------------------------------------------------------
current_wd <- getwd()

# Lint current file
if (endsWith(current_wd, "gbv-health-provider-study")) {
  gbv_project_wd <- current_wd
} else if (endsWith(current_wd, "/paper")) {
  gbv_project_wd <- str_remove(current_wd, "/paper")
} else {
  print("Got a WD that's not handled in the If-else ladder yet")
}

# Load data
source(paste(gbv_project_wd, "/code/attendance_data_cleaning.R", sep = ""))
path_to_clean_attendance <- paste(gbv_project_wd, "/data/clean/attendance_data_clean.RDS", sep = "")
attendance_data <- readRDS(path_to_clean_attendance)

# Delete all but FUAT sessions 
attendance_for_mary <- attendance_data %>%
  select("participant_id_3", "attendance_score_FUAT")

# shorten attendance score variable name
attendance_for_mary <- attendance_for_mary %>%
  rename(attendance = attendance_fuat)

# Export attendance data alone for Mary
path_to_attendance_for_mary <- paste(gbv_project_wd, "/data/clean/attendance_data_for_mary.RDS", sep = "")
saveRDS(attendance_for_mary, file = path_to_attendance_for_mary)

write_dta(attendance_for_mary, "/Users/susanglenn/Repositories/gbv-health-provider-study/extra_data/attendance_for_mary.dta")
