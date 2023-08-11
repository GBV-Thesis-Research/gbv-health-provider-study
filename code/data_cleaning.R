##################################################
## Project:
## Script purpose:
## Date:
## Author: Jessica Dyer
##################################################

# SETUP ------------------------------------------------------------------------
# WD setup
current_wd <- getwd()

if (endsWith(current_wd, "gbv-health-provider-study")) {
  gbv_project_wd <- current_wd
} else if (endsWith(current_wd, "/paper")) {
  gbv_project_wd <- str_remove(current_wd, "/paper")
} else {
  print("Got a WD that's not handled in the If-else ladder yet")
}


source(paste(gbv_project_wd, "/code/participant_id_cleaning.R", sep = ""))

data <- readRDS(path_to_data_rds)

# Lint current file
style_file(paste(gbv_project_wd, "/code/data_cleaning.R", sep = ""))

# DATA MANAGEMENT --------------------------------------------------------------
# Rearrange columns
data <- data %>%
  select(all_of(columns_to_move), everything())

# Clean up demographic data
data <- data %>%
  mutate(sex_factored = factor(sex,
    levels = c(1, 2, 3),
    labels = c("Female", "Male", "Other")
  )) %>%
  mutate(age_groups = factor(age,
    levels = c(1, 2, 3, 4, 5),
    labels = c(
      "Less than 25 years old", "25-34 years old",
      "35-44 years old", "45-54 years old",
      "55 years or older"
    )
  )) %>%
  mutate(position_groups = factor(position,
    levels = c(1, 2, 3, 4, 5, 6, 7, 8),
    labels = c(
      "Community health worker",
      "Medical doctor", "Midwife",
      "Nurse", "Nursing assistant",
      "Social worker or counsellor",
      "Manager", "Other"
    )
  )) %>%
  mutate(previous_training_factored = factor(previous_training,
    levels = c(1, 2),
    labels = c("Yes", "No")
  )) %>%
  mutate(avg_weekly_pt_volume = factor(patient_volume,
    levels = c(1, 2, 3, 4, 5),
    labels = c(
      "Currently not seeing patients",
      "Less than 20", "20-39", "40-59",
      "60 or more"
    )
  )) %>%
  mutate(position_years_clean = ifelse(
    position_years > 99, 2021 - position_years, position_years
  )) %>%
  select(-matches("fup"))

# drop participant 11 due to having only post tests
# (388 to 386 rows) removes 2 rows
data <- data[data$participant_id != 11, ]

# create an actual date column
data$date_as_date_format <- ifelse(is.na(data$date), NA,
  format(as.Date(data$date, format = "%Y-%m-%d"), "%Y-%m-%d")
)

data <- data %>%
  mutate(
    year_diff = ifelse(nchar(position_years) == 4,
      as.numeric(year(date_as_date_format) - position_years),
      NA
    ),
    position_years_clean = ifelse(!is.na(year_diff), year_diff, position_years)
  )

# change missing responses for knowledge questions from "NA" to 99
data <- data %>%
  mutate(across(
    .cols = contains("knowledge"),
    .fns = ~ replace_na(., 99)
  ))

#' The code below addresses a skip logic issue between questions 18 and 19.
#' In the survey, respondents who answered "no" or "NA" to question 18 were not
#' supposed to answer question 19. However, due to inconsistencies, many respondents
#' who answered "no" or "NA" on question 18 still answered question 19. To resolve
#' this, the code creates a new variable "practices_clean_19x" and assigns "NAs"
#' to all variables in question 19 for respondents who answered "no" or "NA" in
#' question 18, thereby cleaning up the data.

# Recode question 18 to be 1 = yes, 0 = no for providers having identified a woman
# suffering DV in the past month.
data <- data %>%
  mutate(practices_18 = case_when(
    practices_18 %in% c(2, 3) ~ 0,
    TRUE ~ practices_18
  ))

# Create new variables "practices_clean_19x". If providers had identified a woman
# suffering domestic violence in the past month, then include their answers to question
# 19. If they had not identified a woman suffering DV in the past month, code as NA.
data <- data %>%
  mutate(practices_clean_19a = ifelse(practices_18 == 1, practices_19a, NA)) %>%
  mutate(practices_clean_19b = ifelse(practices_18 == 1, practices_19b, NA)) %>%
  mutate(practices_clean_19c = ifelse(practices_18 == 1, practices_19c, NA)) %>%
  mutate(practices_clean_19d = ifelse(practices_18 == 1, practices_19d, NA)) %>%
  mutate(practices_clean_19e = ifelse(practices_18 == 1, practices_19e, NA)) %>%
  mutate(practices_clean_19f = ifelse(practices_18 == 1, practices_19f, NA)) %>%
  mutate(practices_clean_19g = ifelse(practices_18 == 1, practices_19g, NA)) %>%
  mutate(practices_clean_19h = ifelse(practices_18 == 1, practices_19h, NA)) %>%
  mutate(practices_clean_19i = ifelse(practices_18 == 1, practices_19i, NA))

# Drop original question 19 from data
data <- data %>%
  select(-starts_with("practices_19"))

# Write data to folder
path_to_clean_rds <- paste(gbv_project_wd, "/data/clean/gbv_data_clean.RDS", sep = "")
saveRDS(data, file = path_to_clean_rds)
