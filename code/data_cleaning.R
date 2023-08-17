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

source(paste(gbv_project_wd, "/code/data_import.R", sep = ""))

# Lint current file
style_file(paste(gbv_project_wd, "/code/data_cleaning.R", sep = ""))

# DATA MANAGEMENT --------------------------------------------------------------
path_to_facility_file <- paste(gbv_project_wd, "/data/extra_data/facility_names.xlsx", sep = "")

standard_facility_names <- read_excel(path_to_facility_file, sheet = "facilities")
standard_facility_names <- standard_facility_names %>% mutate(full_name = paste(type, name, sep = " "))

# Separate out key from data (key is the redcap entry form containing correct answers)
key <- raw_gbv_survey_data %>%
  filter(participant_id == "KEY") %>%
  select(everything())

data <- raw_gbv_survey_data %>%
  mutate(time_point = if_else(date %in% c("2003-07-03", "2003-07-10", "2017-07-10", "2023-07-03", "2023-07-10"), 3, time_point)) %>%
  filter(time_point != 3)

# Drop participants that have not consented to have their data used for research
# Drops from 972 to 929 (removes 43 rows)
data <- data %>%
  filter(consent == 1) %>%
  filter(!date %in% c("2021-06-18", "2021-06-14")) %>%
  filter(municipality != "Dili") %>%
  filter(participant_id != "KEY")

# Standardize municipality names
data <- data %>%
  mutate(municipality = if_else(municipality %in% c("LIQUICA", "Liquisa", "LIQUISA", "Liqujca"),
    "Liquica",
    municipality
  ))

# Filter out identical records which were accidentally imported into RedCap twice
# by the HAMNASA data collection team
# Drops from 929 to 416 (removes 513 rows)
columns_to_not_select <- ("record_id")
all_columns <- colnames(data)

# When the data is first imported, X does not exist in the data. However, it
# exists later. This is to handle that case.
if ("X" %in% all_columns) {
  columns_to_not_select <- c(columns_to_not_select, "X")
}
data <- data %>%
  select(-all_of(columns_to_not_select)) %>%
  distinct()

# Fix dates that are empty
data <- data %>%
  mutate(date = case_when(
    date == "" & facility %in% c("hatulia", "Hatulia") & time_point == 2 ~ "2021-09-24",
    date == "" & facility == "Maubara" & time_point == 1 ~ "2021-10-11",
    date == "" & facility == "Maubara" & time_point == 2 ~ "2021-10-15",
    TRUE ~ date
  ))

# Standardize municipality
data <- data %>%
  mutate(municipality = str_to_title(municipality))

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

data <- data %>%
  mutate(training_group = case_when(
    date %in% c("2021-07-12", "2021-07-16") ~ "Liquica R1 and R2",
    date %in% c("2021-08-17", "2021-08-16", "2021-08-20") ~ "Bazartete R1 and R2",
    date %in% c("2021-09-20", "2021-09-24") & municipality == "Liquica" ~ "Maubara R1",
    date %in% c("2021-10-15", "2021-10-11") ~ "Maubara R2",
    date %in% c("2021-07-19", "2021-07-23") ~ "Ermera combined R1",
    date %in% c("2021-10-18", "2021-10-22") ~ "Ermera combined R2",
    date %in% c("2021-09-13", "2021-09-17") ~ "Atsabe",
    date %in% c("2021-10-26", "2021-10-30") ~ "Letefoho",
    date %in% c("2021-09-20", "2021-09-24") & municipality == "Ermera" ~ "Hatolia",
  ))

# Drop original question 19 from data
data <- data %>%
  select(-starts_with("practices_19"))

data <- data %>%
  mutate(facility_name_title_case = map(facility, str_to_title)) %>%
  mutate(facility_name_title_case = ifelse(facility_name_title_case %in% c("Ps Asulau"), "Asulau Sare", facility_name_title_case)) %>%
  mutate(facility_name_title_case = ifelse(facility_name_title_case %in% c("Ps Vatunau Ediri"), "Ediri", facility_name_title_case)) %>%
  mutate(facility_name_title_case = ifelse(facility_name_title_case %in% c("Postu Saude Leotela"), "Leotala", facility_name_title_case)) %>%
  mutate(facility_name_title_case = ifelse(facility_name_title_case %in% c("Chcguisaduru"), "Guissarudo", facility_name_title_case)) %>%
  mutate(facility_name_title_case = ifelse(facility_name_title_case %in% c("Ps Bakhita"), "Bakita Eraulo", facility_name_title_case)) %>%
  mutate(facility_name_title_case = ifelse(facility_name_title_case %in% c("Hp Leofela"), "Leotala", facility_name_title_case)) %>%
  mutate(facility_name_title_case = ifelse(facility_name_title_case %in% c("Ps Falihbo"), "Fahilebu", facility_name_title_case))

data <- data %>%
  mutate(
    standardized_facility = map_chr(facility_name_title_case, function(fac) {
      dist_matrix <- stringdist::stringdistmatrix(fac, standard_facility_names$full_name)
      closest_standard <- standard_facility_names$full_name[which.min(dist_matrix)]
      return(closest_standard)
    })
  ) %>%
  mutate(standardized_facility = ifelse(facility_name_title_case %in% c(
    "Ssam / Dhs", "Ps Estado", "Postu Da Saude", "Chc Centru Saude", "Js200921", "Hp", "Posto Saude", "Cgc"
  ), NA, standardized_facility)) %>%
  relocate(standardized_facility, .after = facility)
# Write data to folder
path_to_clean_rds <- paste(gbv_project_wd, "/data/clean/gbv_data_interim_clean.RDS", sep = "")
saveRDS(data, file = path_to_clean_rds)
