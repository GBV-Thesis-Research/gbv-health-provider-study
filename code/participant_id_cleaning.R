##################################################
## Project: GBV health provider study
## Script purpose: Participant ID cleaning
## Date: 2023-08-10
## Author: Susan Glenn
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

source(paste(gbv_project_wd, "/code/data_cleaning.R", sep = ""))
# Lint current file
style_file(paste(gbv_project_wd, "/code/participant_id_cleaning.R", sep = ""))

path_to_imterim_clean_rds <- paste(gbv_project_wd, "/data/clean/gbv_data_interim_clean.RDS", sep = "")
path_to_link_log <- paste(gbv_project_wd, "/extra_data/link_log.xlsx", sep = "")

data <- readRDS(path_to_imterim_clean_rds)

link_log <- read_excel(path_to_link_log) %>%
  select(participant_id_2, participant_id_3) %>%
  distinct(participant_id_2, .keep_all = TRUE)

data <- data %>%
  mutate(
    participant_id_2 =
      tolower(str_replace_all(participant_id, "[0-9[:space:][:punct:]]", ""))
  ) %>%
  mutate(sex_matching = ifelse(sex == 3, NA, sex)) %>%
  mutate(participant_id_2 = ifelse(participant_id_2 == "", participant_id, participant_id_2)) %>%
  mutate(participant_id_2 = paste(participant_id_2, tolower(str_replace_all(standardized_facility, " ", "_")), sep = "_")) %>%
  group_by(participant_id_2) %>%
  fill(sex_matching, .direction = "downup") %>%
  ungroup() %>%
  group_by(participant_id_2, time_point) %>%
  mutate(row_number = row_number()) %>%
  mutate(participant_id_2 = ifelse(row_number > 1, paste0(participant_id_2, "_", row_number), participant_id_2)) %>%
  select(-row_number) %>%
  ungroup() %>%
  group_by(participant_id_2) %>%
  mutate(same_sex_all = n_distinct(sex_matching) == 1) %>%
  mutate(participant_id_2 = ifelse(same_sex_all != 1, paste0(participant_id_2, "_", sex_matching), participant_id_2)) %>%
  ungroup() %>%
  left_join(link_log, by = "participant_id_2") %>%
  group_by(participant_id_3) %>%
  fill(standardized_facility, region, .direction = "downup") %>%
  mutate(
    entries_1 = sum(time_point == 1),
    entries_2 = sum(time_point == 2),
    entries_3 = sum(time_point == 3)
  ) %>%
  mutate(status = ifelse(entries_1 == 1 & entries_2 == 1 & entries_3 == 1, "All three", NA)) %>%
  mutate(inclusive_status = ifelse(entries_1 == 1 & entries_2 == 1 & (entries_3 == 1 | entries_3 == 0), "One & two inclusive", NA)) %>%
  mutate(status = ifelse(entries_1 == 1 & entries_2 == 1 & entries_3 == 0, "One & two only", status)) %>%
  mutate(status = ifelse(entries_1 == 1 & entries_2 == 0 & entries_3 == 1, "One & three only", status)) %>%
  mutate(status = ifelse(entries_1 == 0 & entries_2 == 1 & entries_3 == 1, "Two & three only", status)) %>%
  mutate(status = ifelse(entries_1 == 1 & entries_2 == 0 & entries_3 == 0, "One only", status)) %>%
  mutate(status = ifelse(entries_1 == 0 & entries_2 == 1 & entries_3 == 0, "Two only", status)) %>%
  mutate(status = ifelse(entries_1 == 0 & entries_2 == 0 & entries_3 == 1, "Three only", status)) %>%
  mutate(status = ifelse(is.na(status), "Other", status)) %>%
  ungroup() %>%
  relocate(c(status, participant_id_3), .after = time_point)

# Recode participant_id_3 #9 to Ermera facility for all timepoints. We confirmed he is the same individual who
# moved facilities in the last month of the intervention, so will maintain the same facility as at baseline for the purposes
# of the analysis.
data <- data %>%
  mutate(
    region = case_when(
      participant_id_3 == 9 ~ "Ermera",
      TRUE ~ region
    ),
    standardized_facility = case_when(
      participant_id_3 == 9 ~ "Ermera",
      TRUE ~ standardized_facility
    )
  )

data_with_three_time_points <- data %>%
  filter(status == "All three") %>%
  select(-all_of(c(
    "participant_id", "entries_1", "entries_2", "entries_3", "facility_name_title_case",
    "participant_id_2", "facility", "date"
  )))

clean_data <- data %>%
  select(-all_of(c(
    "participant_id", "entries_1", "entries_2", "entries_3", "facility_name_title_case",
    "participant_id_2", "facility", "date", "position_years", "position_months"
  )))

participant_id_table_data <- clean_data %>%
  select(participant_id_3, status, inclusive_status, standardized_facility) %>%
  distinct(participant_id_3, status, standardized_facility, inclusive_status) %>%
  mutate(status = factor(status, levels = c(
    "All three", "One & two only",
    "One & three only", "Two & three only",
    "One only", "Two only", "Three only"
  )))

participant_id_table <-
  participant_id_table_data %>%
  select(status, inclusive_status) %>%
  tbl_summary(
    label = list(status ~ "Exclusive timepoint status", inclusive_status ~ "Inclusive timepoint status")
  )

# Create dataframe for mismatched demographic data from pre, post-intensive, and follow-up tests
dem_info <- clean_data %>%
  group_by(participant_id_3) %>%
  summarize(
    num_timepoints = n_distinct(time_point),
    same_sex_all = n_distinct(sex_matching) == 1,
    same_age_all = n_distinct(age) == 1,
    same_position_all = n_distinct(position) == 1
  )

# Identify which participants have mismatched demographic data across timepoints
pids_to_check <- dem_info %>%
  group_by(participant_id_3) %>%
  filter(any(!same_position_all)) %>%
  distinct(participant_id_3) %>%
  select(participant_id_3)

pids_to_check <- pids_to_check$participant_id_3

# Write data to folder
path_to_clean_rds <- paste(gbv_project_wd, "/data/clean/gbv_data_clean.RDS", sep = "")
path_to_clean_three_timepoints <- paste(gbv_project_wd, "/data/clean/gbv_data_clean_three_timepoints.RDS", sep = "")
saveRDS(clean_data, file = path_to_clean_rds)
saveRDS(data_with_three_time_points, file = path_to_clean_three_timepoints)
