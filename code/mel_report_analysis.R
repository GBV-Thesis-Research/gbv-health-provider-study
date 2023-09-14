##################################################
## Project: GBV Health Provider Study
## Script purpose: Running analysis for GBV HAMNASA USAID report
## Date: 09-05-2023
## Author: Susan Glenn
##################################################

# SETUP ------------------------------------------------------------------------
# WD Setup
current_wd <- getwd()

# Lint current file
if (endsWith(current_wd, "gbv-health-provider-study")) {
  gbv_project_wd <- current_wd
} else if (endsWith(current_wd, "/paper")) {
  gbv_project_wd <- str_remove(current_wd, "/paper")
} else {
  print("Got a WD that's not handled in the If-else ladder yet")
}

source(paste(gbv_project_wd, "/code/dependencies.R", sep = ""))
style_file(paste(gbv_project_wd, "/code/mel_report_analysis.R", sep = ""))

path_to_clean_rds_scores <- paste(gbv_project_wd, "/data/clean/gbv_data_scores.RDS", sep = "")
clean_data_scores <- readRDS(path_to_clean_rds_scores)

path_to_clean_three_timepoints <- paste(gbv_project_wd, "/data/clean/gbv_data_clean_three_timepoints.RDS", sep = "")
clean_data <- readRDS(path_to_clean_three_timepoints)

# Reduce data to only include matches (n = 46)
clean_data_scores <- clean_data_scores %>%
  filter(status == "All three" & time_point != 2) %>%
  select(
    participant_id_3, region, time_point, outcome4_score,
    outcome5_score
  )

# CREATE NEW DATAFRAMES FOR MEDIAN REGIONAL SCORES------------------------------
regional_scores <- clean_data_scores %>%
  ##  group_by(time_point) %>%
  pivot_wider(names_from = c(region), values_from = c(outcome4_score, outcome5_score))

regional_scores <- regional_scores %>%
  mutate(sum_outcome4 = rowSums(select(., starts_with("outcome4")), na.rm = TRUE)) %>%
  mutate(sum_outcome5 = rowSums(select(., starts_with("outcome5")), na.rm = TRUE))

# tried creating a difference column here, though it isn't working and I don't think it's the right approach
# regional_scores <- regional_scores %>%
#   group_by(participant_id_3) %>%
#   mutate(difference_outcome4 = sum_outcome4 - lag(sum_outcome4))

# Create Tables
outcome4_columns <- names(regional_scores)[grep("outcome4", names(regional_scores))]
outcome_4_table <-
  regional_scores %>%
  select(all_of(outcome4_columns), time_point) %>%
  tbl_summary(
    by = time_point,
    type = c(outcome4_columns) ~ "continuous",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})"
    ),
    missing = "no",
    label = list(
      outcome4_score_Gleno ~ "Gleno",
      outcome4_score_Guissarudo ~ "Guissarudo",
      outcome4_score_Atsabe ~ "Atsabe",
      outcome4_score_Maubara ~ "Maubara",
      outcome4_score_Hatolia ~ "Hatolia",
      outcome4_score_Liquica ~ "Liquica",
      outcome4_score_Bazartete ~ "Bazartete",
      outcome4_score_Railaco ~ "Railaco",
      outcome4_score_Letefoho ~ "Letefoho",
      sum_outcome4 ~ "All Facilities"
    )
  ) %>%
  add_n() %>%
  add_difference() %>%
  modify_header(
    label = "**Facility**",
    stat_1 = "**Timepoint 1**",
    stat_2 = "**Timepoint 3**"
  )

outcome5_columns <- names(regional_scores)[grep("outcome5", names(regional_scores))]
outcome_5_table <-
  regional_scores %>%
  select(all_of(outcome5_columns), time_point) %>%
  tbl_summary(
    by = time_point,
    type = c(outcome5_columns) ~ "continuous",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})"
    ),
    missing = "no",
    label = list(
      outcome5_score_Gleno ~ "Gleno",
      outcome5_score_Guissarudo ~ "Guissarudo",
      outcome5_score_Atsabe ~ "Atsabe",
      outcome5_score_Maubara ~ "Maubara",
      outcome5_score_Hatolia ~ "Hatolia",
      outcome5_score_Liquica ~ "Liquica",
      outcome5_score_Bazartete ~ "Bazartete",
      outcome5_score_Railaco ~ "Railaco",
      outcome5_score_Letefoho ~ "Letefoho",
      sum_outcome5 ~ "All Facilities"
    )
  ) %>%
  add_n() %>%
  add_difference() %>%
  modify_header(
    label = "**Facility**",
    stat_1 = "**Timepoint 1**",
    stat_2 = "**Timepoint 3**"
  )
