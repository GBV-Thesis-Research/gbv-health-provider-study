##################################################
## Project: GBV Health provider study
## Script purpose: Data analysis
## Date: 1-13-24
## Author: Jessica Dyer
##################################################

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

source(paste(gbv_project_wd, "/code/dependencies.R", sep = ""))

source(paste(gbv_project_wd, "/code/analysis_prep.R", sep = ""))
style_file(paste(gbv_project_wd, "/code/analysis_prep.R", sep = ""))

analysis_df_fp_wide <- paste(gbv_project_wd, "/data/clean/analysis_data_wide.RDS", sep = "")
df_wide <- readRDS(analysis_df_fp_wide)

analysis_df_fp_long <- paste(gbv_project_wd, "/data/clean/analysis_data_long.RDS", sep = "")
df_long <- readRDS(analysis_df_fp_long)

# Score comparison table (at timepoints 1 & 3)
comparison_tbl_1 <-
  df_long %>%
  filter(time_point %in% c(1, 3)) %>%
  select(
    time_point, knowledge_overall, attitude_overall, system_support_score,
    confidence_score, empathy_score
  ) %>%
  tbl_summary(
    by = time_point, type = list(
      system_support_score ~ "continuous"
    ),
    label = list(
      knowledge_overall ~ "Knowledge (43)",
      attitude_overall ~ "Attitude (102)",
      system_support_score ~ "System Support (6)",
      confidence_score ~ "Confidence (40)",
      empathy_score ~ "Empathy (64)"
    )
  ) %>%
  modify_header(
    label = "**Domain (total points)**",
    stat_1 = "**Baseline**",
    stat_2 = "**Endline**"
  ) %>%
  add_p()

comparison_tbl_2 <-
  df_long %>%
  filter(time_point %in% c(2, 3)) %>%
  select(
    time_point, knowledge_overall, attitude_overall, system_support_score,
    confidence_score, empathy_score
  ) %>%
  tbl_summary(
    by = time_point, type = list(
      system_support_score ~ "continuous"
    ),
    label = list(
      knowledge_overall ~ "Knowledge (43)",
      attitude_overall ~ "Attitude (102)",
      system_support_score ~ "System Support (6)",
      confidence_score ~ "Confidence (40)",
      empathy_score ~ "Empathy (64)"
    )
  ) %>%
  modify_header(
    label = "**Domain (total points)**",
    stat_1 = "**Post-intensive training**",
    stat_2 = "**Endline**"
  ) %>%
  add_p()

comparison_tbl_3 <-
  df_long %>%
  filter(time_point %in% c(1, 2)) %>%
  select(
    time_point, knowledge_overall, attitude_overall, system_support_score,
    confidence_score, empathy_score
  ) %>%
  tbl_summary(
    by = time_point, type = list(
      system_support_score ~ "continuous"
    ),
    label = list(
      knowledge_overall ~ "Knowledge (43)",
      attitude_overall ~ "Attitude (102)",
      system_support_score ~ "System Support (6)",
      confidence_score ~ "Confidence (40)",
      empathy_score ~ "Empathy (64)"
    )
  ) %>%
  modify_header(
    label = "**Domain (total points)**",
    stat_1 = "**Baseline**",
    stat_2 = "**Post-intensive training**"
  ) %>%
  add_p()
