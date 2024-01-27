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

analysis_df_fp_wide <- paste(gbv_project_wd, "/data/clean/analysis_data_wide.RDS", sep = "")
df_wide <- readRDS(analysis_df_fp_wide)

analysis_df_fp_long <- paste(gbv_project_wd, "/data/clean/analysis_data_long.RDS", sep = "")
df_long <- readRDS(analysis_df_fp_long)

comparison_tbl_1 <-
  df_long %>%
  filter(time_point %in% c(1, 3)) %>%
  select(time_point, knowledge_overall, attitude_overall, system_support_score, 
         confidence_score, empathy_score, practice_score) %>%
  tbl_summary(by=time_point, type = list(system_support_score ~ "continuous", 
                                         practice_score ~ "continuous")) %>% 
  add_n() %>% 
  add_p()

# Plot scores across timepoints 
scores_plot <- df_long %>%
  ggplot(aes(x = time_point, y = knowledge_overall)) +
  stat_summary(fun = median, geom = "point", color = "blue") +
  # geom_point() + facet_grid() +  # Add points, or other geoms as needed
  labs(title = "Knowledge Over Time",
       x = "Time Point",
       y = "Median Knowledge Overall")

example_regession <- lm(knowledge_overall_3 ~ sex_factored + attendance_score, data = df_wide)