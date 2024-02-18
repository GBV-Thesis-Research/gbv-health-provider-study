##################################################
## Project: GBV Health provider study
## Script purpose: Exploratory Data Analysis
## Date: 2-17-24
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

# Load data 
analysis_df_fp_wide <- paste(gbv_project_wd, "/data/clean/analysis_data_wide.RDS", sep = "")
df_wide <- readRDS(analysis_df_fp_wide)

analysis_df_fp_long <- paste(gbv_project_wd, "/data/clean/analysis_data_long.RDS", sep = "")
df_long <- readRDS(analysis_df_fp_long)

# Create attendance table
attendance_hist <- hist(df_wide$attendance_score_FUAT, 
                        main = "FUAT Attendance", 
                        xlab = "Number of FUAT sessions attended", 
                        ylab = "Frequency")

# Create dataframe to identify score drops from baseline to endline and 
# midline to endline 
score_drop <- df_wide %>%
  mutate(know_base_to_end = knowledge_overall_3 - knowledge_overall_1,
         att_base_to_end = attitude_overall_3 - attitude_overall_1,
         emp_base_to_end = empathy_score_3 - empathy_score_1,
         syssup_base_to_end = system_support_score_3 - system_support_score_1,
         conf_base_to_end = confidence_score_3 - confidence_score_1,
         know_mid_to_end = knowledge_overall_3 - knowledge_overall_2,
         att_mid_to_end = attitude_overall_3 - knowledge_overall_2,
         emp_mid_to_end = empathy_score_3 - empathy_score_2,
         syssup_mid_to_end = system_support_score_3 - system_support_score_2,
         conf_mid_to_end = confidence_score_3 - confidence_score_2
         ) %>%
  select(know_base_to_end, att_base_to_end, emp_base_to_end, conf_base_to_end, 
         syssup_base_to_end, know_mid_to_end, att_mid_to_end, syssup_mid_to_end,
         emp_mid_to_end, conf_mid_to_end) %>%
  mutate(know_base_to_end_factor = case_when(know_base_to_end < 0 ~ 1,
                                             know_base_to_end == 0 ~ 2,
                                             know_base_to_end > 0 ~ 3),
         att_base_to_end_factor = case_when(att_base_to_end < 0 ~ 1,
                                             att_base_to_end == 0 ~ 2,
                                             att_base_to_end > 0 ~ 3),
         emp_base_to_end_factor = case_when(emp_base_to_end < 0 ~ 1,
                                            emp_base_to_end == 0 ~ 2,
                                            emp_base_to_end > 0 ~ 3),
         syssup_base_to_end_factor = case_when(syssup_base_to_end < 0 ~ 1,
                                               syssup_base_to_end == 0 ~ 2,
                                               syssup_base_to_end > 0 ~ 3),         
         conf_base_to_end_factor = case_when(conf_base_to_end < 0 ~ 1,
                                             conf_base_to_end == 0 ~ 2,
                                             conf_base_to_end > 0 ~ 3),
         know_mid_to_end_factor = case_when(know_mid_to_end < 0 ~ 1,
                                            know_mid_to_end == 0 ~ 2,
                                            know_mid_to_end > 0 ~ 3),
         att_mid_to_end_factor = case_when(att_mid_to_end < 0 ~ 1,
                                           att_mid_to_end == 0 ~ 2,
                                            att_mid_to_end > 0 ~ 3),
         emp_mid_to_end_factor = case_when(emp_mid_to_end < 0 ~ 1,
                                           emp_mid_to_end == 0 ~ 2,
                                           emp_mid_to_end > 0 ~ 3),
         syssup_mid_to_end_factor = case_when(syssup_mid_to_end < 0 ~ 1,
                                              syssup_mid_to_end == 0 ~ 2,
                                              syssup_mid_to_end > 0 ~ 3),
         conf_mid_to_end_factor = case_when(conf_mid_to_end < 0 ~ 1,
                                            conf_mid_to_end == 0 ~ 2,
                                            conf_mid_to_end > 0 ~ 3))
