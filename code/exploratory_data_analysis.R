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
  mutate(know_base_to_end_factor = case_when(know_base_to_end < 0 ~ "Score decreased",
                                             know_base_to_end == 0 ~ "No improvement",
                                             know_base_to_end > 0 ~ "Score increased"),
         att_base_to_end_factor = case_when(att_base_to_end < 0 ~ "Score decreased",
                                             att_base_to_end == 0 ~ "No improvement",
                                             att_base_to_end > 0 ~ "Score increased"),
         emp_base_to_end_factor = case_when(emp_base_to_end < 0 ~ "Score decreased",
                                            emp_base_to_end == 0 ~ "No improvement",
                                            emp_base_to_end > 0 ~ "Score increased"),
         syssup_base_to_end_factor = case_when(syssup_base_to_end < 0 ~ "Score decreased",
                                               syssup_base_to_end == 0 ~ "No improvement",
                                               syssup_base_to_end > 0 ~ "Score increased"),         
         conf_base_to_end_factor = case_when(conf_base_to_end < 0 ~ "Score decreased",
                                             conf_base_to_end == 0 ~ "No improvement",
                                             conf_base_to_end > 0 ~ "Score increased"),
         know_mid_to_end_factor = case_when(know_mid_to_end < 0 ~ "Score decreased",
                                            know_mid_to_end == 0 ~ "No improvement",
                                            know_mid_to_end > 0 ~ "Score increased"),
         att_mid_to_end_factor = case_when(att_mid_to_end < 0 ~ "Score decreased",
                                           att_mid_to_end == 0 ~ "No improvement",
                                            att_mid_to_end > 0 ~ "Score increased"),
         emp_mid_to_end_factor = case_when(emp_mid_to_end < 0 ~ "Score decreased",
                                           emp_mid_to_end == 0 ~ "No improvement",
                                           emp_mid_to_end > 0 ~ "Score increased"),
         syssup_mid_to_end_factor = case_when(syssup_mid_to_end < 0 ~ "Score decreased",
                                              syssup_mid_to_end == 0 ~ "No improvement",
                                              syssup_mid_to_end > 0 ~ "Score increased"),
         conf_mid_to_end_factor = case_when(conf_mid_to_end < 0 ~ "Score decreased",
                                            conf_mid_to_end == 0 ~ "No improvement",
                                            conf_mid_to_end > 0 ~ "Score increased"))

base_to_end_score_tbl <- score_drop %>%
  select(ends_with("base_to_end_factor")) %>%
  tbl_summary(label = list(
    know_base_to_end_factor ~ "Knowledge - Baseline to Endline",
    att_base_to_end_factor ~ "Attitude - Baseline to Endline",
    emp_base_to_end_factor ~ "Empathy - Baseline to Endline",
    syssup_base_to_end_factor ~ "System Support - Baseline to Endline",
    conf_base_to_end_factor ~ "Confidence - Baseline to Endline"))
base_to_end_score_tbl   

mid_to_end_score_tbl <- score_drop %>%
  select(ends_with("mid_to_end_factor")) %>%
  tbl_summary(label = list(
    know_mid_to_end_factor ~ "Knowledge - Midline to Endline",
    att_mid_to_end_factor ~ "Attitude - Midline to Endline",
    emp_mid_to_end_factor ~ "Empathy - Midline to Endline",
    syssup_mid_to_end_factor ~ "System Support - Midline to Endline",
    conf_mid_to_end_factor ~ "Confidence - Midline to Endline"))
mid_to_end_score_tbl   
            
            
            
