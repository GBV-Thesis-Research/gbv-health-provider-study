##################################################
## Project:
## Script purpose:
## Date:
## Author: Jessica Dyer
##################################################

# SETUP ------------------------------------------------------------------------
# WD Setup
current_wd <- getwd()
library(haven)

# Lint current file
if (endsWith(current_wd, "gbv-health-provider-study")) {
  gbv_project_wd <- current_wd
} else if (endsWith(current_wd, "/paper")) {
  gbv_project_wd <- str_remove(current_wd, "/paper")
} else {
  print("Got a WD that's not handled in the If-else ladder yet")
}

source(paste(gbv_project_wd, "/code/dependencies.R", sep = ""))

source(paste(gbv_project_wd, "/code/participant_id_cleaning.R", sep = ""))
path_to_clean_rds <- paste(gbv_project_wd, "/data/clean/gbv_data_clean.RDS", sep = "")
cleaned_data <- readRDS(path_to_clean_three_timepoints)

source(paste(gbv_project_wd, "/code/demographic_data_cleaning.R", sep = ""))
demographic_data_file_path <- paste(gbv_project_wd, "/data/clean/demographic_data_clean.RDS", sep = "")
dem_data <- readRDS(demographic_data_file_path)

path_to_clean_attendance <- paste(gbv_project_wd, "/data/clean/attendance_data_clean.RDS", sep = "")
attendance_data <- readRDS(path_to_clean_attendance)

answers <- cleaned_data %>%
  select(participant_id_3, matches("knowledge|attitudes|system_support|confidence|empathy|practices"))

key_only <- key %>%
  select(matches("knowledge|attitudes|system_support|confidence|empathy|practices"))

# ATTITUDES  -------------------------------------------------------------------
recode_likert_according_to_key <- function(variable_names_to_recode) {
  #' This function is designed to modify attitude variable scores related to Gender-Based Violence (GBV)
  #' based on a provided key. The key indicates whether a positive answer is represented by 1 or 5
  #' for each question in the "attitude vars" set. The function standardizes the scores to ensure higher
  #' values consistently indicate more positive attitudes towards GBV.
  cleaned_data <- cleaned_data %>%
    mutate(across(all_of(variable_names_to_recode), ~ case_when(
      key_only[[cur_column()]] == 5 ~ . - 1,
      key_only[[cur_column()]] == 1 ~ abs(. + 3 - 8),
      TRUE ~ .
    )))
  return(cleaned_data)
}

# Get column names matching 'attitudes'
att_vars <- names(cleaned_data)[str_detect(names(cleaned_data), "attitudes")]

# Get column names matching 'attitudes_12' from att_vars
att12_vars <- att_vars[str_detect(att_vars, "attitudes_12")]

#' Remove the 'attitudes_12' column from the provided data frame, att_vars,
#' as it has 4 possible answers compared to 5 in the other attitude domain questions,
#' necessitating a separate evaluation for Question 12.
att_vars <- att_vars[!str_detect(att_vars, "attitudes_12")]

# Based on the key, readjust values based on desired answer (1 or 5), but
# subtracting 1 when the desired answer is 5. When the desired answer is 1, add 3
# to the value and then subtract 8 so that desired behavior gets the higher score.
cleaned_data <- recode_likert_according_to_key(att_vars)

# Subtract 1 from each answer for att12_vars, then reassigns the value 3 ("I don't know")
# to 1 ("sometimes it is acceptable") for each column in att12_vars.
cleaned_data <- cleaned_data %>%
  mutate(
    across(all_of(att12_vars), ~ . - 1),
    across(all_of(att12_vars), ~ ifelse(. == 3, 1, .))
  )

# CONFIDENCE -------------------------------------------------------------------
#' Rescale to a 0-4 scale
#'
#' This code modified data based on key values for each variable in confidence vars.
#' All items are positively worded, so we moved scale down by 1, with highest confidence
#' score being 4, and low confidence being 0.

#' Get column names matching 'confidence'
conf_vars <- names(cleaned_data)[str_detect(names(cleaned_data), "confidence")]

# Subtracts 1 from each 'confidence' score
cleaned_data <- cleaned_data %>%
  mutate(across(all_of(conf_vars), ~ . - 1))

# EMPATHY -------------------------------------------------------------------
#' Modify data based on key values (rescaled to a 0-4 scale)
#'
#' Modify empathy variable scores related to Gender-Based Violence (GBV)
#' based on a provided key. The key indicates whether a positive answer is represented by 1 or 5
#' for each question in the "empathy vars" set. The function standardizes the scores to ensure higher
#' values consistently indicate more empathy towards those effected by GBV.

# Get column names matching 'empathy'
empathy_vars <- names(data)[str_detect(names(data), "empathy")]
cleaned_data <- recode_likert_according_to_key(empathy_vars)

# CREATE SUB-DOMAINS ---------------------------------------------------------------
# The pre/post test had 5 main domains, with knowledge and attitudes having sub-domains.
# The code below creates those sub-domains for scoring purposes.

# Create knowledge sub-domains (4): general knowledge, warning signs, appropriate ways
# to ask about GBV, and helpful responses to support a woman subjected to GBV
know_vars_general <- names(cleaned_data)[str_detect(names(cleaned_data), "knowledge_7")]
know_vars_warning <- names(cleaned_data)[str_detect(names(cleaned_data), "knowledge_8")]
know_vars_appropriate <- names(cleaned_data)[str_detect(names(cleaned_data), "knowledge_9")]
know_vars_helpful <- names(cleaned_data)[str_detect(names(cleaned_data), "knowledge_10")]

# Create attitudes sub-domains (4): general attitudes towards GBV and the health provider role,
# acceptability for a man to hit his partner, attitudes towards gender roles,
# attitudes towards professional roles
att_vars_general <- names(cleaned_data)[str_detect(names(cleaned_data), "attitudes_11")]
att_vars_acceptability <- names(cleaned_data)[str_detect(names(cleaned_data), "attitudes_12")]
att_vars_genderroles <- names(cleaned_data)[str_detect(names(cleaned_data), "attitudes_13")]
att_vars_profroles <- names(cleaned_data)[str_detect(names(cleaned_data), "attitudes_14")]

# SUM SCORES FOR EACH DOMAIN--------------------------------------------------
# Using the key, score knowledge and system support variables

knowledge_sys_support_key <- key %>%
  select(matches("knowledge|system_support"))

knowledge_sys_support_answers <- cleaned_data %>%
  select(participant_id_3, time_point, standardized_facility, region, matches("knowledge|system_support"))

key_vector <- as.vector(unlist(t(knowledge_sys_support_key)))

participant_ids <- knowledge_sys_support_answers[, 1:4]

knowledge_sys_support_scores_scored <-
  psych::score.multiple.choice(
    key = key_vector,
    data = knowledge_sys_support_answers[, -(1:4)],
    score = FALSE, missing = TRUE, short = TRUE
  )

knowledge_sys_support_scores_raw <- cbind(participant_ids, knowledge_sys_support_scores_scored)

knowledge_sys_support_scores <- knowledge_sys_support_scores_raw %>%
  mutate(
    knowledge_general_score = (rowSums(select(., all_of(matches("knowledge_7"))), na.rm = FALSE)),
    knowledge_warning_score = (rowSums(select(., all_of(matches("knowledge_8"))), na.rm = FALSE)),
    knowledge_appropriate_score = (rowSums(select(., all_of(matches("knowledge_9"))), na.rm = FALSE)),
    knowledge_helpful_score = (rowSums(select(., all_of(matches("knowledge_10"))), na.rm = FALSE)),
    system_support_score = (rowSums(select(., all_of(matches("system_support"))), na.rm = FALSE)),
  ) %>%
  select(
    participant_id_3, time_point, standardized_facility, region, knowledge_general_score, knowledge_warning_score,
    knowledge_appropriate_score, knowledge_helpful_score, system_support_score
  ) %>%
  mutate(
    knowledge_overall = (rowSums(cbind(knowledge_general_score, knowledge_warning_score, knowledge_appropriate_score,
      knowledge_helpful_score,
      na.rm = FALSE
    )))
  )

# Calculate scores by summing up variables for each row and bind participant IDs and timepoint
scores <- cleaned_data %>%
  mutate(
    attitude_general_score = (rowSums(select(., all_of(matches("attitudes_11"))), na.rm = FALSE)),
    attitude_acceptability_score = (rowSums(select(., all_of(matches("attitudes_12"))), na.rm = FALSE)),
    attitude_genderroles_score = (rowSums(select(., all_of(matches("attitudes_13"))), na.rm = FALSE)),
    attitude_profroles_score = (rowSums(select(., all_of(matches("attitudes_14"))), na.rm = FALSE)),
    empathy_score = (rowSums(select(., all_of(empathy_vars)), na.rm = FALSE)),
    confidence_score = (rowSums(select(., all_of(conf_vars)), na.rm = FALSE)),
    practice_score = (rowSums(select(., all_of(pract19_clean_vars)), na.rm = FALSE)),
  ) %>%
  select(
    participant_id_3, status, inclusive_status, standardized_facility, region, time_point, attitude_general_score, attitude_acceptability_score,
    attitude_genderroles_score, attitude_profroles_score, empathy_score,
    confidence_score, practice_score
  )

# Create overall domain scores for knowledge and attitudes
scores <- scores %>%
  mutate(
    attitude_overall = (rowSums(select(., all_of(matches("attitude"))))),
  )

# MERGE DATAFRAMES -------------------------------------------------------------
# Drop unnecessary columns in cleaned_data to avoid mixing up knowledge answers
cleaned_data <- cleaned_data %>% 
  select(participant_id_3, time_point, att_vars, att12_vars, conf_vars, empathy_vars)

# Merge all scores into one data frame
scores_for_imputation <- left_join(knowledge_sys_support_scores, knowledge_sys_support_scores_raw, by = c(
  "participant_id_3", "time_point"))

scores_for_imputation <- left_join(scores_for_imputation, cleaned_data, by = c(
  "participant_id_3", "time_point"))

scores_for_imputation <- left_join(scores_for_imputation, scores, by = c(
  "participant_id_3", "time_point"))

# Drop unnecessary columns in scores_for_imputation dataframe
scores_for_imputation <- scores_for_imputation %>%
  select(-standardized_facility.x, -region.x, -knowledge_warning_score, 
         -knowledge_appropriate_score, -knowledge_general_score, -knowledge_helpful_score,
         -standardized_facility.y, -region.y, -status, -inclusive_status, -standardized_facility,
         -region, -practice_score, -attitude_acceptability_score, -attitude_genderroles_score,
         -attitude_general_score, -attitude_profroles_score)

# Convert to wide
scores_for_imputation_wide <- scores_for_imputation %>%
  pivot_wider(id_cols = participant_id_3, names_from = time_point, values_from = c(
    starts_with("knowledge"),
    starts_with("confidence"),
    starts_with("attitude"),
    starts_with("empathy"),
    starts_with("system")
  )
)

# Drop _1 for Mary's formatting
scores_for_imputation_wide <- scores_for_imputation_wide %>%
  rename_with(~str_replace(., "_1$", ""), .cols = ends_with("_1"))

# Join attendance data
attendance <- attendance_data %>%
  select("participant_id_3", "attendance_score_FUAT")

scores_for_imputation_wide <- left_join(scores_for_imputation_wide, attendance, by = c(
  "participant_id_3")) %>%
  rename(doseattendance = attendance_score_FUAT)

# merge necessary demographic data info
dem <- dem_data %>%
  select(-"status", -"status_binary", -"region", -"municipality", -"previous_training_factored",
         -"position_years_clean", -"standardized_facility")

scores_for_imputation_wide <- left_join(scores_for_imputation_wide, dem, by = c(
  "participant_id_3"))

# RENAME VARIABLES FOR MARY ----------------------------------------------------
# Knowledge
scores_for_imputation_wide <- scores_for_imputation_wide %>%
  rename_with(~ str_replace(., "ledge", ""), .cols = matches("knowledge"))

scores_for_imputation_wide <- scores_for_imputation_wide %>%
  rename(
    knowledge_overall = know_overall,
    knowledge_overall_2 = know_overall_2,
    knowledge_overall_3 = know_overall_3
  )
                
# Attitudes
scores_for_imputation_wide <- scores_for_imputation_wide %>%
  rename_with(~ str_replace(., "itudes", ""), .cols = matches("attitudes"))

# Confidence
scores_for_imputation_wide <- scores_for_imputation_wide %>%
  rename_with(~ str_replace(., "idence", ""), .cols = matches("confidence"))

scores_for_imputation_wide <- scores_for_imputation_wide %>%
  rename(
    confidence_score = conf_score,
    confidence_score_2 = conf_score_2,
    confidence_score_3 = conf_score_3
  )

# System support
scores_for_imputation_wide <- scores_for_imputation_wide %>%
  rename(
    syssup_15a = system_support_15a,
    syssup_15b = system_support_15b,
    syssup_15c = system_support_15c,
    syssup_15d = system_support_15d,
    syssup_15e = system_support_15e,
    syssup_15f = system_support_15f,
    syssup_15a_2 = system_support_15a_2,
    syssup_15b_2 = system_support_15b_2,
    syssup_15c_2 = system_support_15c_2,
    syssup_15d_2 = system_support_15d_2,
    syssup_15e_2 = system_support_15e_2,
    syssup_15f_2 = system_support_15f_2,
    syssup_15a_3 = system_support_15a_3,
    syssup_15b_3 = system_support_15b_3,
    syssup_15c_3 = system_support_15c_3,
    syssup_15d_3 = system_support_15d_3,
    syssup_15e_3 = system_support_15e_3,
    syssup_15f_3 = system_support_15f_3,
  )

# age_collapsed = agegrp
scores_for_imputation_wide <- scores_for_imputation_wide %>%
  rename(
    agegrp = age_collapsed
  )

# DATA BY DOMAIN  --------------------------------------------------------------
# attitude
attitude_data <- scores_for_imputation_wide %>%
  select("participant_id_3", "doseattendance", "agegrp", "age_groups", "position_groups",
         "sex_factored", starts_with("att"))

# knowledge
knowledge_data <- scores_for_imputation_wide %>%
  select("participant_id_3", "doseattendance", "agegrp", "age_groups", "position_groups",
         "sex_factored", starts_with("know"))

# confidence
confidence_data <- scores_for_imputation_wide %>%
  select("participant_id_3", "doseattendance", "agegrp", "age_groups", "position_groups",
         "sex_factored", starts_with("conf"))

# empathy
empathy_data <- scores_for_imputation_wide %>%
  select("participant_id_3", "doseattendance", "agegrp", "age_groups", "position_groups",
         "sex_factored", starts_with("empathy"))

# system support
syssup_data <- scores_for_imputation_wide %>%
  select("participant_id_3", "doseattendance", "agegrp", "age_groups", "position_groups",
         "sex_factored", starts_with("syssup"), starts_with("system"))

# Write score data to folder
path_to_scores_for_imputation <- paste(gbv_project_wd, "/data/clean/scores_for_imputation.RDS", sep = "")
saveRDS(scores_for_imputation_wide, file = path_to_scores_for_imputation)

write_dta(scores_for_imputation_wide, "/Users/susanglenn/Repositories/gbv-health-provider-study/data/clean/all data all timepoints.dta")

write_dta(attitude_data, "/Users/susanglenn/Repositories/gbv-health-provider-study/data/clean/attitude data all timepoints.dta")

write_dta(empathy_data, "/Users/susanglenn/Repositories/gbv-health-provider-study/data/clean/empathy data all timepoints.dta")

write_dta(knowledge_data, "/Users/susanglenn/Repositories/gbv-health-provider-study/data/clean/knowledge all timepoints.dta")

write_dta(syssup_data, "/Users/susanglenn/Repositories/gbv-health-provider-study/data/clean/syssup data all timepoints.dta")

write_dta(confidence_data, "/Users/susanglenn/Repositories/gbv-health-provider-study/data/clean/conf data all timepoints.dta")
