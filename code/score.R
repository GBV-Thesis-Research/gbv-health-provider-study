##################################################
## Project:
## Script purpose:
## Date:
## Author: Jessica Dyer
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

style_file(paste(gbv_project_wd, "/code/score.R", sep = ""))

source(paste(gbv_project_wd, "/code/participant_id_cleaning.R", sep = ""))
path_to_clean_rds <- paste(gbv_project_wd, "/data/clean/gbv_data_clean.RDS", sep = "")
clean_data <- readRDS(path_to_clean_rds)

answers <- clean_data %>%
  select(participant_id_3, matches("knowledge|attitudes|system_support|confidence|empathy|practices"))


key_only <- key %>%
  select(matches("knowledge|attitudes|system_support|confidence|empathy|practices"))

# ATTITUDES  -------------------------------------------------------------------
recode_likert_according_to_key <- function(variable_names_to_recode) {
  #' This function is designed to modify attitude variable scores related to Gender-Based Violence (GBV)
  #' based on a provided key. The key indicates whether a positive answer is represented by 1 or 5
  #' for each question in the "attitude vars" set. The function standardizes the scores to ensure higher
  #' values consistently indicate more positive attitudes towards GBV.
  clean_data <- clean_data %>%
    mutate(across(all_of(variable_names_to_recode), ~ case_when(
      key_only[[cur_column()]] == 5 ~ . - 1,
      key_only[[cur_column()]] == 1 ~ abs(. + 3 - 8),
      TRUE ~ .
    )))
  return(clean_data)
}

# Get column names matching 'attitudes'
att_vars <- names(data)[str_detect(names(data), "attitudes")]

# Get column names matching 'attitudes_12' from att_vars
att12_vars <- att_vars[str_detect(att_vars, "attitudes_12")]

#' Remove the 'attitudes_12' column from the provided data frame, att_vars,
#' as it has 4 possible answers compared to 5 in the other attitude domain questions,
#' necessitating a separate evaluation for Question 12.
att_vars <- att_vars[!str_detect(att_vars, "attitudes_12")]

# Based on the key, readjust values based on desired answer (1 or 5), but
# subtracting 1 when the desired answer is 5. When the desired answer is 1, add 3
# to the value and then subtract 8 so that desired behavior gets the higher score.
clean_data <- recode_likert_according_to_key(att_vars)

# Subtract 1 from each answer for att12_vars, then reassigns the value 3 ("I don't know")
# to 1 ("sometimes it is acceptable") for each column in att12_vars.
clean_data <- clean_data %>%
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
#' (Cory said 3 = correct answer? Ask if this was a mistake / run the numbers to check)

#' Get column names matching 'confidence'
conf_vars <- names(clean_data)[str_detect(names(data), "confidence")]

# Subtracts 1 from each 'confidence' score
clean_data <- clean_data %>%
  mutate(across(all_of(conf_vars), ~ . - 1))

# EMPATHY -------------------------------------------------------------------
#' Modify data based on key values
#'
#' Modify empathy variable scores related to Gender-Based Violence (GBV)
#' based on a provided key. The key indicates whether a positive answer is represented by 1 or 5
#' for each question in the "empathy vars" set. The function standardizes the scores to ensure higher
#' values consistently indicate more empathy towards those effected by GBV.

# Get column names matching 'empathy'
empathy_vars <- names(data)[str_detect(names(data), "empathy")]
clean_data <- recode_likert_according_to_key(empathy_vars)

# PRACTICES --------------------------------------------------------------------

# Get column names matching practices_clean_19
pract19_clean_vars <- names(clean_data)[str_detect(names(clean_data), "practices_clean_19")]

# Recode practices as 0/1 (no/yes)
clean_data <- clean_data %>%
  mutate(
    across(all_of(pract19_clean_vars), ~ ifelse(. == 2, 0, .))
  )

# CREATE SUB-DOMAINS ---------------------------------------------------------------
# The pre/post test had 5 main domains, with knowledge and attitudes having sub-domains.
# The code below creates those sub-domains for scoring purposes.

# Create knowledge sub-domains (4): general knowledge, warning signs, appropriate ways
# to ask about GBV, and helpful responses to support a woman subjected to GBV
know_vars_general <- names(clean_data)[str_detect(names(data), "knowledge_7")]
know_vars_warning <- names(clean_data)[str_detect(names(data), "knowledge_8")]
know_vars_appropriate <- names(clean_data)[str_detect(names(data), "knowledge_9")]
know_vars_helpful <- names(clean_data)[str_detect(names(data), "knowledge_10")]

# Create attitudes sub-domains (4): general attitudes towards GBV and the health provider role,
# acceptability for a man to hit his partner, attitudes towards gender roles,
# attitudes towards professional roles
att_vars_general <- names(clean_data)[str_detect(names(data), "attitudes_11")]
att_vars_acceptability <- names(clean_data)[str_detect(names(data), "attitudes_12")]
att_vars_genderroles <- names(clean_data)[str_detect(names(data), "attitudes_13")]
att_vars_profroles <- names(clean_data)[str_detect(names(data), "attitudes_14")]

# SUM SCORES FOR EACH DOMAIN--------------------------------------------------
# Using the key, score knowledge and system support variables

knowledge_sys_support_key <- key %>%
  select(matches("knowledge|system_support"))

knowledge_sys_support_answers <- clean_data %>%
  select(participant_id_3, time_point, standardized_facility, region, CHC_catchment, matches("knowledge|system_support"))

key_vector <- as.vector(unlist(t(knowledge_sys_support_key)))

participant_ids <- knowledge_sys_support_answers[, 1:5]

knowledge_sys_support_scores_scored <-
  psych::score.multiple.choice(
    key = key_vector,
    data = knowledge_sys_support_answers[, -(1:5)],
    score = FALSE, missing = FALSE, short = TRUE
  )

knowledge_sys_support_scores_raw <- cbind(participant_ids, knowledge_sys_support_scores_scored)

knowledge_sys_support_scores <- knowledge_sys_support_scores_raw %>%
  mutate(
    knowledge_general_score = (rowSums(select(., all_of(matches("knowledge_7"))), na.rm = TRUE) / 19) * 100,
    knowledge_warning_score = (rowSums(select(., all_of(matches("knowledge_8"))), na.rm = TRUE) / 9) * 100,
    knowledge_appropriate_score = (rowSums(select(., all_of(matches("knowledge_9"))), na.rm = TRUE) / 5) * 100,
    knowledge_helpful_score = (rowSums(select(., all_of(matches("knowledge_10"))), na.rm = TRUE) / 10) * 100,
    system_support_score = (rowSums(select(., all_of(matches("system_support"))), na.rm = TRUE) / 6) * 100,
  ) %>%
  select(
    participant_id_3, time_point, CHC_catchment, standardized_facility, region, knowledge_general_score, knowledge_warning_score,
    knowledge_appropriate_score, knowledge_helpful_score, system_support_score
  )

# Calculate scores by summing up variables for each row and bind participant IDs and timepoint
scores <- clean_data %>%
  mutate(
    attitude_general_score = (rowSums(select(., all_of(matches("attitudes_11"))), na.rm = TRUE) / 40) * 100,
    attitude_acceptability_score = (rowSums(select(., all_of(matches("attitudes_12"))), na.rm = TRUE) / 14) * 100,
    attitude_genderroles_score = (rowSums(select(., all_of(matches("attitudes_13"))), na.rm = TRUE) / 24) * 100,
    attitude_profroles_score = (rowSums(select(., all_of(matches("attitudes_14"))), na.rm = TRUE) / 24) * 100,
    empathy_score = (rowSums(select(., all_of(empathy_vars)), na.rm = TRUE) / 64) * 100,
    confidence_score = (rowSums(select(., all_of(conf_vars)), na.rm = TRUE) / 40) * 100,
    practice_score = (rowSums(select(., all_of(pract19_clean_vars)), na.rm = FALSE) / 9) * 100,
  ) %>%
  select(
    participant_id_3, status, inclusive_status, standardized_facility, region, time_point, attitude_general_score, attitude_acceptability_score,
    attitude_genderroles_score, attitude_profroles_score, empathy_score,
    confidence_score, practice_score
  )

# Merge all scores into one data frame
merged_scores <- inner_join(knowledge_sys_support_scores, scores, by = c(
  "participant_id_3", "time_point", "standardized_facility", "region"
))

# CREATE NEW VARIABLES BASED ON MEL PLAN ---------------------------------------
# Create new pre-score variable for outcome 4, including the knowledge, attitude,
# and empathy domains
merged_scores <- merged_scores %>%
  mutate(outcome4_score = ifelse(status == "All three",
    ((knowledge_general_score + knowledge_warning_score +
      knowledge_appropriate_score + knowledge_helpful_score +
      attitude_general_score + attitude_acceptability_score +
      attitude_genderroles_score + attitude_profroles_score +
      empathy_score) / 900) * 100,
    NA
  ))

# Create new pre-score variable for outcome 4, including the confidence, system support,
# and professional role domains <- have to ask Xylia  about the professional role one here
# as its in attitude domain, not its own domain
merged_scores <- merged_scores %>%
  mutate(outcome5_score = ifelse(status == "All three",
    ((confidence_score + system_support_score) / 200) * 100,
    NA
  ))

# Write score data to folder
path_to_clean_rds_scores <- paste(gbv_project_wd, "/data/clean/gbv_data_scores.RDS", sep = "")
saveRDS(merged_scores, file = path_to_clean_rds_scores)
