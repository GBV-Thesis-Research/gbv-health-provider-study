##################################################
## Project:
## Script purpose:
## Date:
## Author: Jessica Dyer
##################################################

#### WD SETUP ####
current_wd <- getwd()
#### Lint current file ####

if (endsWith(current_wd, "gbv-health-provider-study")) {
  gbv_project_wd <- current_wd
} else if (endsWith(current_wd, "/paper")) {
  gbv_project_wd <- str_remove(current_wd, "/paper")
} else {
  print("Got a WD that's not handled in the If-else ladder yet")
}

source(paste(gbv_project_wd, "/code/data_cleaning.R", sep = ""))
source(paste(gbv_project_wd, "/code/dependencies.R", sep = ""))

style_file(paste(gbv_project_wd, "/code/score.R", sep = ""))

clean_data <- readRDS(path_to_clean_rds)

answers <- clean_data %>%
  select(participant_id, matches("knowledge|attitudes|system_support|confidence|empathy|practices"))

key_only <- key %>%
  select(matches("knowledge|attitudes|system_support|confidence|empathy|practices"))

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

clean_data <- recode_likert_according_to_key(att_vars)

# Reassigns the value 3 to 1 for each column in att12_vars.
clean_data <- clean_data %>%
  mutate(
    across(all_of(att12_vars), ~ . - 1),
    across(all_of(att12_vars), ~ ifelse(. == 3, 1, .))
  )

#' CONFIDENCE
#' Rescale to a 0-4 scale
#'
#' This code modified data based on key values for each variable in attitude vars.
#' All items are positively worded, so we moved scale down by 1, with highest confidence
#' score being 4, and low confidence being 0.
#' (Cory said 3 = correct answer? Ask if this was a mistake / run the numbers to check)

#' Get column names matching 'confidence'
conf_vars <- names(clean_data)[str_detect(names(data), "confidence")]

# Subtracts 1 from each 'confidence' score
clean_data <- clean_data %>% mutate(across(all_of(conf_vars), ~ . - 1))

#' Get column names matching 'empathy'
emp_vars <- names(data)[str_detect(names(data), "empathy")]
clean_data <- recode_likert_according_to_key(emp_vars)

#' PRACTICES
#' Clean up data
#'
#' The code below addresses a skip logic issue between questions 18 and 19. In the survey,
#' respondents who answered "no" or "NA" to question 18 were not supposed to answer question 19.
#' However, due to inconsistencies, many respondents who answered "no" or "NA" on question 18
#' still answered question 19. To resolve this, the code assigns "NAs" to all variables in
#' question 19 for respondents who answered "no" or "NA" in question 18, thereby cleaning up the data.

# Get column names matching 'practices'
pract_vars <- names(data)[str_detect(names(data), "practices")]

# Look for 'bad' data -- responding to q19 when q18 is not answered as 'yes' (as it uses skip logic)
table(clean_data$practices_18, data$practices_19a)
sum(is.na(data$practices_19a))

# Recode question 18 to be 1 = yes, they had identified a woman suffering domestic violence
# in the past month, 0 = no.
clean_data <- clean_data %>%
  mutate(practices_18 = case_when(
    practices_18 %in% c(2, 3) ~ 0,
    TRUE ~ practices_18
  ))

# Recode question 19 to be NA for those who had not identified a woman suffering domestic
# violence in the past month in question 18
clean_data <- clean_data %>%
  mutate(across(starts_with("practices_19"), ~ case_when(practices_18 != 1 ~ NA, TRUE ~ .)))

# Recode question 19 to be NA for those who had not answered question 18 (where 18 = NA)
clean_data <- clean_data %>%
  mutate(across(starts_with("practices_19"), ~ case_when(
    is.na(practices_18) ~ NA,
    TRUE ~ .
  )))

# Recode question 19 vars to be 1 = correct answer and 0 = incorrect answer.
clean_data <- clean_data %>%
  mutate(across(starts_with("practices_19"), ~ case_when(. != 1 ~ 0, TRUE ~ .)))

#' SUM SCORES FOR EACH DOMAIN
#' Using the key, score knowledge and system support variables.
knowledge_sys_support_key <- key %>%
  select(matches("knowledge|system_support"))
knowledge_sys_support_answers <- clean_data %>%
  select(participant_id, time_point, matches("knowledge|system_support"))
key_vector <- as.vector(unlist(t(knowledge_sys_support_key)))
participant_ids <- knowledge_sys_support_answers[, 1:2]

knowledge_sys_support_scores_scored <- psych::score.multiple.choice(key = key_vector, data = knowledge_sys_support_answers[, -(1:2)], score = FALSE, missing = FALSE, short = TRUE)
knowledge_sys_support_scores_raw <- cbind(participant_ids, knowledge_sys_support_scores_scored)

knowledge_sys_support_scores <- knowledge_sys_support_scores_raw %>%
  mutate(
    knowledge_score = rowSums(select(., all_of(matches("knowledge"))), na.rm = TRUE),
    system_support_score = rowSums(select(., all_of(matches("system_support"))), na.rm = TRUE)
  ) %>%
  select(participant_id, time_point, knowledge_score, system_support_score)

#' Sum attitudes scores and bind participant IDs and timepoint
attitudes_to_sum <- c(att_vars, att12_vars)

# Calculate attitude scores by summing up attitude variables for each row
scores <- clean_data %>%
  mutate(
    attitude_score = rowSums(select(., all_of(attitudes_to_sum)), na.rm = TRUE),
    empathy_score = rowSums(select(., all_of(emp_vars)), na.rm = TRUE),
    confidence_score = rowSums(select(., all_of(conf_vars)), na.rm = TRUE),
    practice_score = rowSums(select(., all_of(pract_vars)), na.rm = TRUE)
  ) %>%
  select(participant_id, time_point, attitude_score, empathy_score, confidence_score, practice_score)

merged_scores <- inner_join(knowledge_sys_support_scores, scores, by = c("participant_id", "time_point"))
