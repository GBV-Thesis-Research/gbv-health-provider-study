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

#' ATTITUDES
#' Modify data based on key values
#'
#' This code modifies data based on key values for each variable in attitude vars. The
#' questions were phrased in such a way that the desired response was either 1 or 5,
#' depending on the question. Given that higher scores indicate the desired attitudes
#' toward GBV, we wanted all desired answers to be scored as t he highest possible score (4).
#' If the key value is 5, the variable is scaled down by 1 (0-4). If the key value is 1,
#' the variable is reversed coded. After subtracting 1 from all scores, score 3 (response "I don't know")
#' was re-scored to a 1 (the same score as "sometimes acceptable")

# Get column names matching 'attitudes'
att_vars <- names(data)[str_detect(names(data), "attitudes")]

# Get column names matching 'attitudes_12' from att_vars
att12_vars <- att_vars[str_detect(att_vars, "attitudes_12")]

# Remove 'attitudes_12' column names from att_vars.
# Question 12 was removed from att_vars as it only had 4 possible answers, compared to
# 5 in the rest of the attitude domain questions. Question 12 will be evaluated separately.
att_vars <- att_vars[!str_detect(att_vars, "attitudes_12")]

clean_data <- clean_data %>%
  mutate(across(all_of(att_vars), ~ case_when(
    key_only[[cur_column()]] == 5 ~ . - 1,
    key_only[[cur_column()]] == 1 ~ abs(. + 3 - 8),
    TRUE ~ .
  )))

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

#' EMPATHY
#' Modify data based on key values
#'
#' This code modifies data based on key values for each variable in empathy vars. The
#' questions were phrased in such a way that the desired response was either 1 or 5,
#' depending on the question. Given that higher scores indicate the desired empathy,
#' we wanted all desired answers to be scored as the highest possible score (4).
#' If the key value is 5, the variable is scaled down by 1 (0-4). If the key value is 1,
#' the variable is reversed coded.

#' Get column names matching 'empathy'
emp_vars <- names(data)[str_detect(names(data), "empathy")]

# Reassigns the value 3 to 1 for each column in att12_vars.
clean_data <- clean_data %>%
  mutate(across(all_of(emp_vars), ~ case_when(
    key_only[[cur_column()]] == 5 ~ . - 1,
    key_only[[cur_column()]] == 1 ~ abs(. + 3 - 8),
    TRUE ~ .
  )))

#' PRACTICES
#' Clean up data
#'
#' This code modifies data for each variable in practices vars. Question 18 used skip logic,
#' where those who answered "no" or "NA" to question 18 should not have answered question 19.
#' However, most people who answered "no" or "NA" on question 18 still answered question 19.
#' The code below cleans up the data, assigning "NAs" to all variables in question 19 for those
#' who answered "no" or "NA" in question 18.

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
#' Using the key, automatically score knowledge and system support variables.
knowledge_sys_support_key <- key %>%
  select(matches("knowledge|system_support"))
knowledge_sys_support_answers <- clean_data %>%
  select(participant_id, time_point, matches("knowledge|system_support"))
key_vector <- as.vector(unlist(t(knowledge_sys_support_key)))
participant_ids <- knowledge_sys_support_answers[, 1:2]

knowledge_sys_support_scores <- psych::score.multiple.choice(key = key_vector, data = knowledge_sys_support_answers[, -(1:2)], score = FALSE, missing = FALSE, short = TRUE)
knowledge_sys_support_scores <- cbind(participant_ids, scores)

#' Sum attitudes scores and bind participant IDs and timepoint
attitudes_to_sum <- c(att_vars, att12_vars)
attitude_scores <- rowSums(clean_data[, attitudes_to_sum])
attitude_scores_df <- data.frame(rowSums = attitude_scores)
attitude_scores_df <- cbind(clean_data$participant_id, clean_data$time_point, attitude_scores_df)
attitude_scores_df

#' Sum empathy scores
empathy_scores <- rowSums(clean_data[, emp_vars])
empathy_scores_df <- data.frame(rowsums = empathy_scores)
empathy_scores_df <- cbind(clean_data$participant_id, clean_data$time_point, empathy_scores_df)
empathy_scores_df

#' Sum confidence scores
confidence_scores <- rowSums(clean_data[, conf_vars])
confidence_scores_df <- data.frame(rowsums = confidence_scores)
confidence_scores_df <- cbind(clean_data$participant_id, clean_data$time_point, confidence_scores_df)
confidence_scores_df

#' Sum practices scores - only summing question 19, not question 18.
practice_scores <- rowSums(clean_data[, pract_vars19])
practice_scores_df <- data.frame(rowSums = practice_scores)
practice_scores_df <- cbind(clean_data$participant_id, clean_data$time_point, practice_scores_df)
practice_scores_df