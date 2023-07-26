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

#' Attitudes
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

#' Confidence
#' Rescale to a 0-4 scale
#'
#' This code modified data based on key values for each variable in attitude vars.
#' All items are positively worded, so we moved scale down by 1, with highest confidence
#' score being 4, and low confidence being 0.
#' (Cory said 3 = correct answer? Ask if this was a mistake / run the numbers to check)

conf_vars <- names(clean_data)[str_detect(names(data), "confidence")]

clean_data <- clean_data %>% mutate(across(all_of(conf_vars), ~ case_when(
  conf_vars = 1:5 ~ . - 1)))

#' Empathy
#' Modify data based on key values
#'
#' 


# Scoring (might only want to include knowledge & system support here?)
knowledge_sys_support_key <- key %>%
  select(matches("knowledge|system_support"))
knowledge_sys_support_answers <- clean_data %>%
  select(participant_id, time_point, matches("knowledge|system_support"))
key_vector <- as.vector(unlist(t(knowledge_sys_support_key)))
participant_ids <- knowledge_sys_support_answers[, 1:2]

scores <- psych::score.multiple.choice(key = key_vector, data = knowledge_sys_support_answers[, -(1:2)], score = FALSE, missing = FALSE, short = TRUE)
scores <- cbind(participant_ids, scores)
