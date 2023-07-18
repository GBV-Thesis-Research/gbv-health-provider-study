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
#' This code modifies data based on key values for each variable in attitude vars.
#' If the key value is 5, the variable is scaled down by 1 (0-4). If the key value is 1,
#' the variable is reversed coded.

# Get column names matching 'attitudes'
att_vars <- names(data)[str_detect(names(data), "attitudes")]

# Get column names matching 'attitudes_12' from att_vars
att12_vars <- att_vars[str_detect(att_vars, "attitudes_12")]

# Remove 'attitudes_12' column names from att_vars
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

# Scoring (might only want to include knowledge here?)
key_vector <- as.vector(unlist(t(key_only)))
participant_ids <- answers[, 1]

scores <- psych::score.multiple.choice(key = key_vector, data = answers[, -1], score = FALSE, missing = TRUE, short = FALSE)
scores <- cbind(participant_ids, scores)
