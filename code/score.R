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

# Add in here the recoding that Cory did for each of the scales and document
# well.
# Knowledge I think is fine to not recode, because the scoring here should be
# correct. But spot check that they were scored correctly.
# Then do attitudes, looks like the scale was reversed for the key? But not for attitudes12?
# Figure out what is being done there and let's do it better!
# Just start with knowledge and we'll go from there. Maybe we just make a new
# key that makes sense.

key_vector <- as.vector(unlist(t(key_only)))
participant_ids <- answers[, 1]

scores <- psych::score.multiple.choice(key = key_vector, data = answers[, -1], score = FALSE, missing = TRUE, short = FALSE)
scores <- cbind(participant_ids, scores)
