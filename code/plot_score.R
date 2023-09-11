##################################################
## Project: GBV
## Script purpose: Plot for scores across 3 timepoints
## Date: 9-10-23
## Author: Susan Glenn
##################################################

#### WD SETUP ####
current_wd <- getwd()

if (endsWith(current_wd, "gbv-health-provider-study")) {
  gbv_project_wd <- current_wd
} else if (endsWith(current_wd, "/paper")) {
  gbv_project_wd <- str_remove(current_wd, "/paper")
} else {
  print("Got a WD that's not handled in the If-else ladder yet")
}
path_to_clean_rds_scores <- paste(gbv_project_wd, "/data/clean/gbv_data_scores.RDS", sep = "")

source(paste(gbv_project_wd, "/code/dependencies.R", sep = ""))

if (!file.exists(path_to_clean_rds_scores)) {
  source(paste(gbv_project_wd, "/code/score.R", sep = ""))
}

#### Lint current file ####
style_file(paste(gbv_project_wd, "/code/table_scores.R", sep = ""))

# Load cleaned data
path_to_clean_rds_scores <- paste(gbv_project_wd, "/data/clean/gbv_data_scores.RDS", sep = "")
clean_scores <- readRDS(path_to_clean_rds_scores)

# CREATE MEAN SCORES FOR EACH DOMAIN-------------------------------------------


# CREATE PLOT FOR SCORES ACROSS TIMEPOINTS ------------------------------------
ggplot(clean_data_scores, aes(x = Timepoint, y = Value1)) +
  geom_bar(stat = "identity", position = "dodge", fill = "blue", width = 0.7) +
  geom_bar(aes(x = Timepoint, y = Value2), stat = "identity", position = "dodge", fill = "red", width = 0.7) +
  labs(title = "Bar Chart with Three Timepoints", x = "Timepoint", y = "Values") +
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal()