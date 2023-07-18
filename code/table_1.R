##################################################
## Project:
## Script purpose:
## Date: 
## Author: Jessica Dyer
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

#### Lint current file ####
style_file(paste(gbv_project_wd, "/code/data_cleaning.R", sep = ""))

# Load cleaned data
clean_data <- readRDS(path_to_clean_rds)

# Create table
filtered_data <- clean_data %>% filter(time_point == 1)
demographic_table <- table1(~ sex_factored + age_groups + position_groups, data = filtered_data)
demographic_table
