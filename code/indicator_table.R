##################################################
## Project: GBV Study
## Script purpose: Create MEL indicator table for MEL report
## Date: 9-19-23
## Author: Susan Glenn
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

source(paste(gbv_project_wd, "/code/dependencies.R", sep = ""))
style_file(paste(gbv_project_wd, "/code/mel_report_analysis.R", sep = ""))


install.packages("kableExtra")
library(kableExtra)

# CREATE MEL TABLE  ------------------------------------------------------------
# Create indicator dataframe
indicators <- data.frame(
  Result = c("Outcome 4: Health providers who have participated in the ‘Responding 
  to GBV Learning Lab’ Curriculum demonstrate improved knowledge related to GBV",
             "Outcome 5: Health providers who have participated in the ‘Responding 
             to GBV Learning Lab’ have increased confidence in delivering survivor-centered 
             care", "Outcome 7: Increase in clients identified who are victims of GBV"),
  Indicator = c("Indicator 4: % of health facilities demonstrate improvement in staff knowledge, 
                attitude, and empathy in responding to GBV", "Indicator 5: % of health facilities 
                that demonstrate improvement in health provider confidence in responding to GBV",
                "Indicator 7: % increase in GBV clients identified"),
  Frequency_of_Collection = c("Three time points: pre-training, post-intensive training, and post-intervention",
                              "Three time points: pre-training, post-intensive training, and post-intervention",
                              "Two timepoints: pre-training and post-intervention*
                                *This measure can be used to compare to health facility reporting 
                                (which is the official MEL data source) on clients identified"),
  Unit_of_Measure = c("Average composite score of knowledge, attitude, and empathy domains by health facility cohort pre- to post-intervention",
                      "Average composite score of the confidence, system support, and professional role domains  by health facility cohort pre to post-intervention",
                      "Percentage compared to baseline"),
  Value = c("80% of health facilities", "80% of health facilities", "50% increase")
)

# Change column names
names(indicators)[3] <- "Frequency of Collection"
names(indicators)[4] <- "Unit of Measure"

# Create indicator table
indicator_table <- flextable(indicators)
indicator_table