##################################################
## Project: GBV Health provider study
## Script purpose: Imputed results
## Date: 5-3-24
## Author: Jessica Dyer
##################################################

# SETUP ------------------------------------------------------------------------
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
style_file(paste(gbv_project_wd, "/code/imputed_results.R", sep = ""))

# Import dta files
knowledge <- read_dta("data/clean/knowledge MI data.dta")
empathy <- read_dta("data/clean/empathy MI data.dta")
system_support <- read_dta("data/clean/system support MI data.dta")
confidence <-

# Import attitudes differently because its being weird 
attitudes <- tryCatch(
  read_dta(file.path(gbv_project_wd, "data", "clean", "attitude MI data.dta")),
  error = function(e) {
    print(paste("Error reading attitude MI data:", e$message))
    NULL
  }
)

# CREATE RESULTS TABLES --------------------------------------------------------
library(mice)
library(sandwich)
library(lmtest)  

model <- with(attitudes, lm(attitude_overall_3 ~ attitude_overall + attitude_overall_2 + sex_factored 
                            + factor(agegrp) + factor(position_groups) + doseattendance))

robust_se <- coeftest(model, vcov = vcovHC(model))
coefficients <- coef(model)

model
