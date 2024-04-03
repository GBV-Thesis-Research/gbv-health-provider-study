##################################################
## Project: GBV Health provider study
## Script purpose: Imputation for Attitudes
## Date: 3-27-24
## Author: Susan Glenn
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

source(paste(gbv_project_wd, "/code/imputation_prep.R", sep = ""))
path_to_clean_analysis_data_long_imp <- paste(gbv_project_wd, "/data/clean/analysis_data_long_imputation.RDS", sep = "")
imp_data <- readRDS(path_to_clean_analysis_data_long_imp)

# Create attitudes dataset, drop unnecessary data, and pivot wide
att <- imp_data %>%
  select(-3, -5:-8,-10,-11,-14,-21:-62, -92:-123) %>%
  pivot_wider(id_cols = participant_id_3, names_from = time_point, values_from = -participant_id_3) %>%
  select(-2:-4, -9:-10, -12:-13, -15:-16, -18:-19, -21:-22, -24:-31, -33:-34)

# Identify missing data
na_summary <- colSums(is.na(att))
variables_with_na <- names(na_summary[na_summary > 0])

# Identify passive variables - overall scores
meth <- make.method(att)
meth["attitude_overall_1"] <- "~I(attitudes_11a_1 + attitudes_11b_1 + attitudes_11c_1 +
attitudes_11d_1 + attitudes_11e_1 + attitudes_11f_1 + attitudes_11g_1 + attitudes_11h_1 + 
attitudes_11i_1 + attitudes_11j_1 + attitudes_12a_1 + attitudes_12b_1 + attitugit des_12c_1 + 
attitudes_12d_1 + attitudes_12e_1 + attitudes_12f_1 + attitudes_12g_1 + attitudes_13a_1 + 
attitudes_13b_1 + attitudes_13c_1 + attitudes_13d_1 + attitudes_13e_1 + attitudes_13f_1 + 
attitudes_14a_1 + attitudes_14b_1 + attitudes_14c_1 + attitudes_14d_1 + attitudes_14e_1 +
attitudes_14f_1)" 

meth["attitude_overall_2"] <- "~I(attitudes_11a_2 + attitudes_11b_2 + attitudes_11c_2 +
attitudes_11d_2 + attitudes_11e_2 + attitudes_11f_2 + attitudes_11g_2 + attitudes_11h_2 + 
attitudes_11i_2 + attitudes_11j_2 + attitudes_12a_2 + attitudes_12b_2 + attitudes_12c_2 + 
attitudes_12d_2 + attitudes_12e_2 + attitudes_12f_2 + attitudes_12g_2 + attitudes_13a_2 + 
attitudes_13b_2 + attitudes_13c_2 + attitudes_13d_2 + attitudes_13e_2 + attitudes_13f_2 + 
attitudes_14a_2 + attitudes_14b_2 + attitudes_14c_2 + attitudes_14d_2 + attitudes_14e_2 +
attitudes_14f_2)" 

meth["attitude_overall_3"] <- "~I(attitudes_11a_3 + attitudes_11b_3 + attitudes_11c_3 +
attitudes_11d_3 + attitudes_11e_3 + attitudes_11f_3 + attitudes_11g_3 + attitudes_11h_3 + 
attitudes_11i_3 + attitudes_11j_3 + attitudes_12a_3 + attitudes_12b_3 + attitudes_12c_3 + 
attitudes_12d_3 + attitudes_12e_3 + attitudes_12f_3 + attitudes_12g_3 + attitudes_13a_3 + 
attitudes_13b_3 + attitudes_13c_3 + attitudes_13d_3 + attitudes_13e_3 + attitudes_13f_3 + 
attitudes_14a_3 + attitudes_14b_3 + attitudes_14c_3 + attitudes_14d_3 + attitudes_14e_3 +
attitudes_14f_3)" 

meth["age_grp"] <- "~I(attitudes_11a_3 + attitudes_11b_3 + attitudes_11c_3 +
attitudes_11d_3 + attitudes_11e_3 + attitudes_11f_3 + attitudes_11g_3 + attitudes_11h_3 + 
attitudes_11i_3 + attitudes_11j_3 + attitudes_12a_3 + attitudes_12b_3 + attitudes_12c_3 + 
attitudes_12d_3 + attitudes_12e_3 + attitudes_12f_3 + attitudes_12g_3 + attitudes_13a_3 + 
attitudes_13b_3 + attitudes_13c_3 + attitudes_13d_3 + attitudes_13e_3 + attitudes_13f_3 + 
attitudes_14a_3 + attitudes_14b_3 + attitudes_14c_3 + attitudes_14d_3 + attitudes_14e_3 +
attitudes_14f_3)" 

# Include passive variables in prediction matrix
pred <- make.predictorMatrix(att)

pred[c("attitude_overall_1"), c("attitudes_11a_1", "attitudes_11b_1", "attitudes_11c_1", "attitudes_11d_1",
       "attitudes_11e_1", "attitudes_11f_1", "attitudes_11g_1", "attitudes_11h_1", 
       "attitudes_11i_1", "attitudes_11j_1", "attitudes_12a_1", "attitudes_12b_1",
       "attitudes_12c_1", "attitudes_12d_1", "attitudes_12e_1", "attitudes_12f_1", 
       "attitudes_12g_1", "attitudes_13a_1", "attitudes_13b_1", "attitudes_13c_1", 
       "attitudes_13d_1", "attitudes_13e_1", "attitudes_13f_1", "attitudes_14a_1",
       "attitudes_14b_1", "attitudes_14c_1", "attitudes_14d_1", "attitudes_14e_1",
       "attitudes_14f_1")] <- 0

pred[c("attitude_overall_1"), c("attitudes_11a_2", "attitudes_11b_2", "attitudes_11c_2", "attitudes_11d_2",
       "attitudes_11e_2", "attitudes_11f_2", "attitudes_11g_2", "attitudes_11h_2", 
       "attitudes_11i_2", "attitudes_11j_2", "attitudes_12a_2", "attitudes_12b_2",
       "attitudes_12c_2", "attitudes_12d_2", "attitudes_12e_2", "attitudes_12f_2", 
       "attitudes_12g_2", "attitudes_13a_2", "attitudes_13b_2", "attitudes_13c_2", 
       "attitudes_13d_2", "attitudes_13e_2", "attitudes_13f_2", "attitudes_14a_2",
       "attitudes_14b_2", "attitudes_14c_2", "attitudes_14d_2", "attitudes_14e_2",
       "attitudes_14f_2")] <- 0

pred[c("attitude_overall_3"), c("attitudes_11a_3", "attitudes_11b_3", "attitudes_11c_3", "attitudes_11d_3",
       "attitudes_11e_3", "attitudes_11f_3", "attitudes_11g_3", "attitudes_11h_3", 
       "attitudes_11i_3", "attitudes_11j_3", "attitudes_12a_3", "attitudes_12b_3",
       "attitudes_12c_3", "attitudes_12d_3", "attitudes_12e_3", "attitudes_12f_3", 
       "attitudes_12g_3", "attitudes_13a_3", "attitudes_13b_3", "attitudes_13c_3", 
       "attitudes_13d_3", "attitudes_13e_3", "attitudes_13f_3", "attitudes_14a_3",
       "attitudes_14b_3", "attitudes_14c_3", "attitudes_14d_3", "attitudes_14e_3",
       "attitudes_14f_3")] <- 0

pred

att_imp <- mice(att, meth = meth, pred = pred, maxit=20,
                print = FALSE, seed = 382670)

# Run diagnostics on att_imp
plot(att_imp)

# regression model for each imputed data set, using regularization to help with separation
model1 <- with(att_imp, lm(attitude_overall_1 ~ age_collapsed_1 + position_groups_1))
model1

# pool results of regression from each imputed data set
pooled_model <- summary(pool(model1), conf.int = TRUE)
pooled_model
