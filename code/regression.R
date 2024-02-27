##################################################
## Project: GBV Health provider study
## Script purpose: Regression
## Date: 2-15-24
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

source(paste(gbv_project_wd, "/code/analysis_prep.R", sep = ""))
style_file(paste(gbv_project_wd, "/code/analysis_prep.R", sep = ""))

analysis_df_fp_wide <- paste(gbv_project_wd, "/data/clean/analysis_data_wide.RDS", sep = "")
df_wide <- readRDS(analysis_df_fp_wide)

analysis_df_fp_long <- paste(gbv_project_wd, "/data/clean/analysis_data_long.RDS", sep = "")
df_long <- readRDS(analysis_df_fp_long)

## Regression
# set reference groups
df_wide$position_groups <- relevel(df_wide$position_groups, ref = "Medical doctor")
df_wide$age_groups <- relevel(df_wide$age_groups, ref = "Less than 25 years old")

# Conduct bivariate analysis for additive effects
lm_sex <- lm(knowledge_overall_3 ~ sex_factored, data = df_wide)
summ(lm_sex)
confint(lm_sex)

lm_position <- lm(knowledge_overall_3 ~ position_groups, data = df_wide)
summ(lm_position)
confint(lm_position)

lm_age <- lm(knowledge_overall_3 ~ age_groups, data = df_wide)
summ(lm_age)
confint(lm_age)

lm_attendance <- lm(knowledge_overall_3 ~ attendance_score_FUAT, data = df_wide)
summ(lm_attendance)
confint(lm_attendance)

# linear regression, adjusting for baseline scores only
knowledge_reg <- lm(knowledge_overall_3 ~ knowledge_overall_2, data = df_wide)
summ(knowledge_reg)
confint(knowledge_reg)

att_reg <- lm(attitude_overall_3 ~ attitude_overall_2, data = df_wide)
summ(att_reg)
confint(att_reg)

conf_reg <- lm(confidence_score_3 ~ confidence_score_2 + confidence_score_1, data = df_wide)
summ(conf_reg)
confint(conf_reg)

emp_reg <- lm(empathy_score_3 ~ empathy_score_2 + empathy_score_1, data = df_wide)
summ(emp_reg)
confint(emp_reg)

sys_reg <- lm(system_support_score_3 ~ system_support_score_2 + system_support_score_1, data = df_wide)
summ(sys_reg)
confint(sys_reg)

# linear regression, adjusting for characteristics
example_regession <- lm(knowledge_overall_3 ~ sex_factored + attendance_score_FUAT +
                          age_groups + position_groups + knowledge_overall_1, data = df_wide)
summ(example_regession)

example_regession <- lm(knowledge_overall_3 ~ sex_factored + attendance_score_FUAT +
                          age_groups + position_groups + knowledge_overall_1 +
                          knowledge_overall_2, data = df_wide)
summ(example_regession)

# logistic regression - baseline to endline
knowledge_logreg <- glm(know_improve_overall ~ attendance_score_FUAT + factor(sex_factored) + 
                           + factor(age_binary) + factor(position_groups), data = analysis_wide, family = binomial)
exp(cbind(OR = coef(knowledge_logreg), confint(knowledge_logreg)))

att_logreg <- glm(att_improve_overall ~ attendance_score_FUAT + sex_factored + 
                    + age_binary + position_groups, data = analysis_wide, family = binomial)
exp(cbind(OR = coef(att_logreg), confint(att_logreg)))

conf_logreg <- glm(conf_improve_overall ~ attendance_score_FUAT + sex_factored + 
                    + age_binary + position_groups, data = analysis_wide, family = binomial)
exp(cbind(OR = coef(conf_logreg), confint(conf_logreg)))

emp_logreg <- glm(emp_improve_overall ~ attendance_score_FUAT + sex_factored + 
                     + age_binary + position_groups, data = analysis_wide, family = binomial)
exp(cbind(OR = coef(emp_logreg), confint(emp_logreg)))

syssup_logreg <- glm(syssup_improve_overall ~ attendance_score_FUAT + sex_factored + 
                    + age_binary + position_groups, data = analysis_wide, family = binomial)
exp(cbind(OR = coef(syssup_logreg), confint(syssup_logreg)))

# logistic regression - midline to endline
know_midend_logreg <- glm(know_improve_midend ~ attendance_score_FUAT + factor(sex_factored) + 
                          + factor(age_binary) + factor(position_groups), data = analysis_wide, family = binomial)
exp(cbind(OR = coef(know_midend_logreg), confint(know_midend_logreg)))

att_midend_logreg <- glm(att_improve_midend  ~ attendance_score_FUAT + sex_factored + 
                    + age_binary + position_groups, data = analysis_wide, family = binomial)
exp(cbind(OR = coef(att_midend_logreg), confint(att_midend_logreg)))

conf_midend_logreg <- glm(conf_improve_midend ~ attendance_score_FUAT + sex_factored + 
                     + age_binary + position_groups, data = analysis_wide, family = binomial)
exp(cbind(OR = coef(conf_midend_logreg), confint(conf_midend_logreg)))

emp_midend_logreg <- glm(emp_improve_midend ~ attendance_score_FUAT + sex_factored + 
                    + age_binary + position_groups, data = analysis_wide, family = binomial)
exp(cbind(OR = coef(emp_midend_logreg), confint(emp_midend_logreg)))

syssup_midend_logreg <- glm(syssup_improve_midend ~ attendance_score_FUAT + sex_factored + 
                       + age_binary + position_groups, data = analysis_wide, family = binomial)
exp(cbind(OR = coef(syssup_midend_logreg), confint(syssup_midend_logreg)))