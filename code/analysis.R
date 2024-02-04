##################################################
## Project: GBV Health provider study
## Script purpose: Data analysis
## Date: 1-13-24
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

analysis_df_fp_wide <- paste(gbv_project_wd, "/data/clean/analysis_data_wide.RDS", sep = "")
df_wide <- readRDS(analysis_df_fp_wide)

analysis_df_fp_long <- paste(gbv_project_wd, "/data/clean/analysis_data_long.RDS", sep = "")
df_long <- readRDS(analysis_df_fp_long)

comparison_tbl_1 <-
  df_long %>%
  filter(time_point %in% c(1, 3)) %>%
  select(time_point, knowledge_overall, attitude_overall, system_support_score, 
         confidence_score, empathy_score, practice_score) %>%
  tbl_summary(by=time_point, type = list(system_support_score ~ "continuous", 
                                         practice_score ~ "continuous")) %>% 
  add_n() %>% 
  add_p()

## Regression
#load rigr
library(rigr)

# Check distribution
hist(df_wide$knowledge_overall_1, freq = TRUE)
hist(df_wide$knowledge_overall_2, freq = TRUE)
hist(df_wide$knowledge_overall_3, freq = TRUE)


# create variable for attendance at FUAT alone (not intensive training)
df_wide <- df_wide %>%
  mutate(
    attendance_fuat = attendance_score - 1
  )

# set reference groups
df_wide$position_groups <- relevel(df_wide$position_groups, ref = "Medical doctor")
df_wide$age_groups <- relevel(df_wide$age_groups, ref = "Less than 25 years old")

# bivariate analysis for additive effects
library(jtools)
lm_sex <- lm(knowledge_overall_3 ~ sex_factored, data = df_wide)
summ(lm_sex)
confint(lm_sex)

lm_position <- lm(knowledge_overall_3 ~ position_groups, data = df_wide)
summ(lm_position)
confint(lm_position)

lm_age <- lm(knowledge_overall_3 ~ age_groups, data = df_wide)
summ(lm_age)
confint(lm_age)

lm_attendance <- lm(knowledge_overall_3 ~ attendance_fuat, data = df_wide)
summ(lm_attendance)
confint(lm_attendance)

# linear regression, adjusting for baseline
knowledge_reg <- lm(knowledge_overall_3 ~ knowledge_overall_2, data = df_wide)
summ(knowledge_reg)
confint(knowledge_reg)

att_reg <- lm(attitude_overall_3 ~ attitude_overall_2, data = df_wide)
summ(att_reg)
confint(att_reg)

conf_reg <- lm(confidence_score_3 ~ confidence_score_2 +confidence_score_1, data = df_wide)
summ(conf_reg)
confint(conf_reg)

emp_reg <- lm(empathy_score_3 ~ empathy_score_2 +empathy_score_1, data = df_wide)
summ(emp_reg)
confint(emp_reg)

sys_reg <- lm(system_support_score_3 ~ system_support_score_2 +  system_support_score_1, data = df_wide)
summ(sys_reg)
confint(sys_reg)

# linear regression, adjusting for confounders
example_regession <- lm(knowledge_overall_3 ~ sex_factored + attendance_fuat + 
                          age_groups + position_groups + knowledge_overall_1, data = df_wide)
summ(example_regession)

example_regession <- lm(knowledge_overall_3 ~ sex_factored + attendance_fuat + 
                          age_groups + position_groups + knowledge_overall_1 + 
                          knowledge_overall_2, data = df_wide)
summ(example_regession)

# create new improvement variables
df_wide <- df_wide %>%
  mutate(improvement_knowledge_overall =
           ifelse(knowledge_overall_3 > knowledge_overall_1, 1, 0))

df_wide <- df_wide %>%
  mutate(improvement_attitude_midline =
           ifelse(attitude_overall_3 > attitude_overall_2, 1, 0))

df_wide <- df_wide %>%
  mutate(improvement_attitude_overall =
           ifelse(attitude_overall_3 > attitude_overall_1, 1, 0))

# Logistic regression with improvement variables
logregession_knowledge <- glm(improvement_knowledge_overall ~ sex_factored + 
                                  attendance_score + age_groups + position_groups + 
                                  knowledge_overall_1, 
                              data = df_wide)
exp(coef(logregession_knowledge))
exp(confint(logregession_knowledge))
summary(logregession_knowledge)

logregession_knowledge_midline_exp <- glm(improvement_attitude_midline ~ sex_factored + 
                                        attendance_score + age_groups + position_groups + 
                                        knowledge_overall_1, 
                                      data = df_wide)
exp(coef(logregession_knowledge_midline_exp))
exp(confint(logregession_knowledge_midline_exp))

example_regession_attitudes <- glm(improvement_attitude_overall ~ sex_factored + 
                                     attendance_score + age_groups + position_groups + 
                                     knowledge_overall_1, 
                                   data = df_wide)
exp(coef(example_regession_attitudes))
exp(confint(example_regession_attitudes)) 

# install.packages("logistf")
# library(logistf)
# model <- logistf(improvement_attitude_overall ~ sex_factored + 
#                    attendance_score + age_groups + position_groups + 
#                    knowledge_overall_1, 
#                  data = df_wide,
#                  family = binomial)
# 
# exp(coef(model))
# exp(confint(model)) 
