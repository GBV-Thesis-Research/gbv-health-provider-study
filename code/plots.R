##################################################
## Project: GBV Health provider study
## Script purpose: Plots
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

# Plot scores across time for each domain
knowledge1 <- (mean(subset(df_long, time_point == 1)$knowledge_overall, na.rm = TRUE)/43*100)
knowledge2 <- (mean(subset(df_long, time_point == 2)$knowledge_overall, na.rm = TRUE)/43*100)
knowledge3 <- (mean(subset(df_long, time_point == 3)$knowledge_overall, na.rm = TRUE)/43*100)

attitude1 <- (mean(subset(df_long, time_point == 1)$attitude_overall, na.rm = TRUE)/102*100)
attitude2 <- (mean(subset(df_long, time_point == 2)$attitude_overall, na.rm = TRUE)/102*100)
attitude3 <- (mean(subset(df_long, time_point == 3)$attitude_overall, na.rm = TRUE)/102*100)

syssupport1 <- (mean(subset(df_long, time_point == 1)$system_support_score, na.rm = TRUE)/6*100)
syssupport2 <- (mean(subset(df_long, time_point == 2)$system_support_score, na.rm = TRUE)/6*100)
syssupport3 <- (mean(subset(df_long, time_point == 3)$system_support_score, na.rm = TRUE)/6*100)

conf1 <- (mean(subset(df_long, time_point == 1)$confidence_score, na.rm = TRUE)/40*100)
conf2 <- (mean(subset(df_long, time_point == 2)$confidence_score, na.rm = TRUE)/40*100)
conf3 <- (mean(subset(df_long, time_point == 3)$confidence_score, na.rm = TRUE)/40*100)

empathy1 <- (mean(subset(df_long, time_point == 1)$empathy_score, na.rm = TRUE)/64*100)
empathy2 <- (mean(subset(df_long, time_point == 2)$empathy_score, na.rm = TRUE)/64*100)
empathy3 <- (mean(subset(df_long, time_point == 3)$empathy_score, na.rm = TRUE)/64*100)

practice1 <- (mean(subset(df_long, time_point == 1)$practice_score, na.rm = TRUE)/9*100)
practice2 <- (mean(subset(df_long, time_point == 2)$practice_score, na.rm = TRUE)/9*100)
practice3 <- (mean(subset(df_long, time_point == 3)$practice_score, na.rm = TRUE)/9*100)

dfplot <- data.frame(
  Domain = c("Knowledge", "Attitudes", "System Support", "Confidence", "Empathy", "Practice"),
  `2021-06-14` = c(knowledge1, attitude1, syssupport1, conf1, empathy1, practice1),
  `2021-06-18` = c(knowledge2, attitude2, syssupport2, conf2, empathy2, practice2),
  `2022-10-14` = c(knowledge3, attitude3, syssupport3, conf3, empathy3, practice3)
)

dfplot_long <- pivot_longer(dfplot, cols = starts_with("X"), names_to = "Date", values_to = "Score")
dfplot_long$Date <- lapply(dfplot_long$Date, function(x) substr(x, 2, nchar(x)))
dfplot_long$Date <- as.character(dfplot_long$Date)
dfplot_long$Date <- format(as.Date(dfplot_long$Date, format = "%Y.%m.%d"), format = "%Y-%m-%d")
dfplot_long$Date <- as.Date(dfplot_long$Date)

width <- 5 
height <- width / 1.618 

scoreplot <- ggplot(dfplot_long, aes(x = Date, y = Score, color = Domain)) +
  geom_point() + 
  geom_line() + 
  theme_minimal() +
  scale_x_date()
scoreplot

ggsave("/Users/susanglenn/Desktop/School/csss_569/memo/score_plot.pdf", plot = scoreplot, width = width, height = height, units = "in")

# Log(time) plot
dfplot_long <- dfplot_long %>%
  mutate(numeric_date = dfplot_long$Date - min(dfplot_long$Date))

dfplot_long$numeric_date <- as.numeric(dfplot_long$Date - min(dfplot_long$Date))
class(dfplot_long$numeric_date)

scoreplot_log <- ggplot(dfplot_long, aes(x = 5^numeric_date, y = Score, color = Domain)) +
  geom_point() + 
  geom_line() + 
  theme_minimal() +
  scale_x_log10()
scoreplot_log

scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
              labels = scales::trans_format("log10", scales::math_format(10^.x)))
