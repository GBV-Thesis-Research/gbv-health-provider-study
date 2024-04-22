##################################################
## Project: GBV Health provider study
## Script purpose: Exploratory plots for GBV analysis
## Date: 1-13-24
## Author: Jessica Dyer
##################################################

# SETUP ------------------------------------------------------------------------
current_wd <- getwd()
library(scales)
library(RColorBrewer)
library(ggrepel)
library(ggplot2)

# Lint current file
if (endsWith(current_wd, "gbv-health-provider-study")) {
  gbv_project_wd <- current_wd
} else if (endsWith(current_wd, "/paper")) {
  gbv_project_wd <- str_remove(current_wd, "/paper")
} else {
  print("Got a WD that's not handled in the If-else ladder yet")
}

source(paste(gbv_project_wd, "/code/analysis.R", sep = ""))
style_file(paste(gbv_project_wd, "/code/analysis.R", sep = ""))

analysis_df_fp_wide <- paste(gbv_project_wd, "/data/clean/analysis_data_wide.RDS", sep = "")
df_wide <- readRDS(analysis_df_fp_wide)

analysis_df_fp_long <- paste(gbv_project_wd, "/data/clean/analysis_data_long.RDS", sep = "")
df_long <- readRDS(analysis_df_fp_long)

# Make scores proportional for graphine purposes 
knowledge1 <- (median(subset(df_long, time_point == 1)$knowledge_overall, na.rm = TRUE)/43*100)
knowledge2 <- (median(subset(df_long, time_point == 2)$knowledge_overall, na.rm = TRUE)/43*100)
knowledge3 <- (median(subset(df_long, time_point == 3)$knowledge_overall, na.rm = TRUE)/43*100)

attitude1 <- (median(subset(df_long, time_point == 1)$attitude_overall, na.rm = TRUE)/102*100)
attitude2 <- (median(subset(df_long, time_point == 2)$attitude_overall, na.rm = TRUE)/102*100)
attitude3 <- (median(subset(df_long, time_point == 3)$attitude_overall, na.rm = TRUE)/102*100)

syssupport1 <- (median(subset(df_long, time_point == 1)$system_support_score, na.rm = TRUE)/6*100)
syssupport2 <- (median(subset(df_long, time_point == 2)$system_support_score, na.rm = TRUE)/6*100)
syssupport3 <- (median(subset(df_long, time_point == 3)$system_support_score, na.rm = TRUE)/6*100)

conf1 <- (median(subset(df_long, time_point == 1)$confidence_score, na.rm = TRUE)/40*100)
conf2 <- (median(subset(df_long, time_point == 2)$confidence_score, na.rm = TRUE)/40*100)
conf3 <- (median(subset(df_long, time_point == 3)$confidence_score, na.rm = TRUE)/40*100)

empathy1 <- (median(subset(df_long, time_point == 1)$empathy_score, na.rm = TRUE)/64*100)
empathy2 <- (median(subset(df_long, time_point == 2)$empathy_score, na.rm = TRUE)/64*100)
empathy3 <- (median(subset(df_long, time_point == 3)$empathy_score, na.rm = TRUE)/64*100)

# Create dataframe 
dfplot <- data.frame(
  Domain = c("Knowledge", "Attitudes", "System Support", "Confidence", "Empathy"),
  `2021-06-14` = c(knowledge1, attitude1, syssupport1, conf1, empathy1),
  `2021-06-18` = c(knowledge2, attitude2, syssupport2, conf2, empathy2),
  `2022-10-14` = c(knowledge3, attitude3, syssupport3, conf3, empathy3)
)

# Create long data frame, converting dates to days and scores to percents
dfplot_long <- pivot_longer(dfplot, cols = starts_with("X"), names_to = "Date", values_to = "Score")
dfplot_long$Date <- lapply(dfplot_long$Date, function(x) substr(x, 2, nchar(x)))
dfplot_long$Date <- as.character(dfplot_long$Date)
dfplot_long$Date <- format(as.Date(dfplot_long$Date, format = "%Y.%m.%d"), format = "%Y-%m-%d")
dfplot_long$Date <- as.Date(dfplot_long$Date)

dfplot_long <- dfplot_long %>%
  mutate(numeric_date = dfplot_long$Date - min(dfplot_long$Date) + 1)

dfplot_long$numeric_date <- as.numeric(dfplot_long$Date - min(dfplot_long$Date) + 1)
class(dfplot_long$numeric_date)

dfplot_long <- dfplot_long %>%
  mutate(Score = Score/100)

# Create custom ggplot theme
theme_cavis <- theme(
  ## Removes main plot gray background
  panel.background = element_rect(fill = "white"), 
  
  ## Golden rectangle plotting area (leave out for square)
  aspect.ratio = ((1 + sqrt(5))/2)^(-1), 
  
  ## All axes changes
  axis.ticks.length = unit(0.5, "char"),  # longer ticks
  
  ## Horizontal axis changes
  axis.line.x.top = element_line(linewidth = 0.2),    # thinner axis lines
  axis.line.x.bottom = element_line(linewidth = 0.2), # thinner axis lines
  axis.ticks.x = element_line(linewidth = 0.2),       # thinner ticks
  axis.text.x = element_text(color = "black", size = 12),
  
  ## match type of axis labels and titles
  axis.title.x.top = element_text(size = 12,
                                  margin = margin(t = 0, r = 0, b = 7.5, l = 0)),
  
  ## match type; pad space between title and labels
  axis.title.x.bottom = element_text(size = 12,
                                     margin = margin(t = 7.5, r = 0, b = 0, l = 0)),
  
  ## Vertical axis changes
  axis.ticks.y = element_blank(), # no y axis ticks (gridlines suffice)
  axis.text.y = element_text(color = "black", size = 12,
                             margin = margin(t = 0, r = -4, b = 0, l = 0)),
  
  ## match type of axis labels and titles, pad
  axis.title.y = element_text(size = 12,
                              margin = margin(t = 0, r = 7.5, b = 0, l = 0)),
  
  ## Legend
  legend.key = element_rect(fill = NA, color = NA),
  legend.background = element_blank(),
  legend.title = element_blank(),
  ## Remove unhelpful gray background
  
  ## Gridlines 
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_blank(), 
  
  ## Faceting (small multiples)
  strip.background = element_blank(),
  
  ## Remove unhelpful trellis-like shading of titles
  strip.text.x = element_text(size=12),  # Larger facet titles
  strip.text.y = element_text(size=12, angle = 0),  
  strip.placement = "outside",           # Place titles outside plot
  panel.spacing.x = unit(1.25, "lines"), # Horizontal space b/w plots
  panel.spacing.y = unit(1, "lines")     # Vertical space b/w plots
)

theme_cavis_hgrid <- 
  theme_cavis + 
  theme(panel.grid.major.y = element_line(color = "gray75", linewidth = 0.1), 
        axis.line.x.bottom = element_blank(),
        plot.title = element_text(size = 8, face = "bold", hjust = 0.5),
        axis.title.x.bottom = element_text(size = 8, face = "bold"),
        axis.text.x.bottom = element_text(size = 8),
        axis.text.y = element_text(size = 6)
        )

dfplot_long <- dfplot_long %>%
  mutate(numeric_date = jitter(numeric_date, factor = 0.5))
  
# Score plot
scoreplot_log <- dfplot_long %>%
  ggplot(aes(x = numeric_date, y = Score, color = Domain)) +
  theme_cavis_hgrid + 
  geom_line(linewidth = 0.25) +
  geom_point(shape = 21, size = 0.75, fill = "white", stroke = 0.5) + 
  scale_colour_brewer(palette = "Dark2") + 
  geom_text_repel(data = dfplot_long %>%
              filter(numeric_date > 470), aes(label = Domain), size = 3,
              nudge_x = 0.2, hjust = 0.5) +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = c(.50, .60, .70, .80, .90, 1),
                     labels = scales::percent,
                     limits = c(.50, 1)) +
  coord_cartesian(xlim = c(1, 1100)) +
  scale_x_log10(breaks = c(1, 5, 488),
                labels = c("Before training \n(baseline)", "After 5-day intensive \n(post-intensive)", "14-month follow-up \n(endline)")) +
  labs(x = NULL,
       y = NULL,
       title = "Median test scores of a gender-based violence training for clinicians
       \nat baseline, post-intensive, and endline")
scoreplot_log

width <- 8 
height <- width / 1.618 

ggsave("/Users/susanglenn/Desktop/School/csss_569/memo/score_plot.pdf", plot = scoreplot_log, 
       width = width, height = height, units = "in")


ggsave("/Users/susanglenn/Desktop/School/csss_569/memo/score_plot.png", plot = scoreplot_log, 
       width = width, height = height, units = "in")