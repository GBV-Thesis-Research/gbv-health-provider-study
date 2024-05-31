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

source(paste(gbv_project_wd, "/code/dependencies.R", sep = ""))
source(paste(gbv_project_wd, "/code/data_cleaning.R", sep = ""))
source(paste(gbv_project_wd, "/code/demographic_data_cleaning.R", sep = ""))
source(paste(gbv_project_wd, "/code/attendance_data_cleaning.R", sep = ""))

#### Lint current file ####
style_file(paste(gbv_project_wd, "/code/table_1.R", sep = ""))

# Load cleaned data
path_to_clean_rds <- paste(gbv_project_wd, "/data/clean/demographic_data_clean.RDS", sep = "")
clean_data <- readRDS(path_to_clean_rds)

# Create table 1 - for publication
filtered_data <-
  clean_data %>%
  filter(status == "All three") %>%
  mutate(position_groups = droplevels(position_groups))

# Join attendance data to filtered data
filtered_data <- left_join(attendance_data, filtered_data, by = c("participant_id_3")) %>%
  select(-starts_with("FUAT"), -starts_with("GBV"), -"attendance_score")

# Create table 1
demographic_table <- filtered_data %>%
  select(c(
    "sex_factored", "age_collapsed", "position_groups", "position_years_clean",
    "municipality", "previous_training_factored", "attendance_score_FUAT"
  )) %>%
  tbl_summary(by = municipality, label = list(
    sex_factored ~ "Sex",
    age_collapsed ~ "Age (years)",
    position_groups ~ "Position",
    position_years_clean ~ "Years of practice",
    previous_training_factored ~ "Previous GBV Training",
    attendance_score_FUAT ~ "Attendance at Learning Labs (out of 8)"
  ), type = list(position_years_clean ~ "continuous", attendance_score_FUAT ~ "continuous"),
    digits = c(sex_factored, age_collapsed, position_groups, 
                            position_years_clean, previous_training_factored,
                            attendance_score_FUAT) ~ 1) %>%
  add_overall() %>%
  add_n() 
demographic_table

# Create table 1 - for comparison to <2 timepoints
filtered_data_comparison <-
  clean_data %>%
  mutate(position_groups = droplevels(position_groups))

demographic_table_comparison <- filtered_data_comparison %>%
  select(c(
    "sex_factored", "age_collapsed", "position_groups", "position_years_clean",
    "municipality", "status_binary", "previous_training_factored"
  )) %>%
  tbl_summary(by = status_binary, label = list(
    municipality ~ "Municipality",
    sex_factored ~ "Sex",
    age_collapsed ~ "Age (years)",
    position_groups ~ "Position",
    position_years_clean ~ "Years of practice",
    previous_training_factored ~ "Previous GBV Training"
  ), type = list(position_years_clean ~ "continuous")) %>%
  add_overall() %>%
  add_p() %>%
  add_n()

demographic_table_comparison

# Create custom ggplot theme
theme_cavis <- theme(
  ## Removes main plot gray background
  panel.background = element_rect(fill = "white"),

  ## Golden rectangle plotting area (leave out for square)
  aspect.ratio = ((1 + sqrt(5)) / 2)^(-1),

  ## All axes changes
  axis.ticks.length = unit(0.5, "char"), # longer ticks

  ## Horizontal axis changes
  axis.line.x.top = element_line(linewidth = 0.2), # thinner axis lines
  axis.line.x.bottom = element_line(linewidth = 0.2), # thinner axis lines
  axis.ticks.x = element_line(linewidth = 0.2), # thinner ticks
  axis.text.x = element_text(color = "black", size = 12),

  ## match type of axis labels and titles
  axis.title.x.top = element_text(
    size = 12,
    margin = margin(t = 0, r = 0, b = 7.5, l = 0)
  ),

  ## match type; pad space between title and labels
  axis.title.x.bottom = element_text(
    size = 12,
    margin = margin(t = 7.5, r = 0, b = 0, l = 0)
  ),

  ## Vertical axis changes
  axis.ticks.y = element_blank(), # no y axis ticks (gridlines suffice)
  axis.text.y = element_text(
    color = "black", size = 10,
    margin = margin(t = 0, r = -4, b = 0, l = 0)
  ),

  ## match type of axis labels and titles, pad
  axis.title.y = element_text(
    size = 10,
    margin = margin(t = 0, r = 7.5, b = 0, l = 0)
  ),

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
  strip.text.x = element_text(size = 12), # Larger facet titles
  strip.text.y = element_text(size = 12, angle = 0),
  strip.placement = "outside", # Place titles outside plot
  panel.spacing.x = unit(1.25, "lines"), # Horizontal space b/w plots
  panel.spacing.y = unit(1, "lines") # Vertical space b/w plots
)

theme_cavis_hgrid <-
  theme_cavis +
  theme(
    panel.grid.major.y = element_line(color = "gray75", linewidth = 0.1),
    axis.line.x.bottom = element_blank(),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.title.x.bottom = element_text(size = 12, face = "bold"),
    axis.text.x.bottom = element_text(size = 10),
    axis.text.y = element_text(size = 8)
  )


# Plots for sex, age, and profession
sex_plot <- analysis_wide %>%
  ggplot(aes(x = sex_factored, fill = sex_factored)) +
  geom_bar() +
  theme_cavis_hgrid +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "Sex of Participants", x = NULL, y = "Percent") +
  theme(legend.position = "none") +
  scale_y_continuous(
    breaks = seq(0, 60, by = 20),
    limits = c(0, 60)
  )
sex_plot

age_plot <- analysis_wide %>%
  ggplot(aes(x = age_groups, fill = age_groups)) +
  geom_bar() +
  theme_cavis_hgrid +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "Age of Participants", x = NULL, y = "Percent") +
  theme(legend.position = "none") +
  scale_y_continuous(
    breaks = seq(0, 50, by = 15),
    limits = c(0, 50)
  )
age_plot

profession_plot <- analysis_wide %>%
  ggplot(aes(x = position_groups, fill = position_groups)) +
  geom_bar() +
  theme_cavis_hgrid +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "Participant Professions", x = NULL, y = "Percent") +
  theme(legend.position = "none") +
  scale_y_continuous(
    breaks = seq(0, 30, by = 10),
    limits = c(0, 30)
  )
profession_plot

width <- 8
height <- width / 1.618

ggsave(paste(gbv_project_wd, "/figures/sex_plot.png", sep = ""), plot = sex_plot, width = width, height = height, units = "in")
ggsave(paste(gbv_project_wd, "/figures/age_plot.png", sep = ""), plot = age_plot, width = width, height = height, units = "in")
ggsave(paste(gbv_project_wd, "/figures/profession.png", sep = ""), plot = profession_plot, width = width, height = height, units = "in")
