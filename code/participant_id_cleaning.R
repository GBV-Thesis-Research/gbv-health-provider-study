##################################################
## Project: GBV health provider study
## Script purpose: Participant ID cleaning
## Date: 2023-08-10
## Author: Susan Glenn
##################################################

# SETUP ------------------------------------------------------------------------
# WD setup
current_wd <- getwd()

if (endsWith(current_wd, "gbv-health-provider-study")) {
  gbv_project_wd <- current_wd
} else if (endsWith(current_wd, "/paper")) {
  gbv_project_wd <- str_remove(current_wd, "/paper")
} else {
  print("Got a WD that's not handled in the If-else ladder yet")
}

source(paste(gbv_project_wd, "/code/data_cleaning.R", sep = ""))
# Lint current file
style_file(paste(gbv_project_wd, "/code/participant_id_cleaning.R", sep = ""))

path_to_imterim_clean_rds <- paste(gbv_project_wd, "/data/clean/gbv_data_interim_clean.RDS", sep = "")
path_to_link_log <- paste(gbv_project_wd, "/extra_data/link_log.xlsx", sep = "")
data <- readRDS(path_to_imterim_clean_rds)

link_log <- read_excel(path_to_link_log) %>%
  select(participant_id_2, participant_id_3) %>%
  distinct(participant_id_2, .keep_all = TRUE)

data <- data %>%
  mutate(
    participant_id_2 =
      tolower(str_replace_all(participant_id, "[0-9[:space:][:punct:]]", ""))
  ) %>%
  mutate(participant_id_2 = ifelse(participant_id_2 == "", participant_id, participant_id_2)) %>%
  mutate(participant_id_2 = paste(participant_id_2, tolower(str_replace_all(standardized_facility, " ", "_")), sep = "_")) %>%
  group_by(participant_id_2, time_point) %>%
  mutate(row_number = row_number()) %>%
  mutate(participant_id_2 = ifelse(row_number > 1, paste0(participant_id_2, "_", row_number), participant_id_2)) %>%
  select(-row_number) %>%
  ungroup() %>%
  left_join(link_log, by = "participant_id_2") %>%
  group_by(participant_id_3) %>%
  mutate(
    entries_1 = sum(time_point == 1),
    entries_2 = sum(time_point == 2),
    entries_3 = sum(time_point == 3)
  ) %>%
  mutate(status = ifelse(entries_1 == 1 & entries_2 == 1 & entries_3 == 1, "All three", NA)) %>%
  mutate(inclusive_status = ifelse(entries_1 == 1 & entries_2 == 1 & (entries_3 == 1 | entries_3 == 0), "One & two inclusive", NA)) %>%
  mutate(status = ifelse(entries_1 == 1 & entries_2 == 1 & entries_3 == 0, "One & two only", status)) %>%
  mutate(status = ifelse(entries_1 == 1 & entries_2 == 0 & entries_3 == 1, "One & three only", status)) %>%
  mutate(status = ifelse(entries_1 == 0 & entries_2 == 1 & entries_3 == 1, "Two & three only", status)) %>%
  mutate(status = ifelse(entries_1 == 1 & entries_2 == 0 & entries_3 == 0, "One only", status)) %>%
  mutate(status = ifelse(entries_1 == 0 & entries_2 == 1 & entries_3 == 0, "Two only", status)) %>%
  mutate(status = ifelse(entries_1 == 0 & entries_2 == 0 & entries_3 == 1, "Three only", status)) %>%
  mutate(status = ifelse(is.na(status), "Other", status)) %>%
  ungroup() %>%
  relocate(c(status, participant_id_3), .after = time_point)

# Clustering code to figure out matches
dist_matrix <- stringdist::stringdistmatrix(data$participant_id_2, data$participant_id_2)

hclust_result <- hclust(as.dist(dist_matrix), method = "ward.D2")
num_clusters <- nrow(data) / 3 # You can adjust this based on your data and desired grouping
clusters <- cutree(hclust_result, k = num_clusters)

data_clustered <- data %>%
  mutate(cluster = clusters)

data_clustered <- data_clustered %>%
  group_by(cluster) %>%
  mutate(
    entries_1 = sum(time_point == 1),
    entries_2 = sum(time_point == 2),
    entries_3 = sum(time_point == 3)
  ) %>%
  mutate(flag = ifelse(entries_1 == 1 & entries_2 == 1 & entries_3 == 1, "Three timepoints", NA)) %>%
  mutate(flag = ifelse(is.na(flag) & entries_1 == 1 & entries_2 == 1 & entries_3 == 0, "Two timepoints", flag)) %>%
  mutate(flag = ifelse(is.na(flag) & entries_1 == 1 & entries_2 == 1 & entries_3 > 0, "Maybe matches", flag)) %>%
  ungroup() %>%
  select(participant_id, participant_id_2, time_point, standardized_facility, time_point, cluster, flag, entries_1, entries_2, entries_3)

data_with_three_time_points <- data %>%
  filter(status == "All three") %>%
  select(-all_of(c(
    "participant_id", "entries_1", "entries_2", "entries_3", "facility_name_title_case",
    "participant_id_2", "facility", "date"
  )))

clean_data <- data %>%
  select(-all_of(c(
    "participant_id", "entries_1", "entries_2", "entries_3", "facility_name_title_case",
    "participant_id_2", "facility", "date"
  )))

participant_id_table_data <- clean_data %>%
  select(participant_id_3, status, inclusive_status, standardized_facility) %>%
  distinct(participant_id_3, status, standardized_facility, inclusive_status) %>%
  mutate(status = factor(status, levels = c(
    "All three", "One & two only",
    "One & three only", "Two & three only",
    "One only", "Two only", "Three only"
  )))
participant_id_table <-
  participant_id_table_data %>%
  select(status, inclusive_status) %>%
  tbl_summary(
    label = list(status ~ "Exclusive timepoint status", inclusive_status ~ "Inclusive timepoint status")
  )

<<<<<<< HEAD
# Check for mismatched demographic data from pre, post-intensive, and follow-up tests
dem_info <- clean_data %>%
  select("participant_id_3", "time_point", "sex", "age", "position", "position_years_clean")

dem_pre <- dem_info %>% filter(time_point == 1)
dem_mid <- dem_info %>% filter(time_point == 2)
dem_post <- dem_info %>% filter(time_point == 3)

names(dem_pre) <- paste0("pre_", names(dem_pre))
names(dem_mid) <- paste0("mid_", names(dem_mid))
names(dem_post) <- paste0("post_", names(dem_post))

# Merge dem_pre and dem_mid, then add in dem_post
merged_pre_mid <- merge(dem_pre, dem_mid, by.x = "pre_participant_id_3", by.y = "mid_participant_id_3")
dem_all <- merge(merged_pre_mid, dem_post, by.x = "pre_participant_id_3", by.y = "post_participant_id_3")

# Create 'mismatched' column in dem_all dataframe to identify mismatched data between timepoints 1, 2, and 3
dem_all <- dem_all %>%
  mutate(
    mismatched_sex =
      ifelse(pre_sex != mid_sex | mid_sex != post_sex, 1, 0)
  )

dem_all <- dem_all %>%
  mutate(
    mismatched_position =
      ifelse(pre_position != mid_position | mid_position != post_position, 1, 0),
  )

dem_all <- dem_all %>%
  mutate(
    mismatched_agegrp =
      ifelse(pre_age != mid_age | mid_age != post_age, 1, 0),
  )

=======
>>>>>>> main
# Write data to folder
path_to_clean_rds <- paste(gbv_project_wd, "/data/clean/gbv_data_clean.RDS", sep = "")
path_to_clean_three_timepoints <- paste(gbv_project_wd, "/data/clean/gbv_data_clean_three_timepoints.RDS", sep = "")
saveRDS(clean_data, file = path_to_clean_rds)
saveRDS(data_with_three_time_points, file = path_to_clean_three_timepoints)
