#############################################################################################
#
# Author: Cory Spencer
# Purpose: Clean and prepare data for Harmonia Project analyses (script 2; composite indicator prep)
#
#############################################################################################
rm(list = ls())

# set up environment -------------------------------------------------------------------------
library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)

date <- gsub("-", "_", Sys.Date())

out_dir <- paste0("FILEPATH/", date, "/")
dir.create(out_dir, showWarnings = FALSE)

# set standard dem vars
dem.vars <- c(
  "time_point", "participant_id", "municipality",
  "date", "facility", "sex", "position", "age", "position_years",
  "previous_training", "patient_volume", "position_other",
  "previous_training_desc", "post_module_attendance", "post_ha", "post_hu", "post_re", "post_la", "post_s", "post_au", "post_n",
  "training_group", "demographic_mismatch"
)

# (1) Read in pre-prepped data and do some additional processing ------------------------------------------------------------------
data <- fread(paste0(out_dir, "hp_pre_post_clean.csv"))

# domain possible points
points <- data.table(
  domain = c(
    "attitudes_11", "attitudes_12", "attitudes_13", "attitudes_14",
    "confidence_16", "empathy_17", "knowledge_7", "knowledge_8", "knowledge_9",
    "knowledge_10", "system_support_15"
  ),
  points = c(
    (length(names(data)[names(data) %like% "attitudes_11"]) - 1) * 4,
    (length(names(data)[names(data) %like% "attitudes_12"]) - 1) * 2,
    (length(names(data)[names(data) %like% "attitudes_13"]) - 1) * 4,
    (length(names(data)[names(data) %like% "attitudes_14"]) - 1) * 4,
    (length(names(data)[names(data) %like% "confidence_16"]) - 1) * 3,
    (length(names(data)[names(data) %like% "empathy_17"]) - 1) * 4,
    (length(names(data)[names(data) %like% "knowledge_7"]) - 1),
    (length(names(data)[names(data) %like% "knowledge_8"]) - 1),
    (length(names(data)[names(data) %like% "knowledge_9"]) - 1),
    (length(names(data)[names(data) %like% "knowledge_10"]) - 1),
    (length(names(data)[names(data) %like% "system_support_15"]) - 1)
  )
)

# merge on denom
dt.long <- melt.data.table(data, id.vars = dem.vars)
dt.long <- dt.long[variable %in% c(unique(points$domain), names(data)[names(data) %like% "practices"], names(data)[names(data) %like% "system_support"])]
setnames(dt.long, "variable", "domain")

dem.info <- data[, .SD, .SDcols = dem.vars]
dem_pre <- dem.info[time_point == "pre"]
dem_post <- dem.info[time_point == "post"]
names(dem_pre) <- paste0("pre_", names(dem_pre))
names(dem_post) <- paste0("post_", names(dem_post))
dem_both <- merge(dem_pre, dem_post, by.x = "pre_participant_id", by.y = "post_participant_id")
fix_sex <- dem_both[is.na(pre_sex) | is.na(post_sex)]$pre_participant_id
fix_age <- dem_both[is.na(pre_age) | is.na(post_age)]$pre_participant_id
fix_position <- dem_both[is.na(pre_position) | is.na(post_position)]$pre_participant_id

dem_both[is.na(pre_sex) & !is.na(post_sex), pre_sex := post_sex]
dem_both[is.na(post_sex) & !is.na(pre_sex), post_sex := pre_sex]
dem_both[is.na(pre_age) & !is.na(post_age), pre_age := post_age]
dem_both[is.na(post_age) & !is.na(pre_age), post_age := pre_age]
dem_both[is.na(pre_position) & !is.na(post_position), pre_position := post_position]
dem_both[is.na(post_position) & !is.na(pre_position), post_position := pre_position]
dt.long <- merge(dt.long, dem_both[, .(pre_participant_id, pre_sex, pre_age, pre_position)], by.x = "participant_id", by.y = "pre_participant_id", allow.cartesian = T)
dt.long[participant_id %in% fix_sex, sex := pre_sex]
dt.long[participant_id %in% fix_age, age := pre_age]
dt.long[participant_id %in% fix_position, position := pre_position]
dt.long[, c("pre_sex", "pre_age", "pre_position") := NULL]

# cast wide on time point
prop.data <- dcast.data.table(data = dt.long, participant_id + domain + municipality + age + sex + position +
  training_group ~ time_point, value.var = "value")
prop.data[, change := post - pre]
prop.data <- merge(prop.data, points, by = "domain", all.x = T)
prop.data[, `:=`(pre_pct = pre / points, post_pct = post / points)]
prop.data[, change_pct := post_pct - pre_pct]
prop.data[, improve := ifelse(change > 0, 1, 0)]
write.csv(prop.data, paste0(out_dir, "prop_data.csv"), row.names = F)

# melt long
pdata <- melt.data.table(data = prop.data, measure.vars = c("pre", "post", "pre_pct", "post_pct", "change", "change_pct", "improve"))
write.csv(pdata, paste0(out_dir, "prop_data_long.csv"), row.names = F)
