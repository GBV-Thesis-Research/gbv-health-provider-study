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

source(paste(gbv_project_wd, "/code/data_import.R", sep = ""))

#### Lint current file ####
style_file(paste(gbv_project_wd, "/code/data_cleaning.R", sep = ""))

# Separate out key from data (key is the redcap entry form containing correct answers)
key <- raw_gbv_survey_data %>%
  filter(participant_id == "KEY") %>%
  select(everything())

# Drop participants that have not consented to have their data used for research
# Drops from 972 to 929 (removes 43 rows)
data <- raw_gbv_survey_data %>%
  filter(consent == 1) %>%
  filter(!date %in% c("2021-06-18", "2021-06-14")) %>%
  filter(municipality != "Dili") %>%
  filter(participant_id != "KEY")

# Standardize municipality names
data <- data %>%
  mutate(municipality = if_else(municipality %in% c("LIQUICA", "Liquisa", "LIQUISA", "Liqujca"),
    "Liquica",
    municipality
  ))

# Filter out identical records which were accidentally imported into RedCap twice
# by the HAMNASA data collection team
# Drops from 929 to 416 (removes 513 rows)
columns_to_not_select <- ("record_id")
all_columns <- colnames(data)
# When the data is first imported, X does not exist in the data. However, it 
# exists later. This is to handle that case.
if ("X" %in% all_columns) {
  columns_to_not_select <- c(columns_to_not_select, "X")
}
data <- data %>%
  select(-all_of(columns_to_not_select)) %>%
  distinct()

# Fix dates that are empty
data <- data %>%
  mutate(date = case_when(
    date == "" & facility %in% c("hatulia", "Hatulia") & time_point == 2 ~ "2021-09-24",
    date == "" & facility == "Maubara" & time_point == 1 ~ "2021-10-11",
    date == "" & facility == "Maubara" & time_point == 2 ~ "2021-10-15",
    TRUE ~ date
  ))

# Clean participant IDs
data <- data.table(data)
data[participant_id == "QB", participant_id := "Q.B"]
data[, participant_id2 := tolower(gsub("[[:digit:]]", "", participant_id))]
data[, participant_id2 := trimws(participant_id2)]
data[participant_id == "FSN200921", participant_id2 := "fsm"]
data[participant_id2 %in% c("zjd dos s", "zjfdoss"), participant_id2 := "zjd doss"]
data[participant_id == "JEXY200921", participant_id2 := "jexi"]
data[participant_id2 %in% c("smds", "s.m.d.s"), participant_id2 := "smds"]
data[participant_id2 %in% c("a.f.d.j", "a.f de j"), participant_id2 := "afdj"]
data[participant_id2 %in% c("od.a.dc", "od a dc"), participant_id2 := "odadc"]
data[participant_id2 %in% c("jl", "j.l"), participant_id2 := "jl_liquica1"]
data[participant_id2 %in% c("n", "no") & facility == "CS LIQUICA", participant_id2 := "n0304_liquica1"]
data[participant_id2 %in% c("f.s", "fs"), participant_id2 := "fs_liquica1"]
data[participant_id2 %in% c("ls.", "ls"), participant_id2 := "ls22"]
data[participant_id %in% c("JS130921", "JS170921"), participant_id2 := "js_atsabe"]
data[participant_id %in% c("MM"), participant_id2 := "mm_liquica1"]
data[participant_id %in% c("JB200921", "JB240921") & facility == "SSK MAUBARA", participant_id2 := "jb_maubara1"]
data[participant_id %in% c("JB200921", "JB240921") & facility == "CS HATULIA", participant_id2 := "jb_hatulia"]
data[participant_id %in% c("AG", "AG200921") & municipality == "Liquica", participant_id2 := "ag_maubara1"]
data[participant_id %in% c("AG.18", "AG18") & facility == "PS DARULETE", participant_id2 := "ag_liquica1"]
data[participant_id == "0503", participant_id2 := "0503_liquica1"]
data[participant_id %in% c("MS200921", "MS240921") & facility == "PS MANULETE", participant_id2 := "ms_manulete"]
data[participant_id %in% c("MS200921", "MS240921") & facility == "CS HATULIA", participant_id2 := "ms_hatulia"]
data[participant_id == "A.M" & facility %in% c("CHC LIQUICA", "LIQUICA"), participant_id2 := "am_liquica1"]
data[participant_id == "181085", participant_id2 := "181085"]
data[participant_id == "66", participant_id2 := "66"]
data[participant_id == "23031983", participant_id2 := "23031983"]
data[participant_id2 %in% c("apa", "apd"), participant_id2 := "ap_asulau_sare"]
data[participant_id2 %in% c("emg", "mge"), participant_id2 := "mge_hatulia"]
data[participant_id %in% c("MFS", "MFG"), participant_id2 := "mfs_liquica1"]
data[participant_id2 %in% c("c.a", "ca"), participant_id2 := "ca_liquica1"]
data[participant_id2 %in% c("hb", "hfb"), participant_id2 := "hb_liquica1"]
data[participant_id2 %in% c("e", "eze"), participant_id2 := "e_liquica1"]
data[participant_id2 == "rdj" & facility == "CHC ATSABE", participant_id2 := "rdj_atsabe"]
data[participant_id2 == "rdj" & facility %like% "GUISARUDO", participant_id2 := "rdj_guisarudo"]
data[participant_id2 == "esb" & facility %like% "HP", participant_id2 := "esb_ladodo"]
data[participant_id2 == "esb" & facility == "PS HATUGAU", participant_id2 := "esb_hatugau"]
data[participant_id2 == "jm" & facility %like% "PS GUIC", participant_id2 := "jm_guico"]
data[participant_id2 == "jm" & facility %like% "GLENO", participant_id2 := "jm_gleno"]
data[participant_id == "NS" & municipality == "Liquica", participant_id2 := "ns_liquica1"]
data[participant_id2 == "ns" & facility %like% "ERMERA", participant_id2 := "ns_ermera"]
data[participant_id2 %in% c("sofj", "sofdj"), participant_id2 := "sofj_gleno"]
data[participant_id2 %in% c("nfm", "nfmb"), participant_id2 := "nfm_gleno"]
data[participant_id2 %in% c("rgs", "igs"), participant_id2 := "igs_fatuquero"]
data[participant_id2 %in% c("ab", "as") & facility == "PS HATUHEI", participant_id2 := "ab_ps_hatuhei"]
data[participant_id2 %in% c("jt", "_") & facility == "SSK LIQUICA", participant_id2 := "jt_liquica1"]
data[participant_id %in% c("190870", "0819FO"), participant_id2 := "fo_gleno"]
data[participant_id %in% c("110364", "0364"), participant_id2 := "0364_ermera"]
data[participant_id2 %in% c("z. m.d. s.a", "z.m.s.d.a."), participant_id2 := "zmdsa_bazartete2"]
data[participant_id2 %in% c("a. f. d. j. p", "a.f.d.j.p"), participant_id2 := "afdjp_bazartete2"]
data[participant_id2 %in% c("r.d.s", "r.d.s."), participant_id2 := "rds_bazartete2"]
data[participant_id2 %in% c("odca", "o.d.c.a."), participant_id2 := "odca_bazartete2"]
data[participant_id2 %in% c("mades", "m.d.s."), participant_id2 := "mds_bazartete2"]
data[participant_id2 %in% c("smds") & facility == "Ps Baura", participant_id2 := "smds_bazartete2"]
data[participant_id %in% c("P.G", "P.6"), participant_id2 := "pg_bazartete2"]
data <- data[!(participant_id2 == "lmv" & date == "2021-08-20" & time_point == 1)]
data[participant_id2 %in% c("lmv", "l.m.v"), participant_id2 := "lmv"]
data[participant_id == "R.D.R.", date := "2021-08-16"]
data[participant_id %in% c("R.D.R.", "R. D. R"), participant_id2 := "rdr_bazartete2"]
data[participant_id2 %in% c("n", "n.p."), participant_id2 := "np_bazartete2"]
data[participant_id2 %in% c("apr", "a.p,r"), participant_id2 := "apr_bazartete2"]
data[participant_id2 %in% c("c.s.", "gx"), participant_id2 := "cs_bazartete2"]
data[participant_id2 == "js" & patient_volume == 1 & facility == "Maubara", participant_id2 := "js1_maubara"]
data[participant_id2 == "js" & patient_volume == 4 & facility == "Maubara", participant_id2 := "js4_maubara"]
data <- data[facility != "JS200921"]
data[participant_id2 == "js" & position == 2 & facility != "Maubara", participant_id2 := "js1_liquica"]
data[participant_id2 == "js" & position == 3 & facility != "Maubara", participant_id2 := "js2_liquica"]
data[participant_id2 %in% c("xxx", "xg"), participant_id2 := "xx_bazartete2"]

# rename
setnames(data, c("participant_id2", "participant_id"), c("participant_id", "participant_id_original"))

# remove participants without matches from dataset (from 414 to 388 observations) removes 26 rows
data <- data %>%
  group_by(participant_id) %>%
  filter(n() >= 2) %>%
  ungroup()

# replace ids with numbers
ids <- data %>%
  distinct(participant_id) %>%
  mutate(data_id = row_number())

data <- data %>%
  left_join(ids, by = "participant_id") %>%
  select(-participant_id) %>%
  rename(participant_id = data_id)

columns_to_move <- c("participant_id", "participant_id_original", "time_point")

# Rearrange columns
data <- data %>%
  select(all_of(columns_to_move), everything())

# Clean up demographic data
data <- data %>%
  mutate(sex_factored = factor(sex, levels = c(1, 2, 3), labels = c("Female", "Male", "Other"))) %>%
  mutate(age_groups = factor(age, levels = c(1, 2, 3, 4, 5), labels = c("Less than 25 years old", "25-34 years old", "35-44 years old", "45-54 years old", "55 years or older"))) %>%
  mutate(position_groups = factor(position, levels = c(1, 2, 3, 4, 5, 6, 7, 8), labels = c("Community health worker", "Medical doctor", "Midwife", "Nurse", "Nursing assistant", "Social worker or counsellor", "Manager", "Other"))) %>%
  mutate(previous_training_factored = factor(previous_training, levels = c(1, 2), labels = c("Yes", "No"))) %>%
  mutate(avg_weekly_pt_volume = factor(patient_volume, levels = c(1, 2, 3, 4, 5), labels = c("Currently not seeing patients", "Less than 20", "20-39", "40-59", "60 or more"))) %>%
  mutate(position_years_clean = ifelse(position_years > 99, 2021 - position_years, position_years)) %>%
  select(-matches("fup"))

# drop participant 11 (388 to 386 rows) removes 2 rows
data <- data[data$participant_id != 11, ]

# Write data to folder
path_to_clean_rds <- paste(gbv_project_wd, "/data/clean/gbv_data_clean.RDS", sep = "")
saveRDS(data, file = path_to_clean_rds)
