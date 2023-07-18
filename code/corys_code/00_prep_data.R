#############################################################################################
#
# Author: Cory Spencer
# Purpose: Clean and prepare data for Harmonia Project analyses; script 1 (initial cleaning and creating participant matches)
#
#############################################################################################

rm(list=ls())

#set up environment -------------------------------------------------------------------------
library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)

out_dir <- paste0('FILEPATH', gsub('-', '_', Sys.Date()), '/')
dir.create(out_dir, showWarnings=FALSE)

#set standard dem vars
dem.vars <- c('time_point', 'participant_id', 'municipality', 
              'facilitator', 'date', 'facility', 'sex', 'position', 'age', 'position_years', 'position_months',
              'previous_training', 'patient_volume', 'position_other')

# (1) Read in raw data and prepare to re-score answers -------------------------------------------------------------------------------------------------------
data <- fread(paste0('FILEPATH TO SAVED DATA'))
data <- data[consent==1] #subset data to only include those who consented to their data being used for research purposes

#subset to non-standardization records (standardization records in redcap were those collected from training of trainers)
data <- data[!date %in% c('2021-06-18', '2021-06-14')]
data <- data[municipality!='Dili']

#standardize municipality names
data[municipality=='ERMERA', municipality:='Ermera']
data[municipality %in% c('LIQUICA', 'Liquisa', 'LIQUISA', 'Liqujca'), municipality:='Liquica']

#separate out key from data (key is the redcap entry form containing correct answers)
key <- data[participant_id=='KEY']
data <- data[participant_id!='KEY']

#filter out identical records which were accidentally imported into RedCap twice by the HAMNASA data collection team
data_records <- copy(data)
data <- data[, record_id:=NULL]
data <- unique(data)

#fix blank dates
data[date=='' & facility %in% c('hatulia', 'Hatulia') & time_point==2, date:='2021-09-24']
data[date=='' & facility =='Maubara' & time_point==1, date:='2021-10-11']
data[date=='' & facility =='Maubara' & time_point==2, date:='2021-10-15']

#standardize participant ids to create individual level pre/post record matches (making matches manually by looking for similarities between IDs and demographic information/training site & date)
data[participant_id=='QB', participant_id:='Q.B']
data[participant_id=='QUEI_250785', participant_id:='QUEI250785']
data[, participant_id2:=tolower(gsub("[[:digit:]]","",participant_id))]
data[, participant_id2:=trimws(participant_id2)]
data[participant_id=='FSN200921', participant_id2:='fsm']
data[participant_id2 %in% c('zjd dos s', 'zjfdoss'), participant_id2:='zjd doss']
data[participant_id=='JEXY200921', participant_id2:='jexi']
data[participant_id2 %in% c('smds', 's.m.d.s'), participant_id2:='smds']
data[participant_id2 %in% c('a.f.d.j', 'a.f de j'), participant_id2:='afdj']
data[participant_id2 %in% c('od.a.dc', 'od a dc'), participant_id2:='odadc']
data[participant_id2 %in% c('jl', 'j.l'), participant_id2:='jl_liquica1']
data[participant_id2 %in% c('n', 'no') & facility=='CS LIQUICA', participant_id2:='n0304_liquica1']
data[participant_id2 %in% c('f.s', 'fs'), participant_id2:='fs_liquica1']
data[participant_id2 %in% c('ls.', 'ls'), participant_id2:='ls22']
data[participant_id %in% c('JS130921', 'JS170921'), participant_id2:='js_atsabe']
data[participant_id %in% c('MM'), participant_id2:='mm_liquica1']
data[participant_id %in% c('JB200921', 'JB240921') & facility=='SSK MAUBARA', participant_id2:='jb_maubara1']
data[participant_id %in% c('JB200921', 'JB240921') & facility=='CS HATULIA', participant_id2:='jb_hatulia']
data[participant_id %in% c('AG', 'AG200921') & municipality=='Liquica', participant_id2:='ag_maubara1']
data[participant_id %in% c('AG.18', 'AG18') & facility=='PS DARULETE', participant_id2:='ag_liquica1']
data[participant_id=='0503', participant_id2:='0503_liquica1']
data[participant_id %in% c('MS200921', 'MS240921') & facility=='PS MANULETE', participant_id2:='ms_manulete']
data[participant_id %in% c('MS200921', 'MS240921') & facility=='CS HATULIA', participant_id2:='ms_hatulia']
data[participant_id == 'A.M' & facility %in% c('CHC LIQUICA', 'LIQUICA'), participant_id2:='am_liquica1']
data[participant_id == '181085', participant_id2:='181085'] 
data[participant_id == '66', participant_id2:='66']
data[participant_id == '23031983', participant_id2:='23031983'] 
data[participant_id2 %in% c('apa', 'apd'), participant_id2:='ap_asulau_sare'] 
data[participant_id2 %in% c('emg', 'mge'), participant_id2:='mge_hatulia']
data[participant_id %in% c("MFS", "MFG"), participant_id2:='mfs_liquica1']
data[participant_id2 %in% c('c.a', 'ca'), participant_id2:='ca_liquica1']
data[participant_id2 %in% c('hb', 'hfb'), participant_id2:='hb_liquica1'] 
data[participant_id2 %in% c('e', 'eze'), participant_id2:='e_liquica1'] 
data[participant_id2 == 'rdj' & facility == 'CHC ATSABE', participant_id2:='rdj_atsabe']
data[participant_id2 == 'rdj' & facility %like% 'GUISARUDO', participant_id2:='rdj_guisarudo']
data[participant_id2 == 'esb' & facility %like% 'HP', participant_id2:='esb_ladodo']
data[participant_id2 == 'esb' & facility == 'PS HATUGAU', participant_id2:='esb_hatugau']
data[participant_id2 == 'jm' & facility %like% 'PS GUIC', participant_id2:='jm_guico']
data[participant_id2 == 'jm' & facility %like% 'GLENO', participant_id2:='jm_gleno']
data[participant_id=='NS' & municipality=='Liquica', participant_id2:='ns_liquica1']
data[participant_id2=='ns' & facility %like% 'ERMERA', participant_id2:='ns_ermera']
data[participant_id2 %in% c("sofj", "sofdj"), participant_id2:='sofj_gleno']
data[participant_id2 %in% c("nfm", "nfmb"), participant_id2:='nfm_gleno']
data[participant_id2 %in% c('rgs', 'igs'), participant_id2:='igs_fatuquero']
data[participant_id2 %in% c('ab', 'as') & facility=='PS HATUHEI', participant_id2:='ab_ps_hatuhei']
data[participant_id2 %in% c('jt', '_') & facility=='SSK LIQUICA', participant_id2:='jt_liquica1']
data[participant_id %in% c('190870', '0819FO'), participant_id2:='fo_gleno'] 
data[participant_id %in% c('110364', '0364'), participant_id2:='0364_ermera']
data[participant_id2 %in% c('z. m.d. s.a', 'z.m.s.d.a.'), participant_id2:='zmdsa_bazartete2']
data[participant_id2 %in% c('a. f. d. j. p', 'a.f.d.j.p'), participant_id2:='afdjp_bazartete2']
data[participant_id2 %in% c('r.d.s', 'r.d.s.'), participant_id2:='rds_bazartete2']
data[participant_id2 %in% c('odca', 'o.d.c.a.'), participant_id2:='odca_bazartete2']
data[participant_id2 %in% c('mades', 'm.d.s.'), participant_id2:='mds_bazartete2']
data[participant_id2 %in% c('smds') & facility=='Ps Baura', participant_id2:='smds_bazartete2']
data[participant_id %in% c('P.G', 'P.6'), participant_id2:='pg_bazartete2']
data <- data[!(participant_id2=='lmv' & date=='2021-08-20' & time_point==1)]
data[participant_id2 %in% c('lmv', 'l.m.v'), participant_id2:='lmv']
data[participant_id=='R.D.R.', date:='2021-08-16']
data[participant_id %in% c('R.D.R.', 'R. D. R'), participant_id2:='rdr_bazartete2']
data[participant_id2 %in% c('n', 'n.p.'), participant_id2:='np_bazartete2'] 
data[participant_id2 %in% c('apr', 'a.p,r'), participant_id2:='apr_bazartete2'] 
data[participant_id2 %in% c('c.s.', 'gx'), participant_id2:='cs_bazartete2'] 
data[participant_id2=='js' & patient_volume==1 & facility=='Maubara', participant_id2:='js1_maubara']
data[participant_id2=='js' & patient_volume==4 & facility=='Maubara', participant_id2:='js4_maubara']
data <- data[facility!='JS200921']
data[participant_id2=='js' & position==2 & facility!='Maubara', participant_id2:='js1_liquica'] 
data[participant_id2=='js' & position==3 & facility!='Maubara', participant_id2:='js2_liquica']
data[participant_id2 %in% c('xxx', 'xg'), participant_id2:='xx_bazartete2'] 

#rename
setnames(data, c('participant_id2', 'participant_id'), c('participant_id', 'participant_id_original'))

#remove participants without matches from dataset
data <- data[!participant_id %in% c(unique(need.match$participant_id), 'jgg')]

#replace ids with numbers
ids <- data.table(participant_id=unique(data$participant_id), data_id=seq(1:length(unique(data$participant_id))))
data <- merge(data, ids, by='participant_id')
data[, participant_id:=NULL]
setnames(data, 'data_id', 'participant_id')

# (2) Clean up demographic information ----------------------------------------------------------------------------------
sex_map <- data.table(sex=c(1,2,3,NA), Sex=c('Female', 'Male', 'Other', NA))
data <- merge(data, sex_map, by='sex', all.x=T)

age_map <- data.table(age=c(1,2,3,4,5,NA), Age=c('Less than 25 years old', '25-34 years old', '35-44 years old', '45-54 years old', '55 years or older', NA))
data <- merge(data, age_map, by='age', all.x=T)

position_map <- data.table(position=c(1,2,3,4,5,6,7,8,NA), Position=c('Community health worker', 'Medical doctor', 'Midwife', 'Nurse', 'Nursing assistant', 'Social worker or counsellor', 'Manager', 'Other', NA))
data <- merge(data, position_map, by='position', all.x=T, all.y=F)

data[, previous_training:=ifelse(previous_training==1, 'Yes', 'No')]

pt_volume <- data.table(patient_volume=c(1,2,3,4,5,NA), `Average weekly patient volume`=c('Currently not seeing patients', 'Less than 20', '20-39', '40-59', '60 or more', NA))
data <- merge(data, pt_volume, by='patient_volume', all.x=T)

data[, position_years:=ifelse(position_years>99, 2021-position_years, position_years)]

data[, c('age', 'sex', 'position', 'patient_volume'):=NULL]
data[, names(data)[names(data) %like% 'fup']:=NULL]
setnames(data, c('Age', 'Sex', 'Position', 'Average weekly patient volume'), c('age', 'sex', 'position', 'patient_volume'))

write.csv(data, paste0(out_dir, 'raw_data.csv'), row.names=F)

# (3) Create derived variables by topic and construct ----------------------------------------------------------------------------------------------------------------------------

# KNOWLEDGE --------------------------------------------------------------------------------
#recode knowledge variables (all T/F), using key values (answer choice of "I don't know" is scored as false)

kn.vars <- names(data)[names(data) %like% 'knowledge']
for (k in kn.vars){
  c.val <- key[, get(k)]
  data[!is.na(get(k)), (k):=ifelse(get(k)==c.val,1,0)]
}

# ATTITUDES ----------------------------------------------------------------------------------------
att.vars <- names(data)[names(data) %like% 'attitudes']
att12.vars <- att.vars[att.vars %like% 'attitudes_12']
att.vars <- att.vars[!att.vars %like% 'attitudes_12']

for (a in att.vars){
  c.val <- key[, get(a)]
  if (c.val==5){  #if c.val == 5, move scale down by 1 (0-4)
    data[, (a):=get(a)-1]
  } else if (c.val==1) { #otherwise, reverse code
    data[, (a):=get(a)+3]
    data[, (a):=abs(get(a)-8)]
  }
}

#move scale down by 1
data[, (att12.vars):=lapply(.SD, function(x) x-1), .SDcols=att12.vars]

#re-assign I don't know (now coded as 3) to the same as sometimes acceptable (1)
for (a in att12.vars){
  data[, (a):=ifelse(get(a)==3, 1, get(a))]
}

# SYSTEM SUPPORT -----------------------------------------------------------------------------------------------------
sys.vars <- names(data)[names(data) %like% 'system_support']
for (s in sys.vars){
  c.val <- key[, get(s)]
  data[, (s):=ifelse(get(s)==c.val,1,0)]
}

# CONFIDENCE ---------------------------------------------------------------------------------------------------------
conf.vars <- names(data)[names(data) %like% 'confidence']

#all items are positively worded; move scale down by 1 so correct = 3 points
data[, (conf.vars):=lapply(.SD, function(x) x-1), .SDcols=conf.vars]

#EMPATHY -------------------------------------------------------------------------------------------------------------
emp.vars <- names(data)[names(data) %like% 'empathy']

#score according to scale of questions
for (e in emp.vars){
  c.val <- key[, get(e)]
  if (c.val==5){ #if c.val == 5, move scale down by 1 (0-4)
    data[, (e):=get(e)-1]
  } else if (c.val==1) { #otherwise, reverse code
    data[, (e):=get(e)+3]
    data[, (e):=abs(get(e)-8)]
  }
}

#PRACTICES ------------------------------------------------------------------------------------------------------------------------------------

prac.vars <- names(data)[names(data) %like% 'practices']

#look for 'bad' data -- responding to q19 when q18 != 'yes'
table(data$practices_18, data$practices_19a)
dim(data[practices_18 != 1 & !is.na(practices_19a)]) 

#recode to 0/1 (all correct answers are 1's, 2's=incorrect)
prac19vars <- paste0('practices_19', letters[1:9])
data[, (prac19vars):=lapply(.SD, function(x) abs(x-2)), .SDcols=prac19vars]

# (4) Save out all domains coded with scaled scoring ----------------------------------------------------------------------------------------------------------------------------------------------

#calculate derived variable scores 
data[, c('harmonia_learning_lab_curriculum_pre_evaluation_complete', 'participant_id_original', 'consent'):=NULL]
dem.vars <- c(dem.vars, 'previous_training_desc', 'post_module_attendance', 'post_ha', 'post_hu', 'post_re', 'post_la', 'post_s', 'post_au', 'post_n')
dt.long <- melt.data.table(data, id.vars=c(dem.vars))
dt.long[, domain:=str_sub(variable, 1, -2)]

#sum points by participant and domain and time point
sums <- dt.long[, .(num=sum(value, na.rm=F)), by=.(participant_id, time_point, domain, municipality)]

#saved cleaned sums for collaborators (derived + individual items)
derived_data <- copy(sums)
item_data <- copy(dt.long)[, .(participant_id, time_point, municipality, variable, value)]
setnames(item_data, c('variable', 'value'), c('domain', 'num'))
all_data <- rbind(derived_data, item_data)
setnames(all_data, c('domain', 'num'), c('derived_variable', 'composite_score'))
final <- dcast(all_data, municipality+time_point+participant_id ~ derived_variable, value.var='composite_score')
dem.info <- dt.long[, .SD, .SDcols=c(dem.vars)]
dem.info <- unique(dem.info)
final <- merge(final, dem.info, by=c('participant_id', 'time_point', 'municipality'), all.x=T)
derived_vars <- unique(points$domain)
setcolorder(final, neworder=c(dem.vars, derived_vars, kn.vars, att.vars, att12.vars, sys.vars, conf.vars, emp.vars, prac.vars))
final[, time_point:=ifelse(time_point==1, 'pre', 'post')]
final[date %in% c('2021-07-12', '2021-07-16'), training_group:='Liquica R1 and R2']
final[date %in% c('2021-08-17', '2021-08-16', '2021-08-20'), training_group:='Bazartete R1 and R2']
final[date %in% c('2021-09-20', '2021-09-24') & municipality=='Liquica', training_group:='Maubara R1']
final[date %in% c('2021-10-15', '2021-10-11'), training_group:='Maubara R2']
final[date %in% c('2021-07-19', '2021-07-23'), training_group:='Ermera combined R1']
final[date %in% c('2021-10-18', '2021-10-22'), training_group:='Ermera combined R2']
final[date %in% c('2021-09-13', '2021-09-17'), training_group:='Atsabe']
final[date %in% c('2021-10-26', '2021-10-30'), training_group:='Letefoho']
final[date %in% c('2021-09-20', '2021-09-24') & municipality=='Ermera', training_group:='Hatolia']
final[practices_18!=1, practices_19:=NA]
final[, c('practices_18yes_numbe', 'practices_1'):=NULL]

#find and mark demographic/work history info discrepancies between pre/post observations
dem_pre <- dem.info[time_point==1]
dem_post <- dem.info[time_point==2]
names(dem_pre) <- paste0('pre_', names(dem_pre))
names(dem_post) <- paste0('post_', names(dem_post))
dem_both <- merge(dem_pre, dem_post, by.x='pre_participant_id', by.y='post_participant_id')
final[, sex_mismatch:=ifelse(participant_id %in% unique(dem_both[pre_sex!=post_sex]$pre_participant_id), 1, 0)]
final[, age_mismatch:=ifelse(participant_id %in% unique(dem_both[pre_age!=post_age]$pre_participant_id), 1, 0)]
final[, position_mismatch:=ifelse(participant_id %in% unique(dem_both[pre_position!=post_position]$pre_participant_id), 1, 0)]
final[, position_years_mismatch:=ifelse(participant_id %in% unique(dem_both[pre_position_years!=post_position_years]$pre_participant_id), 1, 0)]
final[, patient_volume_mismatch:=ifelse(participant_id %in% unique(dem_both[pre_patient_volume!=post_patient_volume]$pre_participant_id), 1, 0)]
final[, demographic_mismatch:=age_mismatch+sex_mismatch+position_mismatch]
final <- final[demographic_mismatch<2]
final[, age:=ifelse(age_mismatch==1, NA, age)]
final[, sex:=ifelse(sex_mismatch==1, NA, sex)]
final[, position:=ifelse(position_mismatch==1, NA, position)]
final[, patient_volume:=ifelse(patient_volume_mismatch==1, NA, patient_volume)]
final[, position_years:=ifelse(position_years_mismatch==1, NA, position_years)]
final[, c('age_mismatch', 'sex_mismatch', 'position_mismatch', 'position_years_mismatch', 'patient_volume_mismatch', 'position_months', 'facilitator'):=NULL]
write.csv(final, paste0(out_dir, 'hp_pre_post_clean.csv'), row.names=F)

