#############################################################################################
#
# Author: Cory Spencer
# Purpose: Calculate cronbach's alpha for cleaned harmonia project pre/post data
#
#############################################################################################
rm(list=ls())

#set up environment -------------------------------------------------------------------------
library(data.table)
library(dplyr)
library(table1)
library(psych)

date <- gsub('-', '_', Sys.Date())

in_dir <- 'FILEPATH'

#set standard dem vars
dem.vars <- c('time_point', 'participant_id', 'municipality', 
              'date', 'facility', 'sex', 'position', 'age', 'position_years', 
              'previous_training', 'patient_volume', 'position_other')

data <- fread(paste0('FILEPATH/hp_pre_post_clean.csv'))
data <- data[time_point=='pre']

knowledge_7 <- data[, .SD, .SDcols=c(names(data)[names(data) %like% 'knowledge_7' & names(data)!='knowledge_7'])]
alpha(knowledge_7)

knowledge_8 <- data[, .SD, .SDcols=c(names(data)[names(data) %like% 'knowledge_8' & names(data)!='knowledge_8'])]
alpha(knowledge_8)

knowledge_9 <- data[, .SD, .SDcols=c(names(data)[names(data) %like% 'knowledge_9' & names(data)!='knowledge_9'])]
alpha(knowledge_9)

knowledge_10 <- data[, .SD, .SDcols=c(names(data)[names(data) %like% 'knowledge_10' & names(data)!='knowledge_10'])]
alpha(knowledge_10)

attitudes_11 <- data[, .SD, .SDcols=c(names(data)[names(data) %like% 'attitudes_11' & names(data)!='attitudes_11'])]
alpha(attitudes_11)

attitudes_12 <- data[, .SD, .SDcols=c(names(data)[names(data) %like% 'attitudes_12' & names(data)!='attitudes_12'])]
alpha(attitudes_12)

attitudes_13 <- data[, .SD, .SDcols=c(names(data)[names(data) %like% 'attitudes_13' & names(data)!='attitudes_13'])]
alpha(attitudes_13)

attitudes_14 <- data[, .SD, .SDcols=c(names(data)[names(data) %like% 'attitudes_14' & names(data)!='attitudes_14'])]
alpha(attitudes_14)

confidence_16 <- data[, .SD, .SDcols=c(names(data)[names(data) %like% 'confidence_16' & names(data)!='confidence_16'])]
alpha(confidence_16)

empathy_17 <- data[, .SD, .SDcols=c(names(data)[names(data) %like% 'empathy_17' & names(data)!='empathy_17'])]
alpha(empathy_17)

system_support_15 <- data[, .SD, .SDcols=c(names(data)[names(data) %like% 'system_support_15' & names(data)!='system_support_15'])]
alpha(system_support_15)




