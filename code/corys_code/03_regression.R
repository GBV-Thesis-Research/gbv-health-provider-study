#############################################################################################
#
# Author: Cory Spencer
# Purpose: Regression to test individual/training level factors associated with improvement
#
#############################################################################################
rm(list=ls())

#set up environment -------------------------------------------------------------------------
library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(rigr)
library(table1)
library(knitr)
library(lme4)

date <- gsub('-', '_', Sys.Date())

in_dir <- 'FILEPATH'
out_dir <- 'FILEPATH'

#set standard dem vars
dem.vars <- c('time_point', 'participant_id', 'municipality', 
              'date', 'facility', 'sex', 'position', 'age', 'position_years', 
              'previous_training', 'patient_volume', 'position_other')

# (1) Read in and prep data--------------------------------------------------------------------------------------------------------------------------------------

plot.domains <- data.table(domain=c('attitudes_12','attitudes_14',
                                    'confidence_16', 'knowledge_8',
                                    'system_support_15'),
                           plot.label=c('Unacceptability of violence',
                                        'Professional roles',
                                        'Confidence',
                                        'Warning signs of violence',
                                        'System support'))

pdata <- fread(paste0(in_dir, 'prop_data_long.csv'))[variable %in% c('improve')]

pdata <- pdata[domain %in% c('attitudes_12','attitudes_14',
                             'confidence_16', 'knowledge_8',
                             'system_support_15')]

exclusions <- fread(paste0(in_dir, 'prop_data_long.csv'))[variable=='pre_pct']
exclusions <- exclusions[domain %in% c('attitudes_12','attitudes_14',
                                       'confidence_16', 'knowledge_8',
                                       'system_support_15')]
exclusions <- exclusions[value>=0.9]
exclusions <- exclusions[, .(domain, participant_id)][, exclude:=1]
pdata <- merge(pdata, exclusions, by=c('domain', 'participant_id'), all.x=T)
pdata[, exclude:=ifelse(is.na(exclude), 0, 1)]
table(pdata$exclude, pdata$domain)
pdata <- pdata[exclude!=1]

reg.data <- copy(pdata)
reg.data <- reg.data[!(is.na(age) | is.na(sex) | is.na(position))]
reg.data[, position:=ifelse(!position %in% c('Medical doctor', 'Midwife', 'Nurse'), 'Other', position)]
reg.data[, age:=ifelse(age %in% c('45-54 years old', '55 years or older'), '45 years or older', age)]
reg.data[, age:=ifelse(age %in% c('Less than 25 years old', '25-34 years old'), 'Less than 35 years old', age)]

#create dummy vars
reg.data[, age_34to45:=ifelse(age=='35 to 44 years old', 1, 0)]
reg.data[, age_45older:=ifelse(age=='45 years or older', 1, 0)]
reg.data[, age_35older:=ifelse(age %in% c('35 to 44 years older', '45 years or older'), 1, 0)]
reg.data[, position_Nurse:=ifelse(position=='Nurse', 1, 0)]
reg.data[, position_Midwife:=ifelse(position=='Midwife', 1, 0)]
reg.data[, position_Other:=ifelse(position=='Other', 1, 0)]
reg.data[, female:=ifelse(sex=='Female', 1, 0)]
reg.data[, municipality_liquica:=ifelse(municipality=='Liquica', 1, 0)]
reg.data[, liquica:=ifelse(training_group=='Liquica R1 and R2', 1, 0)]
reg.data[, bazartete:=ifelse(training_group=='Bazartete R1 and R2', 1, 0)]
reg.data[, maubara1:=ifelse(training_group=='Maubara R1', 1, 0)]
reg.data[, maubara2:=ifelse(training_group=='Maubara R2', 1, 0)]
reg.data[, ermera1:=ifelse(training_group=='Ermera combined R1', 1, 0)]
reg.data[, ermera2:=ifelse(training_group=='Ermera combined R2', 1, 0)]
reg.data[, atsabe:=ifelse(training_group=='Atsabe', 1, 0)]
reg.data[, letefoho:=ifelse(training_group=='Letefoho', 1, 0)]
reg.data[, hatolia:=ifelse(training_group=='Hatolia', 1, 0)]
reg.data[, early:=ifelse(training_group %in% c('Liquica R1 and R2', 'Ermera combined R1', 'Bazartete R2', 'Bazartete R1'), 1, 0)]
reg.data[training_group %like% 'Bazartete', health_facility:='Bazartete']
reg.data[training_group %like% 'Liquica', health_facility:='Liquica']
reg.data[training_group %like% 'Maubara', health_facility:='Maubara']
reg.data[training_group %like% 'Ermera combined', health_facility:='Ermera combined']
reg.data[training_group %like% 'Hatolia', health_facility:='Hatolia']
reg.data[training_group %like% 'Letefoho', health_facility:='Letefoho']
reg.data[training_group %like% 'Atsabe', health_facility:='Atsabe']
reg.data[liquica==1 | ermera1==1 | bazartete==1, training_date:='2021-07'] #group august with september due to limited sample
# reg.data[bazartete==1, training_date:='2021-08']
reg.data[ermera2==1 | atsabe==1 | hatolia==1 | maubara1==1, training_date:='2021-09']
reg.data[letefoho==1 | maubara2==1, training_date:='2021-10']


# (2) Logistic regression for each construction (knowledge -- kmod; attitudes - amod; preparedness - pmod) -------------------------------------------------------------------------------------------------------------------------------------------------------------------

kmod <- glmer(value ~ female + age_35older + position_Nurse + position_Midwife + position_Other + municipality + as.factor(training_date) + (1 | participant_id), 
             data=reg.data[domain=='knowledge_8'],
             control = glmerControl(optimizer = 'bobyqa'),
             family='binomial')
summary(kmod)
summary_kmod <- summary(kmod)
kcof <- data.table(variable=row.names(summary_kmod$coefficients),summary_kmod$coefficients,N=nobs(mod), domain='knowledge')

amod <- glmer(value ~ female + age_35older + position_Nurse + position_Midwife + position_Other + municipality + as.factor(training_date) + as.factor(domain) + (1 | participant_id), 
              data=reg.data[domain %like% 'attitudes'],
              control = glmerControl(optimizer = 'bobyqa'),
              family='binomial')
summary(amod)
summary_amod <- summary(amod)
acof <- data.table(variable=row.names(summary_amod$coefficients),summary_amod$coefficients,N=nobs(mod),domain='attitudes')

pmod <- glmer(value ~ female + age_35older + position_Nurse + position_Midwife + position_Other + municipality + as.factor(training_date) + as.factor(domain) + (1 | participant_id), 
              data=reg.data[domain %in% c("confidence_16", "system_support_15")],
              control = glmerControl(optimizer = 'bobyqa'),
              family='binomial')
summary(pmod)
summary_pmod <- summary(pmod)
pcof <- data.table(variable=row.names(summary_pmod$coefficients),summary_pmod$coefficients,N=nobs(mod), domain='preparedness')

cof <- rbind(kcof, acof, pcof)

cof[,OR:=exp(Estimate)]
cof[,OR_lwr:=exp(Estimate-(1.96*`Std. Error`))]
cof[,OR_upr:=exp(Estimate+(1.96*`Std. Error`))]

write.csv(cof, paste0(out_dir, 'regression_output.csv'), row.names=F)

table <- copy(cof)
table[, OR_with_CI:=paste0(round(OR, digits=2), ' (', round(OR_lwr, digits=2), ', ', round(OR_upr, digits=2), ')')]
table[, pval:=round(`Pr(>|z|)`, digits=4)]
table[, estimate_sd:=paste0(round(Estimate, digits=2), ' (', round(`Std. Error`, digits=2), ')')]
table <- table[, .(variable, estimate_sd, pval, OR_with_CI, domain)]
write.csv(table, paste0(out_dir, 'regression_table_output.csv'), row.names=F)

#plot

maps <- data.table(variable=c('female', 'age_35older', 'position_Nurse', 'position_Midwife', 'position_Other', 'municipality_liquica', 'as.factor(training_date)2021-09',
                              'as.factor(training_date)2021-10'),
                   label=c('Female', 'Age 35 or older', 'Nurse', 'Midwife', 'Other', 'Liquica municipality', 'September 2021', 'October 2021'))

cof <- merge(cof, maps, by='variable')
cof[, label:=factor(label, levels=rev(c('Female', 'Age 35 or older', 'Nurse', 'Midwife', 'Other', 'Liquica municipality', 'September 2021', 'October 2021')))]
aesth <- theme_bw() + theme(axis.title = element_text(size=13,face='bold'),axis.text =element_text(size=13,face='plain'),plot.title =element_text(size=14,face='bold'),strip.background = element_rect(fill="white"),strip.text=element_text(size=10,face='bold'),legend.position = 'top')

pdf(paste0(out_dir, 'OR_plot.pdf'), 10, 9)

gg_reg <- ggplot() +
  geom_pointrange(data=cof,
                  aes(x=OR,xmax=OR_upr,xmin=OR_lwr,y=label), alpha=0.8, position=position_dodge(0.5))+
  geom_vline(xintercept=1,linetype='longdash') + 
  scale_x_log10()+
  theme(legend.position = 'none')+
  labs(x="OR (95%UI)",color='World Region', shape='Survey', title='Figure 3. Exponentiated coefficients from mixed effects logistic regression', y='Covariate')+
  scale_y_discrete(labels = function(x) str_wrap(x, width = 15), position='left')+
  guides(color = guide_legend(ncol = 1, order=1, reverse=TRUE), shape=guide_legend(ncol=1, order=2))+
  theme(axis.title.y=element_blank())+
  aesth
print(gg_reg)
dev.off()