#############################################################################################
#
# Author: Cory Spencer
# Purpose: Test for pre/post differences using Wilcoxon for Harmonia Project pre/post analysis
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
in_dir <- paste0('FILEPATH', date, '/')
out_dir <- paste0('FILEPATH')
plot.domains <- data.table(domain=c('attitudes_12','attitudes_14',
                                    'confidence_16', 'knowledge_8',
                                    'system_support_15'),
                           plot.label=c('Unacceptability of violence',
                                        'Professional roles',
                                        'Confidence',
                                        'Warning signs of violence',
                                        'System support'))
#set standard dem vars
dem.vars <- c('time_point', 'participant_id', 'municipality', 
              'date', 'facility', 'sex', 'position', 'age', 'position_years', 
              'previous_training', 'patient_volume', 'position_other')

# (1) Read in prepped data scored proportional to 10 points --------------------------------------------------------------------------------------------------------------------------------------

pdata <- fread(paste0(in_dir, 'prop_data.csv'))[domain %in% unique(plot.domains$domain)]

#boxplots to check skew of data
data <- copy(pdata)
data <- melt.data.table(data, id.vars=c('domain', 'participant_id'), measure.vars=c('pre_pct', 'post_pct', 'change_pct'))

d <- 'attitudes_14'#replace with specific domain value interested in investigating
hist(data[domain==d & variable=='pre_pct']$value)
shapiro.test(data[domain==d & variable=='pre_pct']$value)
qqnorm(data[domain==d & variable=='pre_pct']$value)
boxplot(data[domain==d]$value ~ data[domain==d]$variable)
boxplot(data[domain==d & variable=='change_pct']$value*10)

#multiply scores by 10 and run tests for each domain
pdata[, pre_pct:=pre_pct*10]
pdata[, post_pct:=post_pct*10]
tests.list <- list()

for (d in unique(pdata$domain)){
  print(d)
  # test <- shapiro.test(pdata[domain==d]$pre_pct)
  test <- wilcoxon(pdata[domain==d]$pre_pct, pdata[domain==d]$post_pct, paired=TRUE, correct=TRUE, alternative='two.sided')
  test2 <- data.table()
  test2$p.value <- test$p.value
  test2$domain <- d
  test2$n_pt <- length(unique(pdata[domain==d]$participant_id))
  test2$pre_median <- median(pdata[domain==d]$pre_pct, na.rm=T)
  test2$pre_IQR_lower <- quantile(pdata[domain==d]$pre_pct, 0.25, na.rm=T)
  test2$pre_IQR_upper <- quantile(pdata[domain==d]$pre_pct, 0.75, na.rm=T)
  test2$post_median <- median(pdata[domain==d]$post_pct, na.rm=T)
  test2$post_IQR_lower <- quantile(pdata[domain==d]$post_pct, 0.25, na.rm=T)
  test2$post_IQR_upper <- quantile(pdata[domain==d]$post_pct, 0.75, na.rm=T)
  tests.list[[length(tests.list)+1]] <- test2
}
tests <- rbindlist(tests.list, fill=T)

tests$p_holm_adj <- p.adjust(tests$p.value, method='holm')

write.csv(tests, 'FILEPATH')


