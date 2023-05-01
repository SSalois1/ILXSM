library(dplyr)
library(pwr)
source('ilxsm_data.R')
## Sample data - length/weight
ml <- isolate_lengths('ILXSM_EntirePull_Wdealerinfo_3_14_21.csv','groundfish_stat_areas')
wt <- isolate_weights('ILXSM_EntirePull_Wdealerinfo_3_14_21.csv','groundfish_stat_areas')
paired <- pair_length_weight(ml, wt)
## maturity data
setwd(here::here('data/'))
prevail <- read.csv('maturity_data_prevail.csv')
seafarer <- read.csv('maturity_data_seafarer.csv')
prevail$mat_stage_f <- prevail$ngl/prevail$ml
seafarer$mat_stage_f<- seafarer$ngl/seafarer$ml
# merge the data sets 
df <- rbind(prevail, seafarer)
df$date <-lubridate::mdy(df$date)
df <- df %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         week = lubridate::week(date), 
         day = lubridate::day(date))

pwr.r.test(r=0.8, sig.level=0.05, power=0.80)


# estimate sample size via power analysis
# parameters for power analysis
effect = 0.8
alpha = 0.05
power = 0.8
# perform power analysis

h = 2*asin(sqrt(0.00495))-2*asin(sqrt(0.00490))=0.0007

w = √(Χ 2 /(n*df))= √(8/(200*3))=0.115

pwr.r.test(r=0.37, sig.level=0.05, power=0.80)
pwr.r.test(r=0.6, sig.level=0.05, power=0.80)
pwr.r.test(r=0.5, sig.level=0.05, power=0.80)
pwr.r.test(r=0.8, sig.level=0.05, power=0.80)

wp.mrt2arm(f=0.25, J=100, tau11=0.5, sg2=1.0, alpha=0.05, power=0.80, alternative="two.sided")
# Determining effect size from ILXSM data
# 0.2 = small effect, 0.5 medium effect size, 0.8 large effect size

library(lsr)
# by year, need to do by week 
lw.21 <- paired %>% filter(year == 2021) 
lw.22 <- paired %>% filter(year == 2022) 
lw.23 <- paired %>% filter(year == 2023) 
cohensD(lw.21$length,lw.22$length)
# average length of squid in 2021 is 
print('average length of squid in 2021 vs 2022 is:')
cohensD(lw.21$length,lw.22$length)
pwr.r.test(r=0.7691352, sig.level=0.05, power=0.80)
# n = 10.02439
cohensD(lw.21$weight,lw.22$weight)
pwr.r.test(r=0.5674991, sig.level=0.05, power=0.80)
# n = 21
print('average length of squid in 2022 vs 2023 is:')
cohensD(lw.22$length,lw.23$length)
pwr.r.test(r=0.09965282, sig.level=0.05, power=0.80)
# n = 787.23
