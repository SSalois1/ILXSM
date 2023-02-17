# This script trys a few different types of condition estimates 

# -- Fultons K =  100* W /L^3 -- #
df21 <- df %>% filter(year == 2021)
df22 <- df %>% filter(year == 2022)

df21$fk <- 100 * (df21$weights/(df21$lengths^3))
df22$fk <- 100 * (df22$weights/(df22$lengths^3))
# Year specific condition
df21$cond <- (df21$weights/(mean(df21$weights)))
df22$cond <- (df22$weights/(mean(df22$weights)))
# Relative condition based on mean weight for pop
df21$pop_kn <- (df21$weights/(mean(dat$weights)))
df22$pop_kn <- (df22$weights/(mean(dat$weights)))
dat <- rbind(df21, df22)
# -- Relative Condition: kn = w/w' -- # 
# compute length-weight relationships based on 1978 lange_johnson study: 
dat$W <- 0.04810*(dat$lengths*.10)^2.71990
# Relative condtion based on predicted  nmju7t for pop
dat$Kn <- dat$weights/dat$W

write.csv(dat, 'ILXSM_condition_20212022.csv')

dat$lengths*.10
dat$lengths/10
dat <- read.csv('ILXSM_condition_20212022.csv')
dat <- dat[,-1]
# Boxplot of relative condition both years
# summarise the distribution of body condition of all individuals 
# during the sampling year
# Kn = W/W', W = weight of individual squid, 
# W' = predicted length-specific mean weight for the pop in a given region
# Here I used mean weight of population (not length specific)
g0 = ggplot(dat, 
            aes(x = factor(AREA_CODE), y = pop_kn, fill = as.factor(year)))+ 
  labs(x = 'Statistical Area', y = 'Relative Condition (Kn)',
       title = 'Relative Condition:',
       subtitle = 'Based on mean weight of population') +
  theme_classic() +
  guides(fill=guide_legend(title=NULL))+
  geom_hline(yintercept = 1, lty = 2) +
  ylim(0,2) +
  geom_boxplot()
ggsave(path = here::here('results'),"Relative_condition_by_stat_area.jpg", 
       width = 8, height = 3.75, units = "in", dpi = 300)
library(patchwork) 
g1 = ggplot(dat.check, 
       aes(x = factor(AREA_CODE), y = Kn, fill = as.factor(year)))+ 
  labs(x = 'Statistical Area', y = 'Relative Condition (Kn)',
       title = 'Relative Condition (Kn):',
       subtitle = 'Based on 1970s regression coefficients') +
  theme_classic() +
  guides(fill='none')+
  #guides(fill=guide_legend(title=NULL))+
  geom_hline(yintercept = 1, lty = 2) +
  ylim(0,2) +
  geom_boxplot() # %>% filter(Kn <= 4.5)
g2 = ggplot(dat.check %>% filter(Kn.updated <= 6), 
       aes(x = factor(AREA_CODE), y = Kn.updated, fill = as.factor(year)))+ 
  labs(x = 'Statistical Area', y = '',
       title = 'Relative Condition (Kn):',
       subtitle = 'Based on 2021/22 regression coefficients') +
  theme_classic() +
  guides(fill=guide_legend(title=NULL))+
  geom_hline(yintercept = 1, lty = 2) +
  geom_boxplot()
## Note - i changed the filter and/or ylim to play with scale, remove outliers 

g1 + g2
ggsave(path = here::here('results'),"kn_1970s_vs_kn_2122.jpg", 
       width = 10, height = 6, units = "in", dpi = 300)
g1 + g0  
ggsave(path = here::here('results'),"kn_vs_mean_condition_by_stat_area.jpg", 
       width = 10, height = 6, units = "in", dpi = 300)
g1 + g0  # with ylim(0,2)
ggsave(path = here::here('results'),"kn_vs_mean_condition_by_stat_area_zoom.jpg", 
       width = 10, height = 5, units = "in", dpi = 300)

# Here I used length-specific weight of population 
ggplot(dat, aes(x = factor(AREA_CODE), y = Kn, fill = as.factor(year)))+ 
  labs(x = 'Statistical Area', y = 'Relative Condition (Kn)',
       title = '2021 vs 2022') +
  theme_classic() +
  guides(fill=guide_legend(title=NULL))+
  geom_hline(yintercept = 1, lty = 2) +
  geom_boxplot()
# remove 626 as outlier and other outliers
#  2021 622 and 526
sa526 <- df21 %>% filter(AREA_CODE == 526)
sa622 <- df21 %>% filter(AREA_CODE == 622)
max(sa622$weights)
max(sa526$weights)
ggplot(dat %>% filter(AREA_CODE %in% c(622, 537, 632, 616, 526, 541, 623,
                                         525, 600, 621, 615, 562)),
       aes(x = factor(AREA_CODE), y = Kn, fill = as.factor(year)))+ 
  labs(x = 'Statistical Area', y = 'Relative Condition (Kn)',
       title = '2021 vs 2022') +
  theme_classic() +
  guides(fill=guide_legend(title=NULL))+
  geom_hline(yintercept = 1, lty = 2) +
  geom_boxplot()
ggplot(dat %>% filter(AREA_CODE %in% c(622, 537, 632, 616, 526, 541, 623,
                                       525, 600, 621, 615, 562) &
                        weights < 100),
       aes(x = factor(AREA_CODE), y = Kn, fill = as.factor(year)))+ 
  labs(x = 'Statistical Area', y = 'Relative Condition (Kn)',
       title = '2021 vs 2022') +
  theme_classic() +
  guides(fill=guide_legend(title=NULL))+
  geom_hline(yintercept = 1, lty = 2) +
  geom_boxplot()
ggsave(path = here::here('results'),"Relative_condition_Kn_no_outlier_by_stat_area.jpg", 
       width = 8, height = 3.75, units = "in", dpi = 300)

# 2021 boxplot of condition
ggplot(df21, aes(x = factor(AREA_CODE), y = cond2, fill = as.factor(AREA_CODE)))+ 
  labs(x = 'Statistical Area', y = 'Condition',
       title = '2021') +
  theme_classic() +
  guides(fill=guide_legend(title=NULL))+
  geom_hline(yintercept = 1, lty = 2) +
  geom_boxplot()
# 2022 boxplot of condition
ggplot(df22, aes(x = factor(AREA_CODE), y = cond2, fill = as.factor(AREA_CODE)))+ 
  labs(x = 'Statistical Area', y = 'Condition (k)',
       title = '2022') +
  theme_classic() +
  guides(fill=guide_legend(title=NULL))+
  geom_hline(yintercept = 1, lty = 2) +
  geom_boxplot()
# Year specific condition
ggplot(df22, aes(x = factor(AREA_CODE), y = cond, fill = as.factor(AREA_CODE)))+ 
  labs(x = 'Statistical Area', y = 'Condition (k)',
       title = '2022') +
  theme_classic() +
  guides(fill=guide_legend(title=NULL))+
  geom_hline(yintercept = 1, lty = 2) +
  geom_boxplot()

ggplot(df21, aes(x = factor(AREA_CODE), y = fk, fill = as.factor(AREA_CODE)))+ 
  labs(x = 'Statistical Area', y = 'Fulton Body Condition (k)',
       title = 'Fleet: Wet boats',
       subtitle = 'Lengths') +
  #scale_fill_manual(values = c("#F1BB7B","#FD6467")) +
  theme_classic() +
  guides(fill=guide_legend(title=NULL))+
  geom_boxplot()

ggplot(df21, aes(x = factor(hold_type), y = fk, fill = as.factor(hold_type)))+ 
  labs(x = 'Statistical Area', y = 'Fulton Body Condition (k)',
       title = 'Fleet: Wet boats') +
  #scale_fill_manual(values = c("#F1BB7B","#FD6467")) +
  theme_classic() +
  guides(fill=guide_legend(title=NULL))+
  geom_boxplot()

ggplot(df21 %>% filter(week %in% c(25:26)), 
       aes(x = factor(week), y = fk, fill = as.factor(hold_type)))+ 
  labs(x = 'Statistical Area', y = 'Fulton Body Condition (k)') +
  theme_classic() +
  guides(fill=guide_legend(title=NULL))+
  geom_boxplot()

ggplot(df21 %>% filter(hold_type %in% c('Ice', 'FT')), 
       aes(x = factor(week), y = fk, fill = as.factor(hold_type)))+ 
  labs(x = 'Statistical Area', y = 'Fulton Body Condition (k)') +
  theme_classic() +
  guides(fill=guide_legend(title=NULL))+
  geom_boxplot()

ggplot(df21 %>% filter(AREA_CODE %in% c(622, 537, 632, 616, 526, 541, 623,
                                        525, 600, 621, 615, 562)), 
       aes(x = factor(AREA_CODE), y = fk, fill = as.factor(hold_type)))+ 
  labs(x = 'Statistical Area', y = 'Fulton Body Condition (k)') +
  theme_classic() +
  guides(fill=guide_legend(title=NULL))+
  geom_boxplot()

# small: <80 mm ML; medium: 80-120 mm ML; large: >120 mm ML
ggplot(df21 %>% filter(lengths <= 80), 
       aes(x = factor(AREA_CODE), y = fk, fill = as.factor(AREA_CODE)))+ 
  labs(x = 'Statistical Area', y = 'Fulton Body Condition (k)', 
       title = 'Small') +
  theme_classic() +
  guides(fill=guide_legend(title=NULL))+
  geom_boxplot()

ggplot(df21 %>% filter(lengths <= 80), 
       aes(x = factor(AREA_CODE), y = fk, fill = as.factor(AREA_CODE)))+ 
  labs(x = 'Statistical Area', y = 'Fulton Body Condition (k)', 
       title = 'Small') +
  theme_classic() +
  guides(fill=guide_legend(title=NULL))+
  geom_boxplot()
#80- 120 interesting, <200 more like large
ggplot(df21 %>% filter(lengths > 80 & lengths < 200), 
       aes(x = factor(AREA_CODE), y = fk, fill = as.factor(AREA_CODE)))+ 
  labs(x = 'Statistical Area', y = 'Fulton Body Condition (k)', 
       title = 'Medium') +
  theme_classic() +
  guides(fill=guide_legend(title=NULL))+
  geom_boxplot()

ggplot(df21 %>% filter(lengths > 200), 
       aes(x = factor(AREA_CODE), y = fk, fill = as.factor(AREA_CODE)))+ 
  labs(x = 'Statistical Area', y = 'Fulton Body Condition (k)', 
       title = 'Large') +
  theme_classic() +
  guides(fill=guide_legend(title=NULL))+
  geom_boxplot()

ggplot(df21 %>% filter(lengths <= 80), 
       aes(x = factor(AREA_CODE), y = fk, fill = as.factor(AREA_CODE)))+ 
  labs(x = 'Statistical Area', y = 'Fulton Body Condition (k)', 
       title = 'Small') +
  theme_classic() +
  guides(fill=guide_legend(title=NULL))+
  geom_boxplot()

ggplot(k21 %>% filter(hold_type %in% c('RSW', 'Ice') & PARAM_VALUE_NUM <= 500), 
       aes(x = factor(vessel_id), y = PARAM_VALUE_NUM, fill = as.factor(year)))+ 
  labs(x = 'Vessel', y = 'Mantle length (mm)', 
       title = 'Fleet: Wet boats', 
       subtitle = 'Lengths') +
  scale_fill_manual(values = c("#F1BB7B","#FD6467")) +
  theme_classic() + 
  guides(fill=guide_legend(title=NULL))+
  geom_boxplot()