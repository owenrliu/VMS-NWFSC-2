# Investigating the number of crab pots per linear mile (i.e., pot spacing)
# For WA and OR

#-----------------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(here)
library(sf)

# ggplot theme
plot_theme <-   theme_minimal()+
  theme(text=element_text(family="sans",size=12,color="black"),
        legend.text = element_text(size=14),
        axis.title=element_text(family="sans",size=14,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        axis.text.x.bottom = element_text(angle=45),
        legend.position = c(0.8,0.3),
        title=element_text(size=12),
        legend.title = element_text(size=10),
        panel.grid.major = element_line(color="gray50",linetype=3))
theme_set(plot_theme)
options(dplyr.summarise.inform = FALSE)

#-----------------------------------------------------------------------------------

# WASHINGTON

# We will investigate pot spacing as the length of stringline divided by the number of pots reported on it

# we use the form of logbook data that is not yet summarised on a grid level (but pot are simualted along stringline)

# for now we use the version for WA logbook data where too long (>80km) and too short stringlines (0m and >50 pot reported) are RETAINED
# if these are removed before the following code, that doesn't make much difference, 
# and various filters will get used in the following code to deal with any outliers


# use the 'path' approach as data is in raimbow project/repo
path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/traps_g_license_all_logs_2009_2020_too short long flagged not deleted.rds"
traps_g_WA_raw <- readRDS(path.fish_WA) 

# remove geometry, create columns for season, month
traps_g_WA <- traps_g_WA_raw %>% 
  st_set_geometry(NULL) %>% 
  mutate(
    season = str_sub(SetID,1,9),
    year = year(SetDate),
    month_name = month(SetDate, label=TRUE, abbr = FALSE),
    season_month = paste0(season,"_",month_name)) %>% 
  #VLM timeline is basically 2008-09 to 2018-19 seasons
  #we don't have 2008-09 season for WA, but we can exclude 2019-20 to try to match the VLM timeline as best we can
  filter(!season =='2019-2020')


# Read in and join license & pot limit info (in raimbow project/repo)
path.trap_tier_info_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/WA_pot_limit_info_May2021.csv"
trap_tier_info_WA_raw <- read.csv(path.trap_tier_info_WA) 

WA_pot_limit_info <- trap_tier_info_WA_raw %>% 
  rename(License = License_ID)

# join Pot_Limit to traps_g 
traps_g_WA_pot_tier <- traps_g_WA %>% 
  left_join(WA_pot_limit_info,by=c("License")) 
  #we are missing lot of license/pot tier info between 2009-2012, + only couple cases in 2015-2017
  #drop_na(Pot_Limit) # if want to drop NAs


# this RDS has pots simulated along the stringline (each row is an individual simulated pot) - the simulated pots aren't actually necessary, 
# we only need the length of the line and the reported "PotsFished" - remove duplicated rows based on SetID
# WRONG
#traps_g_WA_v2 <- traps_g_WA_pot_tier %>% distinct(SetID, .keep_all = TRUE)

#remove one pot from each stringline - when calcualting spacing, the first pot doesn't actually have spacing
# and should therefore be removed
traps_g_WA_pot_tier_v2 <- traps_g_WA_pot_tier %>% 
  group_by(SetID) %>%
  slice(2:n()) %>% 
  ungroup() %>% 
  #then create a new PotsFished column that reflects that one pot that was removed
  mutate(PotsFished_one = PotsFished - 1)

traps_g_WA_pot_tier_v3 <- traps_g_WA_pot_tier_v2 %>% 
  filter(line_length_m > 0.1) %>%  #van't get pot spacing if line length is 0
  rowwise() %>% 
  #now we can calculate the average spacing in each line
  mutate(spacing_in_m = (sum(line_length_m)) / (sum(PotsFished_one)) )


#Kernel density plot if not filtered 
pot_spacing_density <- traps_g_WA_pot_tier_v3 %>% 
  ggplot(aes(spacing_in_m))+
  #ggplot(aes(spacing_in_m,fill=as.factor(Pot_Limit),as.factor(Pot_Limit)))+
  geom_density(alpha=0.5)+
  labs(x="Pot spacing (m)",y="Kernel Density",fill="Pot limit group",col="Pot limit group")+
  #facet_wrap(~coast_region,nrow=4,scales = 'free_y')+
  #facet_wrap(~ Pot_Limit,scales = 'free_y')+
  #geom_vline(xintercept=-40*1.829,col='red',linetype=2)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text.y=element_blank())
pot_spacing_density

pot_spacing_WA_bins <-  traps_g_WA_pot_tier_v3 %>% 
  mutate(pot_spacing_bins = cut(spacing_in_m, breaks = c(0, 250, 500, 1000, max(spacing_in_m))))

plot1 <- pot_spacing_WA_bins %>%
  ggplot() + 
  geom_bar(aes(x=pot_spacing_bins, y = (..count..)/sum(..count..))) +
  #facet_wrap(~ Pot_Limit) +
  labs(x="pot spacing bin (m)",y="proportion") +
  ggtitle('WA - spacing between pots (in m)') +
  theme_minimal()
plot1
#-----------------------------------------------------------------------------------

# investigate pot spacing

traps_g_WA_v2_adjusted_pots <-  traps_g_WA_v2 %>% 
  mutate(PotsFished_one = PotsFished - 1) # PotsFished - 1 exists because we have to discount the anchor point

new_pot_spacing_WA <- traps_g_WA_v2_adjusted_pots %>% 
  summarise(pot_spacing = (sum(line_length_m)) / (sum(PotsFished_one)) )




















# remove strinlines that are 0m as can't get spacing for those
pot_spacing_WA <-  traps_g_WA_v2 %>% 
  filter(line_length_m > 0.1) %>% 
  mutate(spacing_in_m = line_length_m/(PotsFished-1))


# there are few cases of really long pot spacing
# large spacings occur when few pots reported on a very long string
# e.g. the case of max spacing of 16472m is a case of 1 pot on a 16472m stringline
# or the next longest spacing 9511m (2 pots on a 19023m stringline)
# so we want to make a call of where to cut the data and acknolwedge that the reported info in logbooks
# may not always be fully trustworthy

pot_spacing_WA_bins <-  pot_spacing_WA %>% 
  mutate(pot_spacing_bins = cut(spacing_in_m, breaks = c(0, 250, 500, 1000, max(spacing_in_m))))

plot1 <- pot_spacing_WA_bins %>%
  ggplot() + 
  geom_bar(aes(x=pot_spacing_bins, y = (..count..)/sum(..count..))) +
  facet_wrap(~ Pot_Limit) +
  labs(x="pot spacing bin (m)",y="proportion") +
  ggtitle('WA - spacing between pots (in m)') +
  theme_minimal()
plot1



# summary of pot spacings across 2009-10 to 2018-19
summary_pot_spacing_WA_bins <- pot_spacing_WA_bins %>% 
  group_by(Pot_Limit) %>% 
  summarise(n_records = n(),
            n_0_250m = length(pot_spacing_bins[pot_spacing_bins == "(0,250]"]),
            n_250_500m = length(pot_spacing_bins[pot_spacing_bins == "(250,500]"]),
            n_500_1000m = length(pot_spacing_bins[pot_spacing_bins == "(500,1e+03]"]),
            n_1000m_plus = length(pot_spacing_bins[pot_spacing_bins == "(1e+03,1.65e+04]"])
  ) %>% 
  mutate(percent_0_250m = (n_0_250m/n_records)*100,
         percent_250_500m = (n_250_500m/n_records)*100,
         percent_500_1000m = (n_500_1000m/n_records)*100,
         percent_1000m_plus = (n_1000m_plus/n_records)*100
  ) %>% 
  select(percent_0_250m:percent_1000m_plus)
# ALL DATA:
# percent_0_250m percent_250_500m percent_500_1000m percent_1000m_plus
#   95.9             3.04             0.778              0.318
# ~96% of pot spacing is 0-250m between pots, or ~99% is within 0-500m between pots, regardless of trap tier
# so lets just exclude cases where spacing between pots is >500m



# there is also probably a min distance that the pots would need to be
# lets look at 0-10m spacing subset
pot_tier_300 <- pot_spacing_WA %>% 
  filter(spacing_in_m <= 10) %>% 
  filter(Pot_Limit == 300)
hist(pot_tier_300$spacing_in_m)

pot_tier_500 <- pot_spacing_WA %>% 
  filter(spacing_in_m <= 10) %>% 
  filter(Pot_Limit == 500)
hist(pot_tier_500$spacing_in_m)

pot_tier_NA <- pot_spacing_WA %>% 
  filter(spacing_in_m <= 10) %>% 
  filter(is.na(Pot_Limit))
hist(pot_tier_NA$spacing_in_m)
# spike between 0-2m spacing for 300 & 500 pot tiers (but less for NA)
# let's for now exclude spacing that is <2m or >500m

#Kernel density plot if not filtered 
pot_spacing_density <- pot_spacing_WA %>% 
  ggplot(aes(spacing_in_m))+
  #ggplot(aes(spacing_in_m,fill=as.factor(Pot_Limit),as.factor(Pot_Limit)))+
  geom_density(alpha=0.5)+
  labs(x="Pot spacing (m)",y="Kernel Density",fill="Pot limit group",col="Pot limit group")+
  #facet_wrap(~coast_region,nrow=4,scales = 'free_y')+
  facet_wrap(~ Pot_Limit,scales = 'free_y')+
  #geom_vline(xintercept=-40*1.829,col='red',linetype=2)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text.y=element_blank())
pot_spacing_density

pot_spacing_WA_subset <- pot_spacing_WA %>% 
  filter(spacing_in_m > 2 &  spacing_in_m <= 500)
#or try less conservative cutoffs
pot_spacing_WA_subset <- pot_spacing_WA %>% 
  filter(spacing_in_m > 5 &  spacing_in_m <= 200)

#Kernel density plot if  filtered 
pot_spacing_density <- pot_spacing_WA_subset %>% 
  ggplot(aes(spacing_in_m))+
  #ggplot(aes(spacing_in_m,fill=as.factor(Pot_Limit),as.factor(Pot_Limit)))+
  geom_density(alpha=0.5)+
  labs(x="Pot spacing (m)",y="Kernel Density",fill="Pot limit group",col="Pot limit group")+
  #facet_wrap(~coast_region,nrow=4,scales = 'free_y')+
  facet_wrap(~ Pot_Limit,scales = 'free_y')+
  #geom_vline(xintercept=-40*1.829,col='red',linetype=2)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text.y=element_blank())
pot_spacing_density
#-----------------------------------

# plot of pot spacing across across 2009-10 to 2018-19
WA_pot_spacing_p1 <- pot_spacing_WA_subset %>% 
  ggplot() + 
  geom_bar(aes(x=spacing_in_m, y=stat(prop)), position = "dodge") +
  scale_x_binned(breaks=seq(0, 500, 50),limits=c(0,500)) + 
  #facet_wrap(~ Pot_Limit) +
  #scale_y_continuous(breaks=seq(0, 0.5, 0.05),limits=c(0,0.5))+
  labs(x="Spacing between pots (m)",y="Proportion") +
  ggtitle('WA - spacing between pots (m)')
WA_pot_spacing_p1


summary_pot_spacing_WA_subset <- pot_spacing_WA_subset %>% 
  #group_by(Pot_Limit) %>% #Pot_Limit #season
  summarise(min_spacing = min(spacing_in_m, na.rm=TRUE),
            max_spacing = max(spacing_in_m, na.rm=TRUE),
            sd_spacing = sd(spacing_in_m, na.rm=TRUE), 
            mean_spacing = mean(spacing_in_m, na.rm=TRUE),
            median_spacing = median(spacing_in_m, na.rm=TRUE)
            )
# Washington
# ALL DATA
#             min_spacing  max_spacing  sd_spacing  mean_spacing  median_spacing
#ALL SEASONS      2.0        499.7       59.4        118.1           109.9

#2009-2010        2.1        498.9       59.5        127.5           121.0
#2010-2011        2.4        498.1       57.6        128.2           120.9
#2011-2012        2.62        498.       60.2         124.           117.
#2012-2013        2.31        499.       68.4         121.           109.
#2013-2014        2.04        496.       57.2         116.           107.
#2014-2015        3.09        499.       59.2         117.           111.
#2015-2016        2.11        499.       58.1         116.           108.
#2016-2017        2.41        495.       55.0         114.           108.
#2017-2018        2.14        500.       59.6         112.           102.
#2018-2019        2.03        496.       56.2         113.           104.


# By pot tier
#300              2.0        499.4       58.1         99.3           89.6
#500              2.1        499.7       58.4        122.3           113.0
#NA               2.1        498.9       59.0        126.6           119.7



# Translating spacing_in_m into pots/mile
pots_per_mile_WA_subset <- pot_spacing_WA_subset %>% 
  mutate(pots_per_mile = 1000/spacing_in_m/0.621371)

WA_pots_per_mile_p1 <- pots_per_mile_WA_subset %>% 
  ggplot() + 
  geom_bar(aes(x=pots_per_mile, y=stat(prop)), position = "dodge") +
  scale_x_binned(breaks=seq(0, 100, 10),limits=c(0,100)) + 
  #facet_wrap(~ Pot_Limit) +
  #scale_y_continuous(breaks=seq(0, 0.5, 0.05),limits=c(0,0.5))+
  labs(x="Pots per mile",y="Proportion") +
  ggtitle('WA - pots per mile')
WA_pots_per_mile_p1

#Kernel density plot of filtered pots/mile variable
pots_per_mile_density <- pots_per_mile_WA_subset %>% 
  ggplot(aes(pots_per_mile))+
  #ggplot(aes(spacing_in_m,fill=as.factor(Pot_Limit),as.factor(Pot_Limit)))+
  geom_density(alpha=0.5)+
  labs(x="Pots per mile",y="Kernel Density",fill="Pot limit group",col="Pot limit group")+
  #facet_wrap(~coast_region,nrow=4,scales = 'free_y')+
  facet_wrap(~ Pot_Limit,scales = 'free_y')+
  #geom_vline(xintercept=-40*1.829,col='red',linetype=2)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text.y=element_blank())
pots_per_mile_density

summary_pots_per_mile_WA_subset <- pots_per_mile_WA_subset %>% 
  group_by(season) %>% #Pot_Limit #season
  summarise(min_pots_pre_mile = min(pots_per_mile, na.rm=TRUE),
            max_pots_pre_mile = max(pots_per_mile, na.rm=TRUE),
            sd_pots_pre_mile = sd(pots_per_mile, na.rm=TRUE), 
            mean_pots_pre_mile = mean(pots_per_mile, na.rm=TRUE),
            median_pots_pre_mile = median(pots_per_mile, na.rm=TRUE)
  )
# min_pots_pre_mile  max_pots_pre_mile   sd_pots_pre_mile   mean_pots_pre_mile   median_pots_pre~
#     3.22                  792.              26.3               18.8             14.6


#-----------------------------------------------------------------------------------

# OREGON

# We will investigate pot spacing as the length of stringline divided by the number of pots reported on it

# we use the form of logbook data that is not yet summarised on a grid level (but pot are simulated along stringline)
# the current input RDS cover all OR data we have so far (2007-2018)  

# The data has been Filtered by ODFW Spatial Flag - as per ODFW instrutions

# use the 'path' approach as data is in raimbow project
path.fish_OR <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/OR/OR_traps_g_all_logs_2007_2018_SpatialFlag_filtered.rds"
traps_g_OR_raw <- readRDS(path.fish_OR) 

# remove geometry, create columns for season, month
traps_g_OR <- traps_g_OR_raw %>% 
  st_set_geometry(NULL) %>% 
  mutate(
    season = str_sub(SetID,1,9),
    year = year(SetDate),
    month_name = month(SetDate, label=TRUE, abbr = FALSE),
    season_month = paste0(season,"_",month_name)) %>% 
  #VLM timeline is basically 2008-09 to 2018-19 seasons
  #we don't have 2018-19 season for OR, but we can exclude 2007-08 to try to match the VLM timeline as best we can
  filter(!season =='2007-2008')
#pot tier limit is already included in the OR data


# this RDS has pots simulated along the stringline (each row is an individual simulated pot) - the simulated pots aren't actually necessary, 
# we only need the length of the line and the reported "PotsFished" - remove duplicated rows based on SetID
traps_g_OR_v2 <- traps_g_OR %>% distinct(SetID, .keep_all = TRUE)

#-----------------------------------------------------------------------------------

# investigate pot spacing

# remove strinlines that are 0m as can't get spacing for those
pot_spacing_OR <-  traps_g_OR_v2 %>% 
  filter(line_length_m > 0.1) %>% 
  mutate(spacing_in_m = line_length_m/PotsFished)


# there are few cases of really long pot spacing
# large spacings likely due to cases where few pots reported on a very long string
# e.g. the case of max spacing of 14579m is a case of 1 pot on a 14579m stringline
# or the next longest spacing 8857m (1 pot on a 8857m stringline)
# so we want to make a call of where to cut the data and say that the reported info in logbook
# isn't fully trustworthy

pot_spacing_OR_bins <-  pot_spacing_OR %>% 
  mutate(pot_spacing_bins = cut(spacing_in_m, breaks = c(0, 250, 500, 1000, max(spacing_in_m))))

plot1 <- pot_spacing_OR_bins %>%
  ggplot() + 
  geom_bar(aes(x=pot_spacing_bins, y = (..count..)/sum(..count..))) +
  facet_wrap(~ Potlimit) +
  labs(x="pot spacing bin (m)",y="proportion") +
  ggtitle('OR - spacing between pots (in m)') +
  theme_minimal()
plot1


# summary of pot spacings across all of 2007-2018
summary_pot_spacing_OR_bins <- pot_spacing_OR_bins %>% 
  summarise(n_records = n(),
            n_0_250m = length(pot_spacing_bins[pot_spacing_bins == "(0,250]"]),
            n_250_500m = length(pot_spacing_bins[pot_spacing_bins == "(250,500]"]),
            n_500_1000m = length(pot_spacing_bins[pot_spacing_bins == "(500,1e+03]"]),
            n_1000m_plus = length(pot_spacing_bins[pot_spacing_bins == "(1e+03,1.65e+04]"])
  ) %>% 
  mutate(percent_0_250m = (n_0_250m/n_records)*100,
         percent_250_500m = (n_250_500m/n_records)*100,
         percent_500_1000m = (n_500_1000m/n_records)*100,
         percent_1000m_plus = (n_1000m_plus/n_records)*100
  ) %>% 
  select(percent_0_250m:percent_1000m_plus)
# percent_0_250m percent_250_500m percent_500_1000m percent_1000m_plus
#   95.5             3.55             0.662              0
# 95.5% of pot spacing is 0-250m between pots, or 99% is within 0-500m between pots
# so lets just exclude cases where spacing between pots is >500m



# there is also probably a min distance that the pots would need to be
# lets look at 0-10m spacing subset
test <- pot_spacing_OR %>% 
  filter(spacing_in_m <= 10)
hist(test$spacing_in_m)
# spike at 4m  and 6m spacing - quite different from WA
# let's for now do the same as in WA and exclude spacing that is <2m or >500m


pot_spacing_OR_subset <- pot_spacing_OR %>% 
  filter(spacing_in_m > 2 &  spacing_in_m <= 500)


# plot of pot spacing across all of 2013-2020
OR_pot_spacing_p1 <- pot_spacing_OR_subset %>% 
  ggplot() + 
  geom_bar(aes(x=spacing_in_m, y=stat(prop)), position = "dodge") +
  scale_x_binned(breaks=seq(0, 500, 50),limits=c(0,500)) + 
  #scale_y_continuous(breaks=seq(0, 0.5, 0.05),limits=c(0,0.5))+
  labs(x="Spacing between pots (m)",y="Proportion") +
  ggtitle('OR - spacing between pots (m)')
OR_pot_spacing_p1


summary_pot_spacing_OR_subset <- pot_spacing_OR_subset %>% 
  summarise(min_spacing = min(spacing_in_m, na.rm=TRUE),
            max_spacing = max(spacing_in_m, na.rm=TRUE),
            sd_spacing = sd(spacing_in_m, na.rm=TRUE), 
            mean_spacing = mean(spacing_in_m, na.rm=TRUE),
            median_spacing = median(spacing_in_m, na.rm=TRUE)
  )
# Oregon
# min_spacing  max_spacing  sd_spacing  mean_spacing  median_spacing
#   2.01         499.25       55.64       138.32        132.65





