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

# we use the form of logbook data that is not yet summarised on a grid level (but pots are simulated along stringline)

# for now we use the version for WA logbook data where too long (>80km) and too short stringlines (0m and >50 pot reported) are RETAINED
# various filters will get used in the following code to deal with any outliers
# (if too short/long strings are removed before the following code, that doesn't make much of a difference) 


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

#--------------------------

# remove one pot from each stringline 
# when calculating spacing, the first pot doesn't actually have spacing and should therefore be removed
traps_g_WA_pot_tier_v2 <- traps_g_WA_pot_tier %>% 
  group_by(SetID) %>%
  slice(2:n()) %>% 
  ungroup() %>% 
  #then create a new PotsFished column that reflects that one pot that was removed
  mutate(PotsFished_one = PotsFished - 1)

traps_g_WA_pot_tier_v3 <- traps_g_WA_pot_tier_v2 %>% 
  #there are line lengths of 0 that can be filtered out here, but also later
  #can't get pot spacing if line length is 0
  filter(!line_length_m == 0) %>%  
  rowwise() %>% 
  #now we can calculate the average spacing in each line, using the following method so as to not take averages of averages
  mutate(spacing_in_m = (sum(line_length_m)) / (sum(PotsFished_one)) )


# there are cases of really short pot spacing when a short string line had reportedly lots of pots 
# e.g. 74m string with 500 pots
# there are  cases of really long pot spacing when few pots reported on a very long string
# e.g. 2 post on a 19km string line
# also infinite values when originally reported 1 pot on a string becomes 0 (see above)
# --> would still have been a long string line with very few pots even if had kept PotsFished as 1, as PotsFished = 1 string lines >1km long
# so we want to make a call of where to cut the data and acknowledge that the reported info in logbooks may not always be fully accurate


hist(traps_g_WA_pot_tier_v3$spacing_in_m)

# pot_spacing_WA_bins <-  traps_g_WA_pot_tier_v3 %>% 
#   mutate(pot_spacing_bins = ifelse(spacing_in_m < 300, 'less_than_300', 'more_than_300'))

pot_spacing_WA_subset <- traps_g_WA_pot_tier_v3  %>%   
  ###here can make assumption of reasonable min and max dist between pots 
  ###~99% of pot spacings <300m
  filter(spacing_in_m > 3 &  spacing_in_m <= 300)

hist(pot_spacing_WA_subset$spacing_in_m)

## additional ways to look at max cut off point
# pots_by_spacing <- pot_spacing_WA_subset %>%
#   count(spacing_in_m) %>% 
#   ungroup() %>% 
#   rename(pots=n) %>% 
#   # do cumulative counts
#   arrange(spacing_in_m) %>% 
#   mutate(cumulative_pots=cumsum(pots),perc_pots=cumulative_pots/last(cumulative_pots)*100)
# glimpse(pots_by_spacing)
# 
# dist_pots_spacing <- pots_by_spacing %>% 
#   ggplot(aes(x=spacing_in_m ,y=perc_pots))+
#   geom_line(size=1)+
#   geom_hline(aes(yintercept = 99), colour="blue", linetype=2)+
#   labs(x="Pot spacing (m)",y="Cumulative % Traps") +
#   ggtitle("Distribution of crab pots by pot spacing") + 
#   theme(legend.position = ("top"),legend.title=element_blank())
# dist_pots_spacing

#--------------------------

# summarise
# mean and other summaries will depend on cutoff values (min spacing >2m or >3m, max spacing <300m or <250m)

# #trying to use summarise() command just takes really long time, and is not working properly...

#all seasons, all vessels (without grouping)
mean(pot_spacing_WA_subset$spacing_in_m, na.rm = TRUE) 
#109.5261m --> 14.36pots/mile
1000/(mean(pot_spacing_WA_subset$spacing_in_m))/0.621371
#14.6937 pots/mile

median(pot_spacing_WA_subset$spacing_in_m, na.rm = TRUE)
#105.7899
1000/(median(pot_spacing_WA_subset$spacing_in_m))/0.621371
#15.21265

sd(pot_spacing_WA_subset$spacing_in_m, na.rm = TRUE) 
#47.43304
1000/(sd(pot_spacing_WA_subset$spacing_in_m))/0.621371
#33.92877

min(pot_spacing_WA_subset$spacing_in_m, na.rm = TRUE) 
#3.025228
1000/(min(pot_spacing_WA_subset$spacing_in_m))/0.621371
#531.9746

max(pot_spacing_WA_subset$spacing_in_m, na.rm = TRUE) 
#299.9994
1000/(max(pot_spacing_WA_subset$spacing_in_m))/0.621371
#5.364493

hist(pot_spacing_WA_subset$spacing_in_m)

# pot_spacing_density <- pot_spacing_WA_subset %>% 
#   ggplot(aes(spacing_in_m))+
#   geom_density(alpha=0.5)+
#   labs(x="Pot spacing (m)",y="Kernel Density",fill="Pot limit group",col="Pot limit group")+
#   theme(panel.grid.major=element_blank(),
#         panel.grid.minor=element_blank(),
#         axis.text.y=element_blank())
# pot_spacing_density


#-----------

#All vessels, by season

summary_all_vessels_by_season <-  pot_spacing_WA_subset %>%
  group_by(season) %>% 
  summarise(mean_spacing = mean(spacing_in_m),
            median_spacing = median(spacing_in_m),
            min_spacing = min(spacing_in_m),
            max_spacing = max(spacing_in_m),
            sd_spacing = sd(spacing_in_m),
            #how these translate to pots/mile
            mean_pots_per_mile = 1000/mean_spacing/0.621371,
            median_pots_per_mile = 1000/median_spacing/0.621371,
            min_pots_per_mile = 1000/min_spacing/0.621371,
            max_pots_per_mile = 1000/max_spacing/0.621371,
            sd_pots_per_mile = 1000/sd_spacing/0.621371
  )

ggplot(pot_spacing_WA_subset, aes(x=as.factor(season), y=spacing_in_m)) + 
  geom_boxplot()

box_all_vessels_by_season <- ggplot(pot_spacing_WA_subset, aes(x=as.factor(season), y=spacing_in_m)) + 
  geom_boxplot()
box_all_vessels_by_season


#-----------

#All seasons, by pot limit

summary_all_seasons_by_pot_limit <-  pot_spacing_WA_subset %>%
  group_by(Pot_Limit) %>% 
  summarise(mean_spacing = mean(spacing_in_m),
            median_spacing = median(spacing_in_m),
            min_spacing = min(spacing_in_m),
            max_spacing = max(spacing_in_m),
            sd_spacing = sd(spacing_in_m),
            #how these translate to pots/mile
            mean_pots_per_mile = 1000/mean_spacing/0.621371,
            median_pots_per_mile = 1000/median_spacing/0.621371,
            min_pots_per_mile = 1000/min_spacing/0.621371,
            max_pots_per_mile = 1000/max_spacing/0.621371,
            sd_pots_per_mile = 1000/sd_spacing/0.621371
  )


ggplot(pot_spacing_WA_subset, aes(x=as.factor(Pot_Limit), y=spacing_in_m)) + 
  geom_boxplot()

pot_spacing_density <- pot_spacing_WA_subset %>% 
  ggplot(aes(spacing_in_m))+
  geom_density(alpha=0.5)+
  labs(x="Pot spacing (m)",y="Kernel Density",fill="Pot limit group",col="Pot limit group")+
  facet_wrap(~ Pot_Limit,scales = 'free_y')+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text.y=element_blank())
pot_spacing_density

WA_pot_spacing_p1 <- pot_spacing_WA_subset %>% 
  ggplot() + 
  geom_bar(aes(x=spacing_in_m, y=stat(prop)), position = "dodge") +
  scale_x_binned(breaks=seq(0, 500, 25),limits=c(0,500)) + 
  facet_wrap(~ Pot_Limit) +
  #scale_y_continuous(breaks=seq(0, 0.5, 0.05),limits=c(0,0.5))+
  labs(x="Spacing between pots (m)",y="Proportion") +
  ggtitle('WA - spacing between pots (m)')
WA_pot_spacing_p1



#-----------

# by season and by pot limit

pot_spacing_WA_subset %>% 
  group_by(Pot_Limit, season) %>% 
  summarise(mean_spacing = mean(spacing_in_m),
            mean_pots_per_mile = 1000/(mean(spacing_in_m))/0.621371
  )


ggplot(pot_spacing_WA_subset, aes(x=as.factor(season), y=spacing_in_m, fill=as.factor(Pot_Limit))) + 
  geom_boxplot() +
  theme(legend.position="bottom")


# by MONTH and by pot limit
testx <- pot_spacing_WA_subset %>% 
  mutate(month_name = factor(month_name, levels = c('December','January','February','March','April','May','June','July','August','September','October','November'))) %>%
  filter(!is.na(month_name)) %>% 
  group_by(Pot_Limit, month_name) %>% 
  summarise(mean_spacing = mean(spacing_in_m),
            mean_pots_per_mile = 1000/(mean(spacing_in_m))/0.621371
  )


spacing_by_month <- testx %>%
  ggplot(aes(x=month_name ,y=mean_pots_per_mile, colour=as.factor(Pot_Limit), group=as.factor(Pot_Limit)))+
  geom_line(size=1)+
  labs(x="Month",y="Pots/mile") +
  ggtitle("Mean pots/mile in WA by month and by pot tier group 2009-10 to 2018-19") +
  theme(legend.position = ("top"),legend.title=element_blank())
spacing_by_month


#-----------
#-----------------------------------------------------------------------------------
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


#---------------------------------------------

# remove one pot from each stringline 
# when calculating spacing, the first pot doesn't actually have spacing and should therefore be removed
traps_g_OR_v2 <- traps_g_OR %>% 
  group_by(SetID) %>%
  slice(2:n()) %>% 
  ungroup() %>% 
  #then create a new PotsFished column that reflects that one pot that was removed
  mutate(PotsFished_one = PotsFished - 1)

traps_g_OR_v3 <- traps_g_OR_v2 %>% 
  #there are line lengths of 0 that can be filtered out here, but also later
  #can't get pot spacing if line length is 0
  filter(!line_length_m == 0) %>%  
  rowwise() %>% 
  #now we can calculate the average spacing in each line, using the following method so as to not take averages of averages
  mutate(spacing_in_m = (sum(line_length_m)) / (sum(PotsFished_one)) )


# there are cases of really short pot spacing when a short string line had reportedly lots of pots 
# e.g. 7.8m string with 100 pots
# there are  cases of really long pot spacing when few pots reported on a very long string
# e.g. 2 post on a 11km string line
# also infinite values when originally reported 1 pot on a string becomes 0 (see above)
# --> would still have been a long string line with very few pots even if had kept PotsFished as 1, as PotsFished = 1 string lines >1km long
# so we want to make a call of where to cut the data and acknowledge that the reported info in logbooks may not always be fully accurate



hist(traps_g_OR_v3$spacing_in_m)


# pot_spacing_OR_bins <-  traps_g_OR_v3 %>%
#   mutate(pot_spacing_bins = ifelse(spacing_in_m < 300, 'less_than_300', 'more_than_300'))

pot_spacing_OR_subset <- traps_g_OR_v3  %>%   
  ###here can make assumption of reasonable min and max dist between pots 
  ###~99% of pot spacings <300m
  filter(spacing_in_m > 3 &  spacing_in_m <= 300)

hist(pot_spacing_OR_subset$spacing_in_m)


# # additional ways to look at max cut off point
# pots_by_spacing <- pot_spacing_OR_subset %>%
#   count(spacing_in_m) %>%
#   ungroup() %>%
#   rename(pots=n) %>%
#   # do cumulative counts
#   arrange(spacing_in_m) %>%
#   mutate(cumulative_pots=cumsum(pots),perc_pots=cumulative_pots/last(cumulative_pots)*100)
# glimpse(pots_by_spacing)
# 
# dist_pots_spacing <- pots_by_spacing %>%
#   ggplot(aes(x=spacing_in_m ,y=perc_pots))+
#   geom_line(size=1)+
#   geom_hline(aes(yintercept = 99), colour="blue", linetype=2)+
#   labs(x="Pot spacing (m)",y="Cumulative % Traps") +
#   ggtitle("Distribution of crab pots by pot spacing") +
#   theme(legend.position = ("top"),legend.title=element_blank())
# dist_pots_spacing

#--------------------------

# summarise
# mean and other summaries will depend on cutoff values (min spacing >2m or >3m, max spacing <300m or <250m)

# #trying to use summarise() command just takes really long time, and is not working properly...

#all seasons, all vessels (without grouping)
mean(pot_spacing_OR_subset$spacing_in_m, na.rm = TRUE) 
#132.2435m --> 12.17pots/mile
1000/(mean(pot_spacing_OR_subset$spacing_in_m))/0.621371
#12.16956 pots/mile

median(pot_spacing_OR_subset$spacing_in_m, na.rm = TRUE)
#131.7453
1000/(median(pot_spacing_OR_subset$spacing_in_m))/0.621371
#12.21558

sd(pot_spacing_OR_subset$spacing_in_m, na.rm = TRUE) 
#46.63455
1000/(sd(pot_spacing_OR_subset$spacing_in_m))/0.621371
#34.5097

min(pot_spacing_OR_subset$spacing_in_m, na.rm = TRUE) 
#3.172402
1000/(min(pot_spacing_OR_subset$spacing_in_m))/0.621371
#507.2953

max(pot_spacing_OR_subset$spacing_in_m, na.rm = TRUE) 
#299.9451
1000/(max(pot_spacing_OR_subset$spacing_in_m))/0.621371
#5.365463

hist(pot_spacing_OR_subset$spacing_in_m)

# pot_spacing_density <- pot_spacing_OR_subset %>%
#   ggplot(aes(spacing_in_m))+
#   geom_density(alpha=0.5)+
#   labs(x="Pot spacing (m)",y="Kernel Density",fill="Pot limit group",col="Pot limit group")+
#   theme(panel.grid.major=element_blank(),
#         panel.grid.minor=element_blank(),
#         axis.text.y=element_blank())
# pot_spacing_density


#-----------

#All vessels, by season

summary_all_vessels_by_season <-  pot_spacing_OR_subset %>%
  group_by(season) %>% 
  summarise(mean_spacing = mean(spacing_in_m),
            median_spacing = median(spacing_in_m),
            min_spacing = min(spacing_in_m),
            max_spacing = max(spacing_in_m),
            sd_spacing = sd(spacing_in_m),
            #how these translate to pots/mile
            mean_pots_per_mile = 1000/mean_spacing/0.621371,
            median_pots_per_mile = 1000/median_spacing/0.621371,
            min_pots_per_mile = 1000/min_spacing/0.621371,
            max_pots_per_mile = 1000/max_spacing/0.621371,
            sd_pots_per_mile = 1000/sd_spacing/0.621371
  )

ggplot(pot_spacing_OR_subset, aes(x=as.factor(season), y=spacing_in_m)) + 
  geom_boxplot()

box_all_vessels_by_season <- ggplot(pot_spacing_OR_subset, aes(x=as.factor(season), y=spacing_in_m)) + 
  geom_boxplot()
box_all_vessels_by_season


#-----------

#All seasons, by pot limit

summary_all_seasons_by_pot_limit <-  pot_spacing_OR_subset %>%
  group_by(Potlimit) %>% 
  summarise(mean_spacing = mean(spacing_in_m),
            median_spacing = median(spacing_in_m),
            min_spacing = min(spacing_in_m),
            max_spacing = max(spacing_in_m),
            sd_spacing = sd(spacing_in_m),
            #how these translate to pots/mile
            mean_pots_per_mile = 1000/mean_spacing/0.621371,
            median_pots_per_mile = 1000/median_spacing/0.621371,
            min_pots_per_mile = 1000/min_spacing/0.621371,
            max_pots_per_mile = 1000/max_spacing/0.621371,
            sd_pots_per_mile = 1000/sd_spacing/0.621371
  )


ggplot(pot_spacing_OR_subset, aes(x=as.factor(Potlimit), y=spacing_in_m)) + 
  geom_boxplot()

pot_spacing_density <- pot_spacing_OR_subset %>% 
  ggplot(aes(spacing_in_m))+
  geom_density(alpha=0.5)+
  labs(x="Pot spacing (m)",y="Kernel Density",fill="Pot limit group",col="Pot limit group")+
  facet_wrap(~ Potlimit,scales = 'free_y')+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text.y=element_blank())
pot_spacing_density

OR_pot_spacing_p1 <- pot_spacing_OR_subset %>% 
  ggplot() + 
  geom_bar(aes(x=spacing_in_m, y=stat(prop)), position = "dodge") +
  scale_x_binned(breaks=seq(0, 500, 25),limits=c(0,500)) + 
  facet_wrap(~ Potlimit) +
  #scale_y_continuous(breaks=seq(0, 0.5, 0.05),limits=c(0,0.5))+
  labs(x="Spacing between pots (m)",y="Proportion") +
  ggtitle('OR - spacing between pots (m)')
OR_pot_spacing_p1



#-----------

# by season and by pot limit

pot_spacing_OR_subset %>% 
  group_by(Potlimit, season) %>% 
  summarise(mean_spacing = mean(spacing_in_m),
            mean_pots_per_mile = 1000/(mean(spacing_in_m))/0.621371
  )


ggplot(pot_spacing_OR_subset, aes(x=as.factor(season), y=spacing_in_m, fill=as.factor(Potlimit))) + 
  geom_boxplot() +
  theme(legend.position="bottom")


#-----------
#-----------------------------------------------------------------------------------


