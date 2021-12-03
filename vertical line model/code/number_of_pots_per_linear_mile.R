# Investigating the number of crab pots per linear mile (i.e., pot spacing)
# For WA and OR

#-----------------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(here)

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
## note that the current input RDS has been restricted to 2013-2020 -- but we can do a version for 2009-2020 if needed

# for now we use the version for WA logbook data where too long (>80km) and too short stringlines (0m and >50 pot reported) are RETAINED
# if these are removed before the following code, that doesn't make any noticeable difference, 
# and various filters will get used in the following code to deal with outliers


# use the 'path' approach as data is in raimbow project
path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/traps_g_license_all_logs_2013_2020_too short long flagged not deleted.rds"
traps_g_WA_raw <- readRDS(path.fish_WA) 

# remove geometry, create columns for season, month
traps_g_WA <- traps_g_WA_raw %>% 
  st_set_geometry(NULL) %>% 
  mutate(
    season = str_sub(SetID,1,9),
    year = year(SetDate),
    month_name = month(SetDate, label=TRUE, abbr = FALSE),
    season_month = paste0(season,"_",month_name))


# this RDS has pots simulated along the stringline (each row is an individual simulated pot) - the simulated pots aren't actually necessary, 
# we only need the length of the line and the reported "PotsFished" - remove duplicated rows based on SetID
traps_g_WA_v2 <- traps_g_WA %>% distinct(SetID, .keep_all = TRUE)
  
#-----------------------------------------------------------------------------------

# investigate pot spacing

# remove strinlines that are 0m as can't get spacing for those
pot_spacing_WA <-  traps_g_WA_v2 %>% 
  filter(line_length_m > 0.1) %>% 
  mutate(spacing_in_m = line_length_m/PotsFished)


# there are few cases of really long pot spacing
# large spacings likely due to cases where few pots reported on a very long string
# e.g. the case of max spacing of 16472m is a case of 1 pot on a 16472m stringline
# or the next longest spacing 9511m (2 pots on a 19023m stringline)
# so we want to make a call of where to cut the data and say that the reported info in logbook
# isn't fully trustworthy

pot_spacing_WA_bins <-  pot_spacing_WA %>% 
  mutate(pot_spacing_bins = cut(spacing_in_m, breaks = c(0, 250, 500, 1000, max(spacing_in_m))))

plot1 <- pot_spacing_WA_bins %>%
  ggplot() + 
  geom_bar(aes(x=pot_spacing_bins, y = (..count..)/sum(..count..))) +
  #facet_wrap(~ season) +
  labs(x="pot spacing bin (m)",y="proportion") +
  ggtitle('WA - spacing between pots (in m)') +
  theme_minimal()
plot1

# summary of pot spacings across all of 2013-2020
summary_pot_spacing_WA_bins <- pot_spacing_WA_bins %>% 
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
#   96.4             2.72             0.600              0.227
# 96% of pot spacing is 0-250m between pots, or 99% is within 0-500m between pots
# so lets just exclude cases where spacing between pots is >500m



# there is also probably a min distance that the pots would need to be
# lets look at 0-10m spacing subset
test <- pot_spacing_WA %>% 
  filter(spacing_in_m <= 10)
hist(test$spacing_in_m)
# spike between 0-2m spacing 
# let's for now exclude spacing that is <2m or >500m


pot_spacing_WA_subset <- pot_spacing_WA %>% 
  filter(spacing_in_m > 2 &  spacing_in_m <= 500)


# plot of pot spacing across all of 2013-2020
WA_pot_spacing_p1 <- pot_spacing_WA_subset %>% 
  ggplot() + 
  geom_bar(aes(x=spacing_in_m, y=stat(prop)), position = "dodge") +
  scale_x_binned(breaks=seq(0, 500, 50),limits=c(0,500)) + 
  #scale_y_continuous(breaks=seq(0, 0.5, 0.05),limits=c(0,0.5))+
  labs(x="Spacing between pots (m)",y="Proportion") +
  ggtitle('WA - spacing between pots (m)')
WA_pot_spacing_p1


summary_pot_spacing_WA_subset <- pot_spacing_WA_subset %>% 
  summarise(min_spacing = min(spacing_in_m, na.rm=TRUE),
            max_spacing = max(spacing_in_m, na.rm=TRUE),
            sd_spacing = sd(spacing_in_m, na.rm=TRUE), 
            mean_spacing = mean(spacing_in_m, na.rm=TRUE),
            median_spacing = median(spacing_in_m, na.rm=TRUE)
            )
# Washington
# min_spacing  max_spacing  sd_spacing  mean_spacing  median_spacing
#   2.03         499.68       57.30       114.14        105.84




#-----------------------------------------------------------------------------------

# OREGON

# We will investigate pot spacing as the length of stringline divided by the number of pots reported on it

# we use the form of logbook data that is not yet summarised on a grid level (but pot are simualted along stringline)
# the current input RDS cover all OR data we have so far (2007-2018)  

# for now we use the version for WA logbook data where too long (>80km) and too short stringlines (0m and >50 pot reported) are RETAINED
# if these are removed before the following code, that doesn't make any noticeable difference, 
# and various filters will get used in the following code to deal with outliers


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
    season_month = paste0(season,"_",month_name))


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
  #facet_wrap(~ season) +
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





