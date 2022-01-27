#VMS and non-VMS vessel comparison in logbook data

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

# Read in spatial grid data 
# example spatial grid - 5x5 grid shapefile
grd <- read_sf(here::here('data', 'fivekm_grid_polys_shore_lamb.shp'))
names(grd)

#Owen shared 'wa_tier_has_vms_key.rds' that has links License_ID, Pot_Limit, FederalID and drvid 
#and labels if combos of those had VMS data
# wa_tier_has_vms_key <- read_rds(here::here('data','wa_tier_has_vms_key.rds'))
# glimpse(wa_tier_has_vms_key)

#I manually checked 'wa_tier_has_vms_key.rds' against logbook data, 
#and added vessel name for unique License_ID and FederalID combos.
#In 'wa_tier_has_vms_key.rds' the same License_ID can appear multiple times with different FederalID
#made a version of the file where labelled if some License_ID and FederalID combos never seemed to have VMS data associated (non-VMS)
#cases when License_ID and FederalID combo had two entries, has_vms = 0 AND has_vms = 1, as below
# License_ID  Pot_Limit  FederalID   drvid  has_vms
#  58037        500       972 281   972281    0
#  58037        500       972 281   972281    1
# these are labelled as VMS cases - but we don't know in which year that vessel would have had VMS

wa_tier_has_vms_key_groupings <- read_csv(here::here('data','wa_tier_has_vms_key_groupings.csv')) %>% 
  select(Vessel, grouping) %>% 
  distinct(Vessel, .keep_all = TRUE) %>% 
  filter(!is.na(Vessel)) %>% 
  mutate(grouping = ifelse(grouping == 'unsure', 'VMS', grouping)) #Pacific Pride is the only vessel where unsure of grouping

#-----------------------------------------------------------------------------------

#bring in logbook data, in point format (not yet summarised to grid level)
#bunch of the vessels in 'wa_tier_has_vms_key.rds' are Q999999 - vessels that landed to OR
#therefore use dataset that has OR effort in WA included -- but note that this is limited to 2014-2020
#also use data that is NOT clipped to WA waters (include data by these vessels in OR waters as well) 

# use the 'path' approach as data is in raimbow project/repo

path_traps_g_WA_logs_2014_2020 <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/traps_g_WA_logs_2014_2020_20220119.rds"
traps_g_WA_logs_2014_2020_raw <- readRDS(path_traps_g_WA_logs_2014_2020) %>% 
  st_set_geometry(NULL)
#this is output from script 1, but file not been clipped in QGIS

path_traps_g_WA_Q999999_logs_2014_2020 <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/traps_g_WA_Q999999_logs_2014_2020_20220119.rds"
traps_g_WA_Q999999_logs_2014_2020_raw <- readRDS(path_traps_g_WA_Q999999_logs_2014_2020) %>% 
  st_set_geometry(NULL)
#this is output from script 1, but file not been clipped in QGIS

#join the files - full data set of logbook data in WA waters, landed in WA and OR
traps_g_WA_all_logs_2014_2020 <- rbind(traps_g_WA_logs_2014_2020_raw,traps_g_WA_Q999999_logs_2014_2020_raw)

#-----------------------------------------------------------------------------------

#into logbook data, join in if vessel has ever had VMS (VMS grouping) based on vessel name
traps_g_WA_all_logs_2014_2020_VMS_grouping <- traps_g_WA_all_logs_2014_2020 %>% 
  left_join(wa_tier_has_vms_key_groupings, by=c("Vessel"))

#also join in license data to get PotLimit info
# Read in and join license & pot limit info (in raimbow project/repo)
path.trap_tier_info_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/WA_pot_limit_info_May2021.csv"
trap_tier_info_WA_raw <- read.csv(path.trap_tier_info_WA) 

WA_pot_limit_info <- trap_tier_info_WA_raw %>% 
  rename(License = License_ID)

# join Pot_Limit to traps_g 
traps_g_WA_all_logs_2014_2020_VMS_grouping_pot_tier <- traps_g_WA_all_logs_2014_2020_VMS_grouping %>% 
  left_join(WA_pot_limit_info,by=c("License")) %>% 
  #'wa_tier_has_vms_key.rds' has Vessel RAVEN DANCER as pot limit 500
  mutate(Pot_Limit = 
           ifelse(Vessel == 'RAVEN DANCER', 500, Pot_Limit)) %>% 
  drop_na(Pot_Limit) #the only other NA cases are records with no Vessel in the log





