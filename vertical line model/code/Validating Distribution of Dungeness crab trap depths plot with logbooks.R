#Validating 'Fig. 5. Distribution of Dungeness crab trap depths from simulated traps 
#for all VMS-equipped vessels in four major west coast regions.' 

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

# use the 'path' approach as data is in raimbow project/repo
path.fish_WA <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/traps_g_license_all_logs_2009_2020.rds"
traps_g_WA_raw <- readRDS(path.fish_WA) 


# remove geometry, create columns for season, month
# create matching 'season portion' as Owens: 
# early (November to January), middle (February to April), or late (May to September)
traps_g_WA <- traps_g_WA_raw %>% 
  st_set_geometry(NULL) %>% 
  mutate(
    season = str_sub(SetID,1,9),
    year = year(SetDate),
    month_name = month(SetDate, label=TRUE, abbr = FALSE),
    season_month = paste0(season,"_",month_name)) %>% 
  #VLM timeline is basically 2008-09 to 2018-19 seasons
  #we don't have 2008-09 season for WA, but we can exclude 2019-20 to try to match the VLM timeline as best we can
  filter(!season =='2019-2020') %>% 
    mutate(season_segment=case_when(
    month_name %in% c('November','December','January') ~ 'early',
    month_name %in% c('February','March','April') ~ 'mid',
    month_name %in% c('May','June','July','August','September') ~ 'late'
  )) %>% 
  mutate(season_segment=factor(season_segment,levels=c('early','mid','late'))) %>% 
  # dataset has highly negative values (~ -30000) to denote port and bay areas - remove those. 
  # Also note that place_traps function (script 1) already removes depths deeper than 200m as crab fishing at deeper depths is not likely
  filter(depth > -1000) %>% 
  #also few cases on NAs in season_segment because Set_Date missing from 14 stringlines, remove those
  filter(!is.na(season_segment))


#Depth distributions by season segment

trap_depth_dist <- traps_g_WA %>% 
  ggplot(aes(depth,fill=season_segment,col=season_segment))+
  geom_density(alpha=0.5)+
  labs(x="Trap Depth of simulated pots (m)",y="Kernel Density",fill="Season Portion",col="Season Portion")+
  geom_vline(xintercept=-40*1.829,col='red',linetype=2)+
  ggtitle("WA") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text.y=element_blank(),
        plot.title = element_text(hjust = 0.5)
        )
trap_depth_dist

    


# OREGON

# use the 'path' approach as data is in raimbow project
path.fish_OR <- "C:/Users/Leena.Riekkola/Projects/raimbow/wdfw/data/OR/OR_traps_g_all_logs_2007_2018_SpatialFlag_filtered.rds"
traps_g_OR_raw <- readRDS(path.fish_OR) 

# remove geometry, create columns for season, month
# create matching 'season portion' as Owens: 
# early (November to January), middle (February to April), or late (May to September)
traps_g_OR <- traps_g_OR_raw %>% 
  st_set_geometry(NULL) %>% 
  mutate(
    season = str_sub(SetID,1,9),
    year = year(SetDate),
    month_name = month(SetDate, label=TRUE, abbr = FALSE),
    season_month = paste0(season,"_",month_name)) %>% 
  #VLM timeline is basically 2008-09 to 2018-19 seasons
  #we don't have 2018-19 season for OR, but we can exclude 2007-08 to try to match the VLM timeline as best we can
  filter(!season =='2007-2008') %>% 
  mutate(season_segment=case_when(
    month_name %in% c('November','December','January') ~ 'early',
    month_name %in% c('February','March','April') ~ 'mid',
    month_name %in% c('May','June','July','August','September') ~ 'late'
  )) %>% 
  mutate(season_segment=factor(season_segment,levels=c('early','mid','late'))) %>% 
  # dataset has highly negative values (~ -30000) to denote port and bay areas - remove those. 
  # Also note that place_traps function (script 1) already removes depths deeper than 200m as crab fishing at deeper depths is not likely
  filter(depth > -1000) 


#Depth distributions by season segment

trap_depth_dist_OR <- traps_g_OR %>% 
  ggplot(aes(depth,fill=season_segment,col=season_segment))+
  geom_density(alpha=0.5)+
  labs(x="Trap Depth of simulated pots (m)",y="Kernel Density",fill="Season Portion",col="Season Portion")+
  geom_vline(xintercept=-40*1.829,col='red',linetype=2)+
  ggtitle("OR") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text.y=element_blank(),
        plot.title = element_text(hjust = 0.5)
  )
trap_depth_dist_OR



