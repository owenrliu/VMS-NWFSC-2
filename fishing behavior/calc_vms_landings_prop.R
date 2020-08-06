# Find the proportion of landings taken by VMS-equipped vessels in each crab season
library(tidyverse)
fishtix <- read_rds(here::here('data','processed','fish tickets','fish_tickets_w_dayofseason.rds'))
recid_crabseason <- fishtix %>% dplyr::select(Rec_ID,crab_season) %>% distinct()
vms <- purrr::map_df(2009:2019, function(yr){
  read_rds(paste0(here::here('data','processed','matched','filtering',yr),"matched_filtered.rds"))
}) %>% 
  filter(TARGET_rev=='DCRB') %>% 
  # join crab season indicator
  left_join(recid_crabseason,by='Rec_ID')

# key of RecIDs with and without vms
vms_tix <- vms %>% 
  distinct(Rec_ID) %>% 
  mutate(has_vms=1)
write_rds(vms_tix,here::here('fishing behavior','fish_tickets_with_vms.rds'))

fishtix_vmsflag <- fishtix %>% 
  left_join(vms_tix)

# calc landings of VMS vs. non-VMS tix
vms_prop_landings <- fishtix_vmsflag %>% 
  mutate(has_vms=!is.na(has_vms)) %>% 
  group_by(crab_season,agency_code,has_vms) %>% 
  summarise(totland=sum(DCRB_lbs,na.rm=T)) %>% 
  ungroup() %>% 
  group_by(crab_season,agency_code) %>% 
  mutate(sumland=sum(totland),sumland_thousand_mt=sumland/1000/2204.64,propland=totland/sumland)

# total landings timeseries
total_landings <- vms_prop_landings %>% 
  distinct(crab_season,sumland) %>% 
  ggplot(aes(crab_season,sumland/(1000*2204),group='crab_season'))+
  facet_wrap(~agency_code)+
  geom_point()+geom_line()+theme_minimal()+
  labs(y="Landings (MT)")+
  theme(axis.text.x = element_text(angle=90))
ggsave(here::here('fishing behavior','total_landings_all_fishtickets.png'),total_landings,w=8,h=6)

# prop landings
vms_prop_landings_plot <- vms_prop_landings %>% 
  ggplot(aes(crab_season,propland,fill=has_vms,col=has_vms))+
  geom_bar(stat='identity')+
  facet_wrap(~agency_code)+
  labs(x="Crab Season",y="Proportion of Recorded Landings",fill="State",col="State")+
  theme(axis.text.x = element_text(angle=90,vjust = 0.5))
ggsave(here::here('fishing behavior','vms_prop_landings_all_fishtickets.png'),vms_prop_landings_plot,w=8,h=6)

#save just the VMS proportion
vms_prop_landings %>% 
  filter(has_vms) %>% 
  dplyr::select(crab_season,agency_code,propland) %>% 
  write_rds(here('fishing behavior','vms_proportional_landings_key.rds'))


# abun_vmsadj %>% filter(crab_season %in% unique(vms_prop_landings$crab_season)) %>% 
#   ggplot(aes(crab_season,mean_abun_adj,group="crab_season"))+
#   geom_point()+geom_line()+facet_wrap(~agency_code)+
#   theme(axis.text.x = element_text(angle=90))
