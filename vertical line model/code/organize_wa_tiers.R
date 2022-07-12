# Organize WA Tier info
library(tidyverse)
library(sf)

watier <- read_csv(here::here('vertical line model','WA_pot_limit_info_May2021.csv'))
glimpse(watier)

fishtix_matched_all <- read_rds(here::here('data','processed','fish tickets','fish_tickets_w_dayofseason.rds'))

#match ID numbers
glimpse(fishtix_matched_all)
x <- fishtix_matched_all %>% filter(agency_code=="W")
y <- unique(x$drvid)

# vms tickets
vms_tickets <- read_rds(here('fishing behavior','fish_tickets_with_vms.rds'))
fishtix <- fishtix_matched_all %>% 
  left_join(vms_tickets) %>% 
  mutate(has_vms=replace_na(has_vms,0))
drvid_hasvms <- fishtix %>% 
  filter(year>2017) %>% 
  distinct(drvid,has_vms)
  

# logs
logs <- read_rds(here::here('data','raw','wa','crab-logs-portable','output','WA-crab-log-spatial-lines.rds')) %>% 
  as_tibble()
glimpse(logs)

# pull out unique License-Federal ID combos
ids <- logs %>% distinct(License,FederalID)
# remove spaces in the federal IDs
ids <- ids %>% mutate(drvid=str_replace_all(FederalID," ",""))
glimpse(ids)

wa_tier_drvid <- watier %>% left_join(ids,by=c('License_ID'='License'))
glimpse(wa_tier_drvid)

write_rds(wa_tier_drvid,here::here('vertical line model','wa_tier_drvid_key.rds'))

# check on vms representation
wa_tier_hasvms <- wa_tier_drvid %>% left_join(drvid_hasvms) %>% 
  mutate(has_vms=replace_na(has_vms,0))

write_rds(wa_tier_hasvms,here::here('vertical line model','wa_tier_has_vms_key.rds'))


# check total permitted pots per year
x<-fishtix_matched_all %>% 
  filter(agency_code=="W") %>% 
  distinct(crab_season,drvid) %>% 
  left_join(wa_tier_drvid) %>%   
  # assume 500 if NA? or 0?
  mutate(pots_est=coalesce(Pot_Limit,0))
y<-x %>% 
  group_by(crab_season) %>% 
  summarise(totpots=sum(pots_est))
