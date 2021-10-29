# Join all CA, OR, and WA tier information into one overall key, by year and month

library(tidyverse)
library(here)

# For CA, tier info (number of traps per permit)
CAtier <- read_rds(here('data','raw','CAtier_drvid_matching_key.rds')) %>% 
  dplyr::select(year,drvid,ntraps) %>% 
  rename(C=ntraps)

# For OR, tier info (number of traps per permit, organized in a separate script
ORtier <- read_rds(here('vertical line model','oregon_tier_drvid_matching_key.rds')) %>% 
  mutate(month=match(month,month.name)) %>%
  drop_na() %>% 
  rename(O=ntraps_permitted)

# WA
WAtier <- read_rds(here::here('vertical line model','wa_tier_drvid_key.rds')) %>% 
  dplyr::select(drvid,Pot_Limit) %>% 
  drop_na() %>% 
  rename(W=Pot_Limit)

# COMBINED KEY BY YEAR/MONTH
all_permitted_drvids <- c(unique(CAtier$drvid),unique(ORtier$drvid),unique(WAtier$drvid))
tier_key <- crossing(year=2007:2019,month=1:12,drvid=all_permitted_drvids) %>% 
  mutate(crabyr=ifelse(month<=10,year-1,year)) %>% 
  # join CA tier info
  left_join(CAtier,by=c('crabyr'='year','drvid')) %>% 
  # join OR tier info
  left_join(ORtier,by=c('year','month','drvid')) %>% 
  # join WA tier info
  left_join(WAtier,by=c('drvid')) %>% 
  pivot_longer(C:W,names_to="agency_code",values_to="ntraps_permitted") %>% 
  # filter out NA ntraps
  filter(!is.na(ntraps_permitted))

## SAVE
write_rds(tier_key,here::here('vertical line model','tier_information_year_month_allstates.rds'))
