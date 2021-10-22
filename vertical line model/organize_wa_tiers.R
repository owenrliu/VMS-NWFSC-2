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
