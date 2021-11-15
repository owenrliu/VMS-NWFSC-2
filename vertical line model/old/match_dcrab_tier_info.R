## Match tier information for CA crab vessels

library(tidyverse)
library(magrittr)
library(here)

# CA tier info
# tiers <- read_csv(here::here('data','raw','dcrab_tiers.csv'),col_types = 'dcidcccc')
# 
# tiers %<>% rename(vesselID=`FG VESSEL ID`,permit_number=`LE PERMIT NUM`)
# # vessels and permit numbers from tier info
# unique_vessels <- unique(tiers$vesselID)
# unique_permits <- unique(tiers$permit_number)
# 
# # Fish tickets with day of season indicator (pre-VMS match)
# fishtix <- read_rds(here::here('data','processed','fish tickets','fish_tickets_w_dayofseason.rds'))
# # vessel IDs from the (processed) fish tickets
# unique_drvid <- unique(fishtix$drvid)
# 
# # try matching
# matched_vesselIDs <- intersect(unique_drvid,unique_vessels)
# 
# # these don't match
# # try with a set of raw fish tickets
# raw_fishtix <- read_csv(here::here('data','raw','fish tickets','fish tickets 2016-2017.csv'))
# 
# # pull out some candidate variables to check for overlap
# test_raw_ids <- raw_fishtix %>% 
#   filter(AGENCY_CODE=="C") %>% 
#   select(AGENCY_CODE,VESSEL_REGISTRATION_ID,FISHER_LICENSE_NUM,VESSEL_ID,VESSEL_NUM)
# 
# glimpse(test_raw_ids)
# 
# test_raw_ids %<>% mutate(FISHER_LICENSE_NUM=as.integer(FISHER_LICENSE_NUM))
# 
# unique_raw_ids <- unique(test_raw_ids$FISHER_LICENSE_NUM)
# 
# # do these match?
# matched_raw_ids <- intersect(unique_raw_ids,unique_vessels)
# # they match a little (16 vessels), so FISHER_LICENSE_NUM might be promising?
# # let's pull these ID values from all years' data
# pull_ids <- function(fname) {
#   ids <- read_csv(fname) %>% 
#     filter(AGENCY_CODE=="C") %>% 
#     select(AGENCY_CODE,VESSEL_REGISTRATION_ID,FISHER_LICENSE_NUM,VESSEL_ID,VESSEL_NUM) %>% 
#     mutate(FISHER_LICENSE_NUM=as.double(FISHER_LICENSE_NUM),VESSEL_NUM=as.character(VESSEL_NUM)) %>% 
#     distinct()
#   ids
# }
# 
# fnames <- list.files(here::here('data','raw','fish tickets'),full.names = T)
# # pull out all these IDs from all raw fish tickets
# raw_ids <- purrr::map_df(fnames,pull_ids) %>% 
#   mutate(is_in_fishtix="Y")
# 
# glimpse(raw_ids)
# 
# # Now try the join again
# tiers_matched <- tiers %>% 
#   select(vesselID,TIER) %>% 
#   left_join(raw_ids,by=c('vesselID'="FISHER_LICENSE_NUM"))
# 
# # how many matched?
# sum(tiers_matched$is_in_fishtix=="Y",na.rm=T)
# # only 304 total vessels matched (maybe this is enough?)


#### TRYING AGAIN, 08/11/20 ####
# fish ticket drvids
fishtix <- read_rds(here::here('data','processed','fish tickets','fish_tickets_w_dayofseason.rds'))
# vessel IDs from the (processed) fish tickets
unique_drvid <- unique(fishtix$drvid)

# need vessel registrations
vessel_reg_fls <- list.files(here('data','raw','vessel registration'),full.names = T)
vessel_registration <- purrr::map_dfr(vessel_reg_fls,read_csv,col_types='ddcddccdcccccdddcccccccddll') %>% distinct()

# we should (a) join fish tickets and vessel registration tables using the field vessel_registration_id, and 
# (b) carry the registration_number field with us from the vessel_registrations table when we make the vessel key.
# VESSEL_NUM == drvid in the fish tickets
# REGISTRATION_NUM == FG VESSEL ID in the CDFW tier info
# so we need a matching key between VESSEL_NUM/drvid and REGISTRATION NUM

vessel_registration %<>% select(VESSEL_REGISTRATION_ID,VESSEL_NUM,AGENCY_CODE,REGISTRATION_YEAR,REGISTRATION_NUM) %>% distinct()

# how many vessel_registration_id associated with each VESSEL_NUM?
drvid_num_records <- vessel_registration %>% group_by(VESSEL_NUM,REGISTRATION_YEAR,AGENCY_CODE) %>% 
  # California vessels in the range of the VMS data
  filter(VESSEL_NUM != "UNKNOWN",AGENCY_CODE=="C",REGISTRATION_YEAR>2007) %>% 
  summarise(n_ids=n_distinct(REGISTRATION_NUM)) %>% 
  ungroup()
# 6 drvids have multiple registration numbers in single years, all others have only one registration_num per year
# Create the key matching VESSEL_NUM to REGISTRATION_NUM
drvid_fgVesselID_match_key <- vessel_registration %>% 
  # California vessels in the range of the VMS data
  filter(VESSEL_NUM != "UNKNOWN",AGENCY_CODE=="C",REGISTRATION_YEAR>2007) %>% 
  rename(drvid=VESSEL_NUM,year=REGISTRATION_YEAR,tier_fgVesselID=REGISTRATION_NUM) %>% 
  distinct(drvid,year,tier_fgVesselID) %>% 
  mutate(tier_fgVesselID=as.numeric(tier_fgVesselID))

# Filter vessel registration numbers

tiers <- read_csv(here::here('data','raw','dcrab_tiers.csv'),col_types = 'dcidcccc')
tiers %<>% rename(tier_fgVesselID=`FG VESSEL ID`,permit_number=`LE PERMIT NUM`,tier=TIER,year=`DCVP YR`,
                  issue_date=`ISSUE DATE`,end_date=`END DATE`) %>% 
  select(tier_fgVesselID,tier,year,issue_date,end_date)

# Match
tiers_drvid_match <- tiers %>% left_join(drvid_fgVesselID_match_key) %>% 
  # add number of traps
  mutate(ntraps=case_when(
    tier==1 ~ 500,
    tier==2 ~ 450,
    tier==3 ~ 400,
    tier==4 ~ 350,
    tier==5 ~ 300,
    tier==6 ~ 250,
    tier==7 ~ 175
  ))

# quickly look at double matches
double_match_check <- tiers_drvid_match %>% count(tier_fgVesselID,year) %>% filter(n>1)
# after looking closely, it appears that some vessels have multiple tiers within the same year
# for these, choose the permit with the latest issue date
library(lubridate)
tiers_drvid_match %<>%
  mutate(issue_date=as_date(issue_date,format='%m/%d/%Y'),
         end_date=as_date(end_date,format='%m/%d/%Y')) %>% 
  group_by(tier_fgVesselID,year) %>% 
  arrange(desc(issue_date)) %>% 
  slice(1) %>% 
  ungroup()

# save the key
write_rds(tiers_drvid_match,here('data','raw','CAtier_drvid_matching_key.rds'))
write_csv(tiers_drvid_match,here('data','raw','CAtier_drvid_matching_key.csv'))
