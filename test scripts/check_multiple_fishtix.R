# check multiple tickets
library(tidyverse)
library(here)

# import all clean fish tickets
yrs <- 2009:2019
fishtix_all <- purrr::map_df(yrs,function(x){
  read_rds(here::here('data','processed','fish tickets',paste0(x,"fishtix",".rds")))
})
glimpse(fishtix_all)

# number of tickets by date and vessel id

fishtix_multi_dcrb <- fishtix_all %>% 
  
  #
  filter(TARGET_rev=="DCRB") %>% 
  # boolean indicating whether a record is for personal use
  mutate(is_personal=removal_type_name=="PERSONAL USE") %>% 
  
  # for each vessel and date
  group_by(drvid,agency_code,date) %>% 
  
  # calculate quantities of interest
  summarise(n_tix=n_distinct(Rec_ID),
         n_pers=n_distinct(Rec_ID[is_personal]),
         n_nonpers_tix=n_tix-n_pers) %>% 
  select(-n_pers) %>% 
  ungroup()

range(fishtix_multi_dcrb$n_tix)

fishtix_multi_dcrb %>% 
  ggplot()+
  geom_histogram(aes(n_tix),binwidth = 1)+
  facet_wrap(~agency_code,nrow=1)+
  labs(x="Number of Tickets Landed per Day",y="Number of Vessel Landing-Days")+
  theme_minimal()

# investigating with 2011 data
tix2011 <- read_rds(here::here('data','processed','fish tickets',"2011fishtix.rds")) %>%
  
  #
  filter(TARGET_rev=="DCRB") %>% 
  # boolean indicating whether a record is for personal use
  mutate(is_personal=removal_type_name=="PERSONAL USE") %>% 
  
  # for each vessel and date
  group_by(drvid,date) %>% 
  
  # calculate quantities of interest
  mutate(n_tix=n_distinct(Rec_ID),
         n_pers=n_distinct(Rec_ID[is_personal]),
         n_nonpers_tix=n_tix-n_pers) %>% 
  select(-n_pers,-is_personal) %>% 
  ungroup()
range(tix2011$n_tix)
tix2011 %>% count(n_tix)

# check where we los the tickets associated with multi-tix days
test6 <- tix2011 %>% filter(n_tix==4)

# from the matched data
test <- read_rds(here('data','processed','matched','matching','2011matched_alltix.rds')) %>% 
  filter(TARGET_rev=="DCRB") %>% 
  # boolean indicating whether a record is for personal use
  mutate(is_personal=removal_type_name=="PERSONAL USE") %>% 
  
  # for each vessel and date
  group_by(drvid,date) %>% 
  
  # calculate quantities of interest
  mutate(n_tix=n_distinct(Rec_ID),
         n_pers=n_distinct(Rec_ID[is_personal]),
         n_nonpers_tix=n_tix-n_pers) %>% 
  select(-n_pers,-is_personal) %>% 
  ungroup()
range(test$n_tix)

multitix <- test %>% filter(Rec_ID %in% test6$Rec_ID)
length(unique(multitix$Rec_ID))

## CONCLUSION-- THIS IS AN ISSUE WE NEED TO DEAL WITH
## MULTI-TICKET DAYS NOT BEING HANDLED CORRECTLY