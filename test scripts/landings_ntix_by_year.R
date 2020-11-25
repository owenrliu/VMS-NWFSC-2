library(tidyverse)
library(lubridate)
dat <- read_rds(here::here('data','processed','matched','interpolation','vms_all_interpolated_w_grd.rds'))
tix_yr <- dat %>% 
  filter(agency_code=="C") %>% 
  distinct(date,DCRB_lbs,Rec_ID) %>% 
  mutate(yr=year(date)) %>% 
  group_by(yr) %>% 
  summarise(ntix=n_distinct(Rec_ID),landings=sum(DCRB_lbs,na.rm=T)) %>% 
  ungroup()

tix_plot <- tix_yr %>% 
  ggplot(aes(yr,ntix))+
  geom_point()+geom_line()+
  scale_x_continuous(breaks=seq(2009,2020,by=1))+
  labs(x="Year",y="Number of Distinct Tickets")

landings_plot <- tix_yr %>% 
  ggplot(aes(yr,landings/1000))+
  geom_point()+geom_line()+
  scale_x_continuous(breaks=seq(2009,2020,by=1))+
  labs(x="Year",y="Total Landings (1000 lbs)")

tix_plot
landings_plot
