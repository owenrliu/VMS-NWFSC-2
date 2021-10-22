# Import, clean, and output OR logbook data
library(tidyverse)
# Oregon logbook data
permits <- read_csv(here::here('data','oregon','oregon_permits.csv'))
logs <- read_csv(here::here('data','oregon','oregon_logs.csv'))

# metadata for the logbooks
logs_meta <- read_csv(here::here('data','oregon','oregon_logs_metadata.csv'))

(logs_meta)

# Pull out the important variables, rename them to match our other data. 
# Also, remove any records for whom the spatial information is unreasonable (variable `SpatialFlag==T`)

glimpse(logs)

# discard and rename some variables
logs %<>% 
  filter(!SpatialFlag) %>% 
  select(FishTicket,VessID,Date,Pots,AdjLbs,BegLat,Beglon,EndLat,EndLon) %>% 
  rename(FTID=FishTicket,drvid=VessID,date=Date,n_pots=Pots,adj_lbs=AdjLbs,startlat=BegLat,startlon=Beglon,endlat=EndLat,endlon=EndLon)

# Reduce logs to only those years for which we have VMS data
logs %<>% 
  mutate(date=dmy(date)) %>%
  filter(year(date)>2008)
glimpse(logs)
