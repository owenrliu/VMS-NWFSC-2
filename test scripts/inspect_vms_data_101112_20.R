# Check OLE VMS 2020 Q4 data (now includes AIS data)
# BEF mod of original OL code
# Note from DT: Attached is the quarterly data request. In late December we started to ingest AIS data
# into our VMS database. The AIS data you will see is attached to a VMS required vessel.
# So any vessel that has AIS will position with both. Any status other than AIS is a VMS
# position. I added a column called Status so you can distinguish between VMS and AIS
# positions (if you care) I had to split December into two files
library(tidyverse)
library(here)
library(magrittr)
library(gtools)

# import data using here
# dat <- read_csv(here::here('data','raw','vms','10_2020.csv'))

# import data using conventional means for October and November
setwd("~/Documents/Projects/Ecosystem Science/Whale Entanglement/VMS analyses/CONFIDENTIAL/RAW data")
dat10 <- read_csv('10_2020.csv')
spec(dat10)
glimpse(dat10)
head(dat10)

dat11 <- read_csv('11_2020.csv')
spec(dat11)
glimpse(dat11)
head(dat11)

# special import data using conventional means for December 2020
dat12A <- read_csv('12_2020_A.csv')
# 12_2020_B.csv does not have headers
dat12B <- read_csv(('12_2020_B.csv'), col_names=FALSE)
# tried bind_rows in dplyr to merge dat12A and dat12B while using the column names from dat12A, but just adds 12 new columns
# dat12 <- bind_rows(dat12A, dat12B)

spec(dat12A)
glimpse(dat12A)
head(dat12A)

spec(dat12B)
glimpse(dat12B)
head(dat12B)

spec(dat12)
glimpse(dat12)
head(dat12)

# check to see if header names are the same between October and November
names(dat10) == names(dat11)

# check to see if header names are the same between October and December A
names(dat10) == names(dat12)

# For OLE files prior to April of 2020, Lat/Lon are messed up and need to be processed in order to generate mappable geocoords
# convert character lat/lon to decimal degrees. Specify whether lats and lons are positive (N or E) or negative (S or W)
make_dd <- function(string,type="positive"){
  d <- as.numeric(string[1])
  ms <- as.numeric(paste0(string[2],'.',string[3]))/60
  if(type=="positive"){
   out <-  d + ms
  } else {
      out <- d-ms
  }
  out
}


# function takes a string, removes weird unknown characters, then applies the make_dd() function and outputs the new vector
fix_degrees <- function(string,type="positive"){
  str_replace_all(string,"[^[:alnum:][:blank:]?&/\\-]"," ") %>% 
    str_split(" ") %>% 
    purrr::map_dbl(~make_dd(.,type=type))
}

dat %<>%
  mutate(Lat=fix_degrees(Lat,type="positive"),Lon=fix_degrees(Lon,type="negative"))

glimpse(dat)
