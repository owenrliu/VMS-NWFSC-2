# Check 2020 Q3 data
library(tidyverse)
library(here)
# import data

dat <- read_csv(here::here('data','raw','vms','May_2020.csv'))

spec(dat)
glimpse(dat)
head(dat)

# Lat/Lon are messed up again
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
