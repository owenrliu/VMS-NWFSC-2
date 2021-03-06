# 040620

# inspect new vms data as it rolls in

library(tidyverse)
library(here)
library(magrittr)

#Jameal's files
time_period <- "jan2020_mar2020"

new_vms_files_df <- list.files(path = paste0("~/Documents/RAIMBOW/Raw Data/Raw VMS data as sent by OLE/",time_period), full.names=TRUE, pattern = ".csv") %>%
  map_dfr(read_csv)

glimpse(new_vms_files_df)

#Owen's files (find out what Dennis named the files and put those names in a chr vector)
(fls <- paste0(c('Jan','Feb','March'),"_2020.csv"))

new_vms <- here::here('data','raw','vms') %>% paste0("/",fls) %>% map_dfr(read_csv)
# look at them
head(new_vms)
# fix Lat/Long columns
head(new_vms$Lat)

# function makes a string of 3 characters (degrees, minutes, decimal minutes) into one decimal degrees double
make_dd <- function(string){as.numeric(string[1])+as.numeric(paste0(string[2],'.',string[3]))/60}

# function takes a string, removes weird unknown characters, then applies the make_dd() function and outputs the new vector
fix_degrees <- function(string){
  str_replace_all(string,"[^[:alnum:][:blank:]?&/\\-]"," ") %>% 
    str_split(" ") %>% 
    purrr::map_dbl(~make_dd(.))
}

new_vms %<>%
  mutate(Lat=fix_degrees(Lat),Lon=fix_degrees(Lon))

glimpse(new_vms)
