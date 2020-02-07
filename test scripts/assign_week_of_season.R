# Aggregate vessel data to week and port group

setwd("~/Desktop/HAB Dist Data Analysis/Economic Impacts/1 Raw Data")

rm(list=ls())

library("dplyr")
library("lubridate")
library("tidyverse")


# Read and clean data -----------------------------------------------------


data <- data %>% 
  select(LANDING_YEAR, FINAL_LENGTH, LANDING_DATE, PACFIN_GROUP_PORT_CODE, 
         LANDING_MONTH, LANDED_WEIGHT_LBS, EXVESSEL_REVENUE, VESSEL_NUM) %>% 
  ## get calendar week for each fish ticket
  mutate(week = lubridate::week(dmy(LANDING_DATE)),
         date = dmy(data$LANDING_DATE))

# rename columns
names(data)[1] <- "year"
names(data)[4] <- "portgroup"

# Delete port groups that aren't needed
data <- data %>%
  filter(!data$portgroup %in% c("CA2", "LAA", "SDA", "WA5", "SBA")) %>% 
  droplevels()

# Assign season using YY-YY notation, filter for seasons of interest
data <- data %>%
  mutate(season = ifelse(week>45, paste0(substr(year,start=3,stop=4),"-",substr(year+1,start=3,stop=4)),
                         paste0(substr(year-1,start=3,stop=4),"-",substr(year,start=3,stop=4)))) %>%
  filter(season %in% c("10-11", "11-12", "12-13",
                       "13-14", "14-15", "15-16", "16-17"))

# Season dates - this file only includes season dates for 2007 thru 2017. needed for within_season function.
season_dates_df <- read_csv("dcrb_season_dates.csv") #use read_csv to make sure that the "open" column is read in as a character
season_dates_df$pcgroup <- as.factor(season_dates_df$pcgroup)




# Define Functions --------------------------------------------------------

# function to identify in season tickets
within_season <- function(mydates, port_groups, season_dates = season_dates_df){
  mydat <- data.frame(tdate = mydates,
                      pcgroup = port_groups) 
  mydat <- mydat %>%
    ## get the start year and end year of the fishing season
    mutate(start_yr = ifelse(month(tdate) < 8, year(tdate) - 1,year(tdate)),
           end_yr = ifelse(month(tdate) < 8, year(tdate),year(tdate) + 1)) %>%
    ## get the start date of the crab season, joining season info and then adding year.
    left_join(season_dates_df, by=c("pcgroup","start_yr" = "crab_year")) %>%
    mutate(open_date=ifelse(start_yr == 2015, paste0(end_yr, "-",open), paste0(start_yr,"-",open)),
           close_date=ifelse(pcgroup %in% c("CCA","ERA","BGA"), paste0(end_yr,"-07-15"), paste0(end_yr,"-06-30"))) %>%
    mutate(in_season =  ifelse(tdate >= ydm(open_date) & tdate <= ymd(close_date), "Y","N")) #open_date must be in year-day-month format, close_date must be in y-m-d format
  ## QC: if there are any "NAs" for in-season, this will print out those tickets **!! edited 11/26/2019 !!**
  if(any(is.na(mydat$in_season))){
    message("\nWARNING:in-season designations failed for the following fish tickets...\n")
    print(filter(mydat,is.na(in_season)))
  }
  output <- mydat[,"in_season"]
  return(output)
}


# function to number fishing weeks
fswk <- function(mydate, season, port_group, first_landing_df){
  week_list = c()
  for(i in seq(1,length(mydate))){
    tmp_date=mydate[i]
    tmp_season=season[i]
    tmp_port_group=port_group[i]
    sstart_df <- filter(first_landing_df, season==tmp_season & PACFIN_GROUP_PORT_CODE == tmp_port_group)
    if(length(sstart_df$first_landing) < 1){
      week <- NA
    } else{
      sstart <- ymd(sstart_df$first_landing)
      difference <- as.numeric(difftime(tmp_date, sstart, unit="days")) / 7
      week <- ceiling(difference)
    }
    ### if the ticket was landed the same day as sstart_df, week will be 0.
    if(week == 0){
      week <- 1
    }
    week_list[i] <- week
  }
  return(week_list)
}



# Assign Fishing Week -----------------------------------------------------


# Filter for in-season tickets
data <- data %>%
  mutate(inseason = within_season(mydates = date,
                                  port_groups = portgroup))


# Get data frame of first fish ticket landed in each port, for each season (*must run this after filtering for in-season tickets*)
first_dcrb_landing_byport <- data  %>%
  filter(inseason == "Y") %>%
  group_by(season, pcgroup) %>%
  summarise(first_landing = min(tdate))


# Assign fishing week
data <- data %>%
  mutate(fswk = fswk(mydate=date,
                     season=season, 
                     port_group=portgroup,
                     first_landing_df=first_dcrb_landing_byport))


